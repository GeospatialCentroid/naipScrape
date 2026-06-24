process_aoi <- function(
    aoi_id,
    target_years,
    local_dir,
    g100_grid,
    batch_id,
    network_dir,
    buffer_m = 250,
    p = NULL
) {
  # --- 1. JITTER FOR RATE LIMITING ---
  # Force the worker to sleep for a random time between 0.5 and 3.0 seconds.
  # This staggers the Planetary Computer API hits to avoid rate limits.
  Sys.sleep(runif(1, min = 0.5, max = 3.0))
  
  # 2. Isolate Terra Temp Directories
  worker_temp <- file.path(tempdir(), paste0("terra_worker_", Sys.getpid()))
  dir.create(worker_temp, showWarnings = FALSE)
  terra::terraOptions(tempdir = worker_temp)
  
  on.exit(
    {
      unlink(worker_temp, recursive = TRUE)
    },
    add = TRUE
  )
  
  # 3. Create Flat Output Directory & Status Path
  aoi_folder <- file.path(local_dir, aoi_id)
  status_file <- file.path(aoi_folder, "status.json")
  
  # --- 4. SMART RETRY LOGIC (JSON File Based) ---
  years_to_process <- target_years
  year_statuses <- list()
  year_metas <- list()
  
  if (file.exists(status_file)) {
    # Read and parse status JSON
    check <- tryCatch({
      jsonlite::fromJSON(status_file)
    }, error = function(e) NULL)
    
    if (!is.null(check)) {
      y1_ok <- !is.null(check$year_1) && (check$year_1 == "Success" || grepl("Skipped", check$year_1))
      y2_ok <- !is.null(check$year_2) && (check$year_2 == "Success" || grepl("Skipped", check$year_2))
      y3_ok <- !is.null(check$year_3) && (check$year_3 == "Success" || grepl("Skipped", check$year_3))
      
      # Retain previously loaded metadata if present
      if (!is.null(check$year_1_meta)) year_metas[[target_years[1]]] <- check$year_1_meta
      if (!is.null(check$year_2_meta)) year_metas[[target_years[2]]] <- check$year_2_meta
      if (!is.null(check$year_3_meta)) year_metas[[target_years[3]]] <- check$year_3_meta
      
      # If all three are good, we can completely skip this AOI
      if (y1_ok && y2_ok && y3_ok) {
        if (!is.null(p)) {
          p(step = 1, message = sprintf("Skipped (All Complete) %s", aoi_id))
        }
        return(list(
          aoi_id = aoi_id,
          batch_id = batch_id,
          year_1 = check$year_1,
          year_2 = check$year_2,
          year_3 = check$year_3,
          status = "Complete",
          year_1_meta = check$year_1_meta,
          year_2_meta = check$year_2_meta,
          year_3_meta = check$year_3_meta
        ))
      }
      
      # Otherwise, preserve the good statuses and filter the years we actually need to process
      year_statuses[[target_years[1]]] <- check$year_1
      year_statuses[[target_years[2]]] <- check$year_2
      year_statuses[[target_years[3]]] <- check$year_3
      
      years_to_process <- target_years[!c(y1_ok, y2_ok, y3_ok)]
    }
  }
  
  # Ensure AOI folder is created
  dir.create(aoi_folder, showWarnings = FALSE, recursive = TRUE)
  
  # --- PROTECTED API QUERIES ---
  current_step <- "Fetching AOI Geometry"
  aoi <- tryCatch({
    R.utils::withTimeout({
      getAOI(grid100 = g100_grid, id = aoi_id)
    }, timeout = 120, onTimeout = "error")
  }, error = function(e) return(NULL))
  
  if (is.null(aoi)) {
    if (!is.null(p)) p(step = 1, message = sprintf("Failed Geom %s", aoi_id))
    
    res <- list(
      aoi_id = aoi_id,
      batch_id = batch_id,
      year_1 = "Failed",
      year_2 = "Failed",
      year_3 = "Failed",
      status = "Failed: Missing/Timeout AOI Geometry",
      year_1_meta = year_metas[[target_years[1]]],
      year_2_meta = year_metas[[target_years[2]]],
      year_3_meta = year_metas[[target_years[3]]]
    )
    writeLines(jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE), status_file)
    return(res)
  }
  
  # Gather all available years with protection
  current_step <- "Fetching NAIP Metadata/Years"
  years_available <- tryCatch({
    R.utils::withTimeout({
      getNAIPYear(aoi)
    }, timeout = 120, onTimeout = "error")
  }, error = function(e) return(NULL))
  
  if (is.null(years_available)) {
    if (!is.null(p)) p(step = 1, message = sprintf("Failed Metadata %s", aoi_id))
    
    res <- list(
      aoi_id = aoi_id,
      batch_id = batch_id,
      year_1 = "Failed",
      year_2 = "Failed",
      year_3 = "Failed",
      status = "Failed: API Timeout on Metadata",
      year_1_meta = year_metas[[target_years[1]]],
      year_2_meta = year_metas[[target_years[2]]],
      year_3_meta = year_metas[[target_years[3]]]
    )
    writeLines(jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE), status_file)
    return(res)
  }
  # 6. Process ONLY the missing/failed years
  for (target_year in years_to_process) {
    tryCatch(
      {
        # --- NEW TIMEOUT WRAPPER: 4 Minutes (240 seconds) per year ---
        R.utils::withTimeout({
          
          # --- IF NOT SKIPPED, PROCEED TO API QUERY ---
          current_step <- "STAC API Query for availability"
          
          # Define the exact testing hierarchy
          target_num <- as.numeric(target_year)
          preferred_years <- as.character(c(
            target_num, # 2. Test initial year (e.g., 2012)
            target_num - 1, # 3. Move one year down (e.g., 2011)
            target_num - 2, # 4. Move two years down (e.g., 2010)
            target_num + 1 # 5. Move one year up (e.g., 2013)
          ))
          
          actual_year <- NULL
          
          # Check each year in our preferred order
          for (test_year in preferred_years) {
            if (test_year %in% years_available) {
              actual_year <- test_year
              break # Match found! Exit this search loop immediately.
            }
          }
          
          # 6. Cancel attempt and log to SQL if no imagery was found
          if (is.null(actual_year)) {
            stop(sprintf(
              "Target %s not found. Fallbacks (%s, %s, %s) also completely missing from Planetary Computer.",
              target_year,
              preferred_years[2],
              preferred_years[3],
              preferred_years[4]
            ))
          }
          
          # --- PAUSE & RETRY LOGIC ---
          current_step <- paste("Downloading VSI tiles for", actual_year)
          
          max_retries <- 3
          retry_count <- 0
          download_success <- FALSE
          
          tile_meta <- NULL
          while (!download_success && retry_count < max_retries) {
            tryCatch(
              {
                tile_meta <- downloadNAIP_vsi(
                  aoi = aoi,
                  year = actual_year,
                  exportFolder = worker_temp,
                  buffer_m = buffer_m
                )
                download_success <- TRUE # If it gets here, it worked!
              },
              error = function(api_err) {
                retry_count <<- retry_count + 1
                if (retry_count < max_retries) {
                  # Pause for 15 to 45 seconds to let the Planetary Computer API cool down
                  Sys.sleep(runif(1, min = 15, max = 45))
                } else {
                  # If we failed 3 times, pass the error up to the main tryCatch to fail the year
                  stop(paste("API Timeout after 3 attempts:", api_err$message))
                }
              }
            )
          }
          
          # --- STRICT REGEX FIX ---
          current_step <- "Regex gathering downloaded raw tiles"
          
          # Uses explicit underscores and boundaries to prevent "1" from matching "12"
          naip_string <- paste0(
            "^naip_",
            actual_year,
            "_id_",
            aoi_id,
            "_[0-9]+\\.tif$"
          )
          
          naip_files <- list.files(
            path = worker_temp,
            pattern = naip_string,
            full.names = TRUE
          )
          
          if (length(naip_files) == 0) {
            stop("Download succeeded, but no files matched regex.")
          }
          
          current_step <- "Merging and exporting 2km NAIP"
          mergeAndExportNAIP(
            files = naip_files,
            out_path = aoi_folder,
            aoi = aoi,
            year = actual_year,
            buffer_m = buffer_m,
            buffer_only = TRUE
          )
          
          year_statuses[[target_year]] <- "Success"
          
          # Store exact fallback year and collection metadata
          year_metas[[target_year]] <- list(
            actual_year   = actual_year,
            capture_dates = tile_meta$collection_date,
            item_ids      = tile_meta$item_id,
            naip_states   = tile_meta$naip_state
          )
          
          file.remove(naip_files)
          terra::tmpFiles(remove = TRUE)
          gc(reset = TRUE, full = TRUE)
          
        }, timeout = 240, onTimeout = "error")
        # --- END TIMEOUT WRAPPER ---
      },
      TimeoutException = function(ex) {
        # Catch the 4-minute timeout specifically
        year_statuses[[target_year]] <<- paste0(
          "Failed at [",
          current_step,
          "]: Exceeded 4-minute time limit."
        )
        terra::tmpFiles(remove = TRUE)
        gc(reset = TRUE, full = TRUE)
      },
      error = function(e) {
        # Catch all standard errors
        year_statuses[[target_year]] <<- paste0(
          "Failed at [",
          current_step,
          "]: ",
          e$message
        )
        terra::tmpFiles(remove = TRUE)
        gc(reset = TRUE, full = TRUE)
      }
    )
  }
  
  # Safely extract the statuses, defaulting to "Failed" if they somehow remained NULL
  s1 <- if (is.null(year_statuses[[target_years[1]]])) {
    "Failed"
  } else {
    year_statuses[[target_years[1]]]
  }
  s2 <- if (is.null(year_statuses[[target_years[2]]])) {
    "Failed"
  } else {
    year_statuses[[target_years[2]]]
  }
  s3 <- if (is.null(year_statuses[[target_years[3]]])) {
    "Failed"
  } else {
    year_statuses[[target_years[3]]]
  }
  
  # Check if all three years are either Success or safely skipped
  is_complete <- all(
    c(s1, s2, s3) %in%
      c("Success", "Skipped - Exists", "Skipped - All Years Complete")
  )
  final_status <- ifelse(is_complete, "Complete", "Partial")
  
  if (!is.null(p)) {
    p(step = 1, message = sprintf("Finished %s", aoi_id))
  }
  
  res <- list(
    aoi_id = aoi_id,
    batch_id = batch_id,
    year_1 = s1,
    year_2 = s2,
    year_3 = s3,
    status = final_status,
    year_1_meta = year_metas[[target_years[1]]],
    year_2_meta = year_metas[[target_years[2]]],
    year_3_meta = year_metas[[target_years[3]]]
  )
  writeLines(jsonlite::toJSON(res, auto_unbox = TRUE, pretty = TRUE), status_file)
  return(res)
}
