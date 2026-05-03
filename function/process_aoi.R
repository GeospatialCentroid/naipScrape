process_aoi <- function(
  aoi_id,
  target_years,
  local_dir,
  db_path,
  g100_grid,
  batch_id,
  network_dir,
  p = NULL
) {
  # --- 1. JITTER FOR RATE LIMITING ---
  # Force the worker to sleep for a random time between 1 and 15 seconds.
  # This perfectly staggers the Planetary Computer API hits.
  Sys.sleep(runif(1, min = 1, max = 15))

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

  # 3. Database Connection & Concurrency
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000;")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")

  # --- 4. SMART RETRY LOGIC ---
  check <- DBI::dbGetQuery(
    con,
    "SELECT * FROM aoi_tracker WHERE aoi_id = ?",
    params = list(aoi_id)
  )

  # Default assumption: process all years, no prior statuses
  years_to_process <- target_years
  year_statuses <- list()

  if (nrow(check) > 0) {
    # Check which specific years succeeded previously
    y1_ok <- !is.na(check$year_1) &&
      (check$year_1 == "Success" || grepl("Skipped", check$year_1))
    y2_ok <- !is.na(check$year_2) &&
      (check$year_2 == "Success" || grepl("Skipped", check$year_2))
    y3_ok <- !is.na(check$year_3) &&
      (check$year_3 == "Success" || grepl("Skipped", check$year_3))

    # If all three are good, we can completely skip this AOI
    if (y1_ok && y2_ok && y3_ok) {
      if (!is.null(p)) {
        p(step = 1, message = sprintf("Skipped (All Complete) %s", aoi_id))
      }
      return("Skipped - All Years Complete")
    }

    # Otherwise, preserve the good statuses and filter the years we actually need to process
    year_statuses[[target_years[1]]] <- check$year_1
    year_statuses[[target_years[2]]] <- check$year_2
    year_statuses[[target_years[3]]] <- check$year_3

    years_to_process <- target_years[!c(y1_ok, y2_ok, y3_ok)]
  }

  # 5. Create Flat Output Directory
  aoi_folder <- file.path(local_dir, aoi_id)
  dir.create(aoi_folder, showWarnings = FALSE)

  current_step <- "Fetching AOI Geometry"
  aoi <- tryCatch(
    getAOI(grid100 = g100_grid, id = aoi_id),
    error = function(e) return(NULL)
  )

  if (is.null(aoi)) {
    # Include retry logic for Database Lock on the missing geometry insert
    write_success <- FALSE
    retries <- 0
    while (!write_success && retries < 10) {
      tryCatch(
        {
          DBI::dbExecute(
            con,
            "INSERT OR REPLACE INTO aoi_tracker (aoi_id, batch_id, status) VALUES (?, ?, 'Failed: Missing AOI Geometry')",
            params = list(aoi_id, batch_id)
          )
          write_success <- TRUE
        },
        error = function(e) {
          if (grepl("locked", e$message, ignore.case = TRUE)) {
            Sys.sleep(runif(1, min = 1, max = 3))
            retries <<- retries + 1
          } else {
            stop(e)
          }
        }
      )
    }

    if (!is.null(p)) {
      p(step = 1, message = sprintf("Failed Geom %s", aoi_id))
    }
    return("Failed")
  }

  # 6. Process ONLY the missing/failed years
  for (target_year in years_to_process) {
    tryCatch(
      {
        # --- NEW PRE-CHECK (NO API NEEDED) ---
        current_step <- "Pre-checking if 2km export already exists locally or on network"

        fallback_year <- as.character(as.numeric(target_year) - 1)

        loc_target <- file.path(
          aoi_folder,
          paste0("naip_2km_", aoi_id, "_", target_year, ".tif")
        )
        loc_fallback <- file.path(
          aoi_folder,
          paste0("naip_2km_", aoi_id, "_", fallback_year, ".tif")
        )

        net_target <- file.path(
          network_dir,
          paste0("naip_batch_", batch_id),
          aoi_id,
          paste0("naip_2km_", aoi_id, "_", target_year, ".tif")
        )
        net_fallback <- file.path(
          network_dir,
          paste0("naip_batch_", batch_id),
          aoi_id,
          paste0("naip_2km_", aoi_id, "_", fallback_year, ".tif")
        )

        if (
          file.exists(loc_target) ||
            file.exists(loc_fallback) ||
            file.exists(net_target) ||
            file.exists(net_fallback)
        ) {
          year_statuses[[target_year]] <- "Skipped - Exists"
          next
        }

        # --- IF NOT SKIPPED, PROCEED TO API QUERY ---
        current_step <- "STAC API Query for availability"
        years_available <- getNAIPYear(aoi)
        actual_year <- target_year

        if (!target_year %in% years_available) {
          actual_year <- fallback_year
        }

        if (!actual_year %in% years_available) {
          stop(paste(
            "Neither",
            target_year,
            "nor fallback",
            actual_year,
            "exist in Planetary Computer."
          ))
        }

        # --- PAUSE & RETRY LOGIC ---
        current_step <- paste("Downloading VSI tiles for", actual_year)

        max_retries <- 3
        retry_count <- 0
        download_success <- FALSE

        while (!download_success && retry_count < max_retries) {
          tryCatch(
            {
              downloadNAIP_vsi(
                aoi = aoi,
                year = actual_year,
                exportFolder = worker_temp
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
          buffer_only = TRUE
        )

        year_statuses[[target_year]] <- "Success"

        file.remove(naip_files)
        terra::tmpFiles(remove = TRUE)
        gc(reset = TRUE, full = TRUE)
      },
      error = function(e) {
        # FIX 1: Use <<- to push the error message to the parent environment
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

  # 7. Log Completion with the new final_status + Database Lock retry logic
  write_success <- FALSE
  retries <- 0
  while (!write_success && retries < 10) {
    tryCatch(
      {
        DBI::dbExecute(
          con,
          "INSERT OR REPLACE INTO aoi_tracker (aoi_id, batch_id, year_1, year_2, year_3, status) VALUES (?, ?, ?, ?, ?, ?)",
          params = list(aoi_id, batch_id, s1, s2, s3, final_status)
        )
        write_success <- TRUE
      },
      error = function(e) {
        if (grepl("locked", e$message, ignore.case = TRUE)) {
          Sys.sleep(runif(1, min = 1, max = 3))
          retries <<- retries + 1
        } else {
          stop(e)
        }
      }
    )
  }

  if (!is.null(p)) {
    p(step = 1, message = sprintf("Finished %s", aoi_id))
  }

  return(final_status)
}
