# primary workflow function for the bluck download process

process_aoi <- function(aoi_id, target_years, local_dir, db_path, g100_grid) {
  worker_temp <- file.path(tempdir(), paste0("terra_worker_", Sys.getpid()))
  dir.create(worker_temp, showWarnings = FALSE)
  terra::terraOptions(tempdir = worker_temp)

  on.exit(
    {
      unlink(worker_temp, recursive = TRUE)
    },
    add = TRUE
  )

  # 2. Database Connection & Concurrency
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, synchronous = NULL)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA busy_timeout = 10000;")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")

  # 3. Check Status
  check <- DBI::dbGetQuery(
    con,
    sprintf("SELECT status FROM aoi_tracker WHERE aoi_id = '%s'", aoi_id)
  )
  if (nrow(check) > 0 && check$status == "Complete") {
    return("Skipped")
  }

  # 4. Create Flat Output Directory
  aoi_folder <- file.path(local_dir, aoi_id)
  dir.create(aoi_folder, showWarnings = FALSE)

  # Initialize the breadcrumb variable outside the tryCatch
  current_step <- "Fetching AOI Geometry"

  # Safe fetch in case the grid doesn't intersect
  aoi <- tryCatch(
    getAOI(grid100 = g100_grid, id = aoi_id),
    error = function(e) return(NULL)
  )

  if (is.null(aoi)) {
    DBI::dbExecute(
      con,
      sprintf(
        "INSERT OR REPLACE INTO aoi_tracker (aoi_id, status) VALUES ('%s', 'Failed: Missing AOI Geometry')",
        aoi_id
      )
    )
    return("Failed")
  }

  year_statuses <- list()

  # 5. Process Years
  for (target_year in target_years) {
    tryCatch(
      {
        current_step <- "STAC API Query for availability"
        years_available <- getNAIPYear(aoi)
        actual_year <- target_year

        if (!target_year %in% years_available) {
          actual_year <- as.character(as.numeric(target_year) - 1)
        }

        # Hard stop if neither the target year nor the fallback year exists
        if (!actual_year %in% years_available) {
          stop(paste(
            "Neither",
            target_year,
            "nor fallback",
            actual_year,
            "exist in Planetary Computer."
          ))
        }

        current_step <- "Checking if 2km export already exists"
        export_check <- file.path(
          aoi_folder,
          paste0("naip_2km_", aoi_id, "_", actual_year, ".tif")
        )
        if (file.exists(export_check)) {
          year_statuses[[target_year]] <- "Skipped - Exists"
          next
        }

        current_step <- paste("Downloading VSI tiles for", actual_year)
        downloadNAIP_vsi(
          aoi = aoi,
          year = actual_year,
          exportFolder = worker_temp
        )

        current_step <- "Regex gathering downloaded raw tiles"
        naip_string <- paste0("^naip_", actual_year, ".*", aoi_id, ".*\\.tif$")
        naip_files <- list.files(
          path = worker_temp,
          pattern = naip_string,
          full.names = TRUE
        )

        if (length(naip_files) == 0) {
          stop("Download succeeded, but no files matched regex.")
        }

        current_step <- "Merging and exporting 2km NAIP"
        # explicitly call buffer_only = TRUE
        mergeAndExportNAIP(
          files = naip_files,
          out_path = aoi_folder,
          aoi = aoi,
          year = actual_year,
          buffer_only = TRUE
        )

        year_statuses[[target_year]] <- "Success"

        # Clear raster memory and temp files
        file.remove(naip_files)
        terra::tmpFiles(remove = TRUE)
        gc(reset = TRUE, full = TRUE)
      },
      error = function(e) {
        # Inject the breadcrumb string into the database log
        year_statuses[[target_year]] <- paste0(
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

  # 6. Log Completion
  DBI::dbExecute(
    con,
    sprintf(
      "INSERT OR REPLACE INTO aoi_tracker (aoi_id, year_1, year_2, year_3, status) 
     VALUES ('%s', '%s', '%s', '%s', 'Complete')",
      aoi_id,
      if (is.null(year_statuses[[target_years[1]]])) {
        "NULL"
      } else {
        year_statuses[[target_years[1]]]
      },
      if (is.null(year_statuses[[target_years[2]]])) {
        "NULL"
      } else {
        year_statuses[[target_years[2]]]
      },
      if (is.null(year_statuses[[target_years[3]]])) {
        "NULL"
      } else {
        year_statuses[[target_years[3]]]
      }
    )
  )
  return("Complete")
}
