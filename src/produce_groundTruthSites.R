# This is a generalized method for downloading material from the planetary computer

pacman::p_load(dplyr, sf, terra, tidyr, tictoc, foreach, doParallel, doSNOW, snic)
# once exported can zip via terminal with the following command for d in */; do zip -r "${d%/}.zip" "$d"; done

# testing
library(tmap)
tmap_mode(mode = "view")

# source files
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# establish grid features
g100 <- sf::st_read("data/grid100km_aea.gpkg")

lrr_symbol <- "G"
# random sampling with an LRR
mlra <- sf::st_read(dsn = "data/mlra/lower48MLRA.gpkg") |>
  dplyr::filter(LRRSYM == lrr_symbol)

random <- FALSE

# establish methods for random selection within an LLR or selection from within an establish set of 1km areas else it should read in a specific set of site ids. 
set.seed(12486)

if(random){
  # assign sample number : number of sites 
  sites <- 50
  
  # generate random spatial samples
  points <- sf::st_sample(x = mlra, size = sites, by_polygon = TRUE)
  coords_df <- as.data.frame(st_coordinates(points))
  
  # Builds table with 54 features (18 locations * 3 years)
  table <- build_index_table(
    years = rep(c("2010", "2012", "2014", "2016","2018", "2020", "2022", "2024"), sites),
    lat = coords_df$Y,
    lon = coords_df$X
  )
  
  
}else{
  # new table for the update datasets or specific location runs 
  table <-  readr::read_csv("data/groundTruthSites/llr_G_forestNeyman200_sites.csv")
  # need to assing a random year - added method back to the naip scape process 
  years <- c(2012, 2016, 2020)
  table <- table %>%
    mutate(year = sample(years, size = n(), replace = TRUE))
}



# Setup directories
aoi_dir <- file.path("data/aoiExports")
naip_dir <- file.path("data/naipExports")
snic_dir <- file.path("data/snicExports")
lidar_dir <- file.path("data/lidarExports")
temp_dir <- file.path("data/download")
export_dir <- file.path("data/exportData")

# --- EXECUTION TOGGLES ---
run_parallel <- FALSE # Set to TRUE for production, FALSE for sequential debugging
run_snic <- TRUE     # Set to TRUE to generate SNIC location data, FALSE to skip
# ------------------------
# set buffer dist 
buff_dist_m <- 250 # produces a 1.5km image 

tic("Total Script Runtime")

if (run_parallel) {
  # ==========================================
  # PARALLEL EXECUTION
  # ==========================================
  num_cores <- max(1, parallel::detectCores() - 16)
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  cat("Starting cluster with", num_cores, "cores...\n")
  results <- foreach(
    task_row = 1:nrow(table),
    .packages = c("terra", "sf", "tictoc", "stringr", "purrr", "rstac", "dplyr", "tidyr", "snic"),
    .export = c("getAOI", "getNAIPYear", "downloadNAIP_vsi", "mergeAndExportNAIP", 
                "generate_scaled_seeds", "process_segmentations", "copyToExport",
                "g100", "temp_dir", "naip_dir", "snic_dir", "run_snic", "buff_dist_m", 
                "table", "export_dir", "aoi_dir", "lidar_dir", 
                "readAndName"), # <--- ADD THIS HERE
    .errorhandling = 'pass'
  ) %dopar% {
    lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)
    
    target_year <- as.character(table$year[task_row]) 
    
    # --- 1. DYNAMIC AOI FETCHING (Synced from Sequential) ---
    if("lon" %in% names(table)){
      pt_lon <- table$lon[task_row]
      pt_lat <- table$lat[task_row]
      current_point <- c(pt_lon, pt_lat)
      aoi <- getAOI(grid100 = g100, point = current_point)
    } else {
      aoi <- getAOI(grid100 = g100, id = table$id[task_row])
    }
    
    id <- aoi$id
    
    if (dir.exists(paste0("data/exportData/aoi_", id, "_", target_year))) {
      return(list(id = id, year = target_year, status = "Skipped - Already Exists", time = 0))
    }
    
    # --- 2. API YEAR CHECK ---
    years_available <- tryCatch({
      getNAIPYear(aoi)
    }, error = function(cond) {
      return(NULL) 
    })
    
    if (is.null(years_available)) {
      return(list(id = id, year = target_year, status = "Failed - STAC API Error", time = 0))
    }
    
    # --- 3. ROBUST YEAR HANDLING ---
    target_num <- as.numeric(target_year)
    preferred_years <- as.character(c(
      target_num,      
      target_num - 1,  
      target_num - 2,  
      target_num + 1   
    ))
    
    actual_year <- NULL
    for (test_year in preferred_years) {
      if (test_year %in% years_available) {
        actual_year <- test_year
        break 
      }
    }
    
    if (is.null(actual_year)) {
      return(list(id = id, year = target_year, status = "Failed - No imagery found within fallback range", time = 0))
    }
    # Check if the export for the actual fallback year already exists
    if (dir.exists(paste0(export_dir, "/aoi_", id, "_", actual_year))) {
      return(list(id = id, year = target_year, status = "Skipped - Already Exists (Fallback Year)", time = 0))
    }
    # --- 4. EXECUTION ---
    tic()
    process_status <- tryCatch({
      downloadNAIP_vsi(aoi = aoi, year = actual_year, buffer_m = buff_dist_m, exportFolder = temp_dir)
      
      naip_string <- paste0("^naip_", actual_year, "_id_", id, "_[0-9]+\\.tif$")
      naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
      
      if (length(naip_files) == 0) stop("No files matched the regex pattern on disk.")
      
      # Synced argument: buffer_only = FALSE
      mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi, year = actual_year, buffer_only = FALSE)
      
      if (run_snic) {
        # Synced Regex: 1km_
        r1_path <- list.files(path = naip_dir, pattern = paste0("1km_.*", id, ".*\\.tif$"), full.names = TRUE)      
        r1  <- terra::rast(r1_path)
        seeds <- generate_scaled_seeds(r = r1)
        # Synced argument: aoi = aoi
        process_segmentations(r = r1, seed_list = seeds, output_dir = snic_dir, file_id = id, aoi = aoi, year = actual_year)
      }
      
      copyToExport(id = id, year = actual_year)
      "Success"
    }, error = function(cond) {
      return(paste("Failed:", conditionMessage(cond)))
    })
    
    t_out <- toc(quiet = TRUE)
    return(list(id = id, year = target_year, status = process_status, time = t_out$toc - t_out$tic))
  }
  stopCluster(cl)
  
} else {
  # ==========================================
  # SEQUENTIAL EXECUTION (WITH DEBUG TRACKING)
  # ==========================================
  cat("Running sequentially for debugging...\n")
  
  results <- foreach(
    task_row = 1:nrow(table),
    .packages = c("terra", "sf", "tictoc", "stringr", "purrr"),
    .errorhandling = 'pass'
  ) %do% {
    
    target_year <- as.character(table$year[task_row]) 
    
    # Extract coordinates to pass to getAOI
    if("lon" %in% names(table)){
      pt_lon <- table$lon[task_row]
      pt_lat <- table$lat[task_row]
      current_point <- c(pt_lon, pt_lat)
      
      cat(sprintf("\n--- Starting Task %d of %d ---\n", task_row, nrow(table)))
      cat("1. Fetching AOI using point feature...\n")
      
      # Pass the coordinate vector to getAOI
      aoi <- getAOI(grid100 = g100, point = current_point)
    }else{
      
      cat(sprintf("\n--- Starting Task %d of %d ---\n", task_row, nrow(table)))
      cat("1. Fetching AOI using id feature...\n")
      
      # Pass the coordinate vector to getAOI
      aoi <- getAOI(grid100 = g100, id = table$id[task_row])
    }
    
    id <- aoi$id
    
    cat("   -> AOI ID:", id, "| Target Year:", target_year, "\n")
    
    if (dir.exists(paste0("data/exportData/aoi_", id, "_", target_year))) {
      cat("   -> Status: Skipped (Directory already exists)\n")
      return(list(id = id, year = target_year, status = "Skipped - Already Exists", time = 0))
    }
    
    # --- 2. API YEAR CHECK ---
    cat("2. Checking STAC API for available years...\n")
    years_available <- tryCatch({
      getNAIPYear(aoi)
    }, error = function(cond) {
      return(NULL) 
    })
    
    if (is.null(years_available)) {
      cat("   -> Status: Failed (STAC API Error)\n")
      return(list(id = id, year = target_year, status = "Failed - STAC API Error", time = 0))
    }
    
    # --- 3. ROBUST YEAR HANDLING ---
    target_num <- as.numeric(target_year)
    preferred_years <- as.character(c(
      target_num,      # Initial year
      target_num - 1,  # Move one year down
      target_num - 2,  # Move two years down
      target_num + 1   # Move one year up
    ))
    
    actual_year <- NULL
    for (test_year in preferred_years) {
      if (test_year %in% years_available) {
        actual_year <- test_year
        break 
      }
    }
    
    if (is.null(actual_year)) {
      cat("   -> Status: Failed (No imagery within fallback range)\n")
      return(list(id = id, year = target_year, status = "Failed - No imagery found within fallback range", time = 0))
    }
    
    cat("   -> Actual Year assigned:", actual_year, "\n")
    
    if (dir.exists(paste0(export_dir, "/aoi_", id, "_", actual_year))) {
      cat("   -> Status: Skipped (Directory already exists for actual year)\n")
      return(list(id = id, year = target_year, status = "Skipped - Already Exists (Fallback Year)", time = 0))
    }
    
    tic()
    process_status <- tryCatch({
      
      cat("4. Requesting Planetary Computer Download...\n")
      downloadNAIP_vsi(aoi = aoi, year = actual_year,buffer_m = buff_dist_m,  exportFolder = temp_dir)
      
      cat("5. Locating Downloaded Files...\n")
      naip_string <- paste0("^naip_", actual_year, "_id_", id, "_[0-9]+\\.tif$")
      naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
      
      if (length(naip_files) == 0) {
        stop("Download function passed, but no files matched the regex pattern on disk.")
      }
      cat("   -> Found", length(naip_files), "files to merge.\n")
      
      cat("6. Merging NAIP Imagery...\n")
      mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi,year = actual_year,buffer_only = FALSE)
      
      if (run_snic) {
        cat("7. Starting SNIC Processing...\n")
        r1_path <- list.files(path = naip_dir, pattern = paste0("1km_.*", id, ".*\\.tif$"), full.names = TRUE)      
        r1  <- terra::rast(r1_path)
        seeds <- generate_scaled_seeds(r = r1)
        process_segmentations(r = r1, seed_list = seeds, output_dir = snic_dir, file_id = id, aoi = aoi, year = actual_year)
      } else {
        cat("7. Skipping SNIC Processing...\n")
      }
      
      cat("8. Exporting Final Data...\n")
      copyToExport(id = id, year = actual_year)
      
      cat("   -> Task completed successfully.\n")
      "Success"
      
    }, error = function(cond) {
      cat("   -> ERROR Encountered:", conditionMessage(cond), "\n")
      return(paste("Failed:", conditionMessage(cond)))
    })
    
    t_out <- toc(quiet = TRUE)
    return(list(id = id, year = target_year, status = process_status, time = t_out$toc - t_out$tic))
  }
}

total_runtime <- toc()

# ---------------------------------------------------------
# REPORTING (Works for both Sequential and Parallel)
# ---------------------------------------------------------
valid_results <- Filter(function(x) is.list(x) && !is.null(x$status), results)

# Use Filter instead of bracket subsetting with sapply
successful_runs <- Filter(function(x) x$status == "Success", valid_results)
failed_runs     <- Filter(function(x) grepl("Failed", x$status), valid_results)
skipped_runs    <- Filter(function(x) grepl("Skipped", x$status), valid_results)

success_times <- sapply(successful_runs, function(x) x$time)

cat("\n==========================================\n")
cat("PROCESSING SUMMARY ( Parallel:", run_parallel, ")\n")
cat("Total Tasks Attempted: ", nrow(table), "\n")
cat("Successful: ", length(successful_runs), "\n")
cat("Failed: ", length(failed_runs), "\n")
cat("Skipped: ", length(skipped_runs), "\n")
cat("------------------------------------------\n")
if (length(success_times) > 0) {
  cat("Avg time per successful loop: ", round(mean(success_times), 2), "seconds\n")
}
cat("Total script runtime: ", round(total_runtime$toc - total_runtime$tic, 2), "seconds\n")
cat("==========================================\n")

if (length(failed_runs) > 0) {
  cat("\nError Log:\n")
  for (fail in failed_runs) {
    cat("ID:", fail$id, "| Year:", fail$year, "| Error:", fail$status, "\n")
  }
}




cleanup_mismatched_aois <- function(target_table, export_directory, dry_run = TRUE) {
  # Get a list of all directories in the target folder
  all_folders <- list.dirs(export_directory, full.names = TRUE, recursive = FALSE)
  
  if (length(all_folders) == 0) {
    message("No folders found in the export directory.")
    return(invisible())
  }
  
  # Filter for folders that match the "aoi_" prefix
  folder_basenames <- basename(all_folders)
  valid_folders <- all_folders[grepl("^aoi_", folder_basenames)]
  valid_basenames <- basename(valid_folders)
  
  folders_to_delete <- c()
  
  for (i in seq_along(valid_folders)) {
    folder_path <- valid_folders[i]
    folder_name <- valid_basenames[i]
    
    # Strip the "aoi_" prefix to isolate {id}_{year}
    name_no_prefix <- sub("^aoi_", "", folder_name)
    
    # Extract the ID and Year using regex. 
    # This looks for everything up to the last underscore for the ID, 
    # and exactly 4 digits at the end for the year.
    matches <- regmatches(name_no_prefix, regexec("^(.*)_([0-9]{4})$", name_no_prefix))
    
    if (length(matches[[1]]) == 3) {
      folder_id <- matches[[1]][2]
      folder_year <- as.character(matches[[1]][3])
      
      # Check if this specific AOI ID exists in the target table
      if (folder_id %in% target_table$id) {
        
        # Retrieve the newly assigned expected year from the table
        expected_year <- as.character(target_table$year[target_table$id == folder_id])
        
        # If the years do not match, flag the folder for deletion
        if (folder_year != expected_year) {
          folders_to_delete <- c(folders_to_delete, folder_path)
        }
      }
    }
  }
  
  # Execution block
  if (length(folders_to_delete) > 0) {
    message(sprintf("Found %d mismatched folders.", length(folders_to_delete)))
    
    for (del_folder in folders_to_delete) {
      if (dry_run) {
        message(paste("[DRY RUN] Would delete:", basename(del_folder)))
      } else {
        message(paste("Deleting:", basename(del_folder)))
        unlink(del_folder, recursive = TRUE)
      }
    }
    
    if (!dry_run) message("Cleanup complete.")
    
  } else {
    message("No mismatched folders found. Directory is clean.")
  }
  
  return(invisible(folders_to_delete))
}

# test before deleting completely 
cleanup_mismatched_aois(target_table = table, export_directory = export_dir, dry_run = TRUE)

# 2. Once verified, run it with dry_run = FALSE to actually delete the data:
# cleanup_mismatched_aois(target_table = table, export_directory = export_dir, dry_run = FALSE)