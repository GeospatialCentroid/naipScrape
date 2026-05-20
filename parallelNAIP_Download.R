# This is a generalized method for downloading material from the planetary computer

pacman::p_load(dplyr, sf, terra, tidyr, tictoc, foreach, doParallel, doSNOW, snic)

# testing
library(tmap)
tmap_mode(mode = "view")

# source files
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# establish grid features
g100 <- sf::st_read("data/grid100km_aea.gpkg")

# random sampling with an LRR
mlra <- sf::st_read(dsn = "data/mlra/lower48MLRA.gpkg") |>
  dplyr::filter(LRRSYM == "F")

# change seed for difference
set.seed(12486)

# generate 18 random spatial samples
points <- sf::st_sample(x = mlra, size = 54, by_polygon = TRUE)
coords_df <- as.data.frame(st_coordinates(points))

# Builds table with 54 features (18 locations * 3 years)
table <- build_index_table(
  years = rep(c("2012", "2016", "2020"), 18),
  lat = coords_df$Y,
  lon = coords_df$X
)

# new table for the update datasets 
table <-  readr::read_csv("temp/missing_features.csv")



# Setup directories
aoi_dir <- file.path("data/aoiExports")
naip_dir <- file.path("data/naipExports")
snic_dir <- file.path("data/snicExports")
lidar_dir <- file.path("data/lidarExports")
temp_dir <- file.path("data/download")
export_dir <- file.path("data/exportData")

# --- EXECUTION TOGGLE ---
run_parallel <- FALSE # Set to TRUE for production, FALSE for sequential debugging
# ------------------------

tic("Total Script Runtime")

if (run_parallel) {
  # ==========================================
  # PARALLEL EXECUTION
  # ==========================================
  num_cores <- max(1, parallel::detectCores() - 8)
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  cat("Starting cluster with", num_cores, "cores...\n")
  
  results <- foreach(
    task_row = 1:nrow(table),
    .packages = c("terra", "sf", "tictoc", "stringr", "purrr"),
    .errorhandling = 'pass'
  ) %dopar% {
    
    # Updated to table$year based on sequential edit
    target_year <- as.character(table$year[task_row]) 
    
    # Extract coordinates to pass to getAOI
    pt_lon <- table$lon[task_row]
    pt_lat <- table$lat[task_row]
    current_point <- c(pt_lon, pt_lat)
    
    # Pass the coordinate vector to getAOI
    aoi <- getAOI(grid100 = g100, point = current_point)
    id <- aoi$id
    
    if (dir.exists(paste0("data/exportData/aoi_", id, "_", target_year))) {
      return(list(id = id, year = target_year, status = "Skipped - Already Exists", time = 0))
    }
    
    years <- getNAIPYear(aoi)
    actual_year <- ifelse(target_year %in% years, target_year, as.character(as.numeric(target_year) - 1))
    
    tic()
    process_status <- tryCatch({
      downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir)
      
      naip_string <- paste0("^naip_",actual_year,".*", id, ".*\\.tif$")
      naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
      
      if (length(naip_files) == 0) stop("No files matched the regex pattern.")
      
      # Updated with year = actual_year argument based on sequential edit
      mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi, year = actual_year)
      
      # r1 <- terra::rast(list.files(path = naip_dir, pattern = paste0("^oneKM_.*", id, ".*\\.tif$"), full.names = TRUE))
      # seeds <- generate_scaled_seeds(r = r1)
      # process_segmentations(r = r1, seed_list = seeds, output_dir = snic_dir, file_id = id, year = actual_year)
      
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
      aoi <- getAOI(grid100 = g100, id = table$Id[task_row])
    }
   
    id <- aoi$id
    
    cat("   -> AOI ID:", id, "| Target Year:", target_year, "\n")
    
    if (dir.exists(paste0("data/exportData/aoi_", id, "_", target_year))) {
      cat("   -> Status: Skipped (Directory already exists)\n")
      return(list(id = id, year = target_year, status = "Skipped - Already Exists", time = 0))
    }
    
    years <- getNAIPYear(aoi)
    actual_year <- ifelse(target_year %in% years, target_year, as.character(as.numeric(target_year) - 1))
    cat("   -> Actual Year assigned:", actual_year, "\n")
    
    tic()
    process_status <- tryCatch({
      
      cat("2. Requesting Planetary Computer Download...\n")
      downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir)
      
      cat("3. Locating Downloaded Files...\n")
      naip_string <- paste0("^naip_",actual_year,".*", id, ".*\\.tif$")
      naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
      
      if (length(naip_files) == 0) {
        stop("Download function passed, but no files matched the regex pattern on disk.")
      }
      cat("   -> Found", length(naip_files), "files to merge.\n")
      
      cat("4. Merging NAIP Imagery...\n")
      mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi,year = actual_year,buffer_only = FALSE)
      
      # cat("5. Starting SNIC Processing...\n")
      # r1_path <- list.files(path = naip_dir, pattern = paste0("^oneKM_.*", id, ".*\\.tif$"), full.names = TRUE)
      # r1 <- terra::rast(r1_path)
      # seeds <- generate_scaled_seeds(r = r1)
      # process_segmentations(r = r1, seed_list = seeds, output_dir = snic_dir, file_id = id, year = actual_year)
      # 
      cat("6. Exporting Final Data...\n")
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

successful_runs <- valid_results[sapply(valid_results, function(x) x$status == "Success")]
failed_runs <- valid_results[sapply(valid_results, function(x) grepl("Failed", x$status))]
skipped_runs <- valid_results[sapply(valid_results, function(x) grepl("Skipped", x$status))]

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

