# This is a generalized method for downloading material from the planetary computer

# Swapped doParallel for doSNOW
pacman::p_load(dplyr, sf, terra, tidyr, tictoc, foreach, doSNOW)

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

# sample areas 
grids <- readr::read_csv("data/LRR_sampleGrids/selectedSample.csv")

# ---------------------------------------------------------
# DIRECTORY SETUP
# ---------------------------------------------------------
# Simplified storage: Focusing just on the NAIP imagery
aoi_dir <- file.path("data/aoiExports")
temp_dir <- file.path("data/download") # Raw tiles go here
naip_dir <- file.path("data/naipExports") # Final merged unique images go here

# Create directories if they don't exist
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(naip_dir, showWarnings = FALSE, recursive = TRUE)

# Initialize storage for NAIP-specific timings
naip_iteration_times <- numeric()

tic("Total Script Runtime") # Overall timer for the whole process

# 1. Setup Parallel Backend with doSNOW
num_cores <- max(1, parallel::detectCores() - 6)
cl <- makeCluster(num_cores)
registerDoSNOW(cl)

cat("Starting cluster with", num_cores, "cores...\n")

# --- MULTI-YEAR SETUP ---
target_years <- c("2012", "2016", "2020")
target_indices <- 751:1050 # Replace with 1:nrow(grids) when ready for the full run

# Create a master task list of all index/year combinations
tasks <- expand.grid(index = target_indices, year = target_years, stringsAsFactors = FALSE)

# Setup the progress bar
iterations <- nrow(tasks)
pb <- txtProgressBar(max = iterations, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# 2. Execute Parallel Loop
results <- foreach(
  task_row = 1:iterations,
  .packages = c("terra", "sf", "tictoc"),
  .errorhandling = 'pass',
  .options.snow = opts # Inject the progress bar here
) %dopar% {
  
  # Extract parameters for this specific task iteration
  i <- tasks$index[task_row]
  target_year <- tasks$year[task_row]
  
  aoi <- getAOI(grid100 = g100, id = grids$id[i])
  id <- aoi$id
  
  # Check if the final merged NAIP image already exists to skip unnecessary processing
  # Adjust this filename check based on what mergeAndExportNAIP outputs
  expected_file <- file.path(naip_dir, paste0("naip_", target_year, "_", id, ".tif"))
  if (file.exists(expected_file)) {
    return(list(id = id, year = target_year, status = "Skipped - Already Exists", time = 0))
  }
  
  # Fallback year logic
  years <- getNAIPYear(aoi)
  actual_year <- target_year
  if (!target_year %in% years) {
    actual_year <- as.character(as.numeric(target_year) - 1)
  }
  
  # Start the timer for this iteration
  tic()
  
  process_status <- tryCatch({
    # 1. Download raw tiles to temp_dir
    downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir)
    
    # 2. Gather the raw tiles
    naip_string <- paste0("^naip_",actual_year,".*", id, ".*\\.tif$")
    naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
    
    if (length(naip_files) == 0) {
      stop("Download succeeded but no files matched the regex pattern.")
    }
    
    # 3. Merge and Export
    # This function should save the final, single unique image to naip_dir
    mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi)
    
    # Optional cleanup: Delete the raw tiles from temp_dir to save disk space
    process_status <- tryCatch({
      # 1. Download raw tiles to temp_dir
      downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir)
      
      # 2. Gather the raw tiles
      naip_string <- paste0("^naip_",actual_year,".*", id, ".*\\.tif$")
      naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
      
      if (length(naip_files) == 0) {
        stop("Download succeeded but no files matched the regex pattern.")
      }
      
      # 3. Merge and Export
      mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi)
      
      # ==========================================
      # 4. AGGRESSIVE MEMORY & DISK CLEANUP
      # ==========================================
      
      # A. Delete the raw downloaded tiles to prevent disk space exhaustion
      # (Highly recommended: Disk IO failures often masquerade as memory errors in terra)
      file.remove(naip_files) 
      
      # B. Clean up terra's hidden temporary files for this specific worker session
      terra::tmpFiles(remove = TRUE)
      
      # C. Explicitly remove large objects from the worker's environment
      rm(naip_files, aoi, naip_string)
      
      # D. Force R to run garbage collection and release RAM back to the OS
      gc(reset = TRUE, full = TRUE)
      
      # ==========================================
      
      "Success"
    }, error = function(cond) {
      # Even if it fails, try to run garbage collection
      gc(reset = TRUE, full = TRUE)
      terra::tmpFiles(remove = TRUE)
      return(paste("Failed:", conditionMessage(cond)))
    })
    
    "Success"
  }, error = function(cond) {
    return(paste("Failed:", conditionMessage(cond)))
  })
  
  # Capture the time
  t_out <- toc(quiet = TRUE)
  elapsed_time <- t_out$toc - t_out$tic
  
  # Return the task results
  return(list(id = id, year = target_year, status = process_status, time = elapsed_time))
}

# Close the progress bar and stop the cluster
close(pb)
stopCluster(cl)
total_runtime <- toc()

# ---------------------------------------------------------
# REPORTING
# ---------------------------------------------------------

# Filter out raw errors.
valid_results <- Filter(function(x) is.list(x) && !is.null(x$status), results)

# Safely categorize the results
successful_runs <- valid_results[sapply(valid_results, function(x) x$status == "Success")]
failed_runs <- valid_results[sapply(valid_results, function(x) grepl("Failed", x$status))]
skipped_runs <- valid_results[sapply(valid_results, function(x) grepl("Skipped", x$status))]

success_times <- sapply(successful_runs, function(x) x$time)

cat("\n==========================================\n")
cat("PARALLEL PROCESSING SUMMARY\n")
cat("Total Tasks Attempted: ", nrow(tasks), "\n")
cat("Successful: ", length(successful_runs), "\n")
cat("Failed: ", length(failed_runs), "\n")
cat("Skipped: ", length(skipped_runs), "\n")
cat("------------------------------------------\n")
if (length(success_times) > 0) {
  cat(
    "Avg time per successful loop: ",
    round(mean(success_times), 2),
    "seconds\n"
  )
}
cat(
  "Total script runtime: ",
  round(total_runtime$toc - total_runtime$tic, 2),
  "seconds\n"
)
cat("==========================================\n")

if (length(failed_runs) > 0) {
  cat("\nError Log:\n")
  for (fail in failed_runs) {
    cat("ID:", fail$id, "| Year:", fail$year, "| Error:", fail$status, "\n")
  }
}