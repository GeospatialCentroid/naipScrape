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
num_cores <- max(1, parallel::detectCores() - 20)
cl <- makeCluster(num_cores)
registerDoSNOW(cl)

cat("Starting cluster with", num_cores, "cores...\n")

# --- MULTI-YEAR SETUP ---
target_years <- c("2012", "2016", "2020")
# 1. Get the list of all exported files
exported_files <- list.files(path = "data/naipExports", pattern = "\\.tif$")

# 2. Extract the IDs from the filenames
# This regex strips the "oneKM_" or "buffered_" prefix and the "_YYYY.tif" suffix
# "oneKM_1936-4-c-14-2_2015.tif" becomes "1936-4-c-14-2"
exported_ids <- unique(gsub("^(oneKM|buffered)_(.*)_[0-9]{4}\\.tif$", "\\2", exported_files))

# 3. Filter your grids dataframe

# Option A: Keep only the grids that HAVE been exported
grids_completed <- grids |>
  filter(id %in% exported_ids)
#use this to clear out data from the download folder. 
# 1. Define the target directory
# Path derived from the screenshot provided
download_dir <- "data/download"

# 2. Get the full paths of all raw tile files
download_files <- list.files(path = download_dir, pattern = "\\.tif$", full.names = TRUE)

# 3. Extract just the file names (without the directory paths) to run the regex on
file_names <- basename(download_files)

# 4. Extract the grid ID from the filenames
# This regex strips "naip_YYYY_id_" from the front and "_X.tif" from the back
# Example: "naip_2015_id_2000-2-b-16-4_2.tif" becomes "2000-2-b-16-4"
download_ids <- gsub("^naip_[0-9]{4}_id_(.*)_[0-9]+\\.tif$", "\\1", file_names)

# 5. Identify which of these files match the IDs in your completed dataframe
# We use logical subsetting to keep only the file paths where the ID matches
files_to_delete <- download_files[download_ids %in% grids_completed$id]

# 6. Execute the deletion
deleted_count <- sum(file.remove(files_to_delete))

cat("Successfully deleted", deleted_count, "raw tile files.\n")


# Option B: Keep only the grids that HAVE NOT been exported
grids_missing <- grids |>
  filter(!id %in% exported_ids)
# use this to ensure only 
# manual 
target_indices <- 1391:1400 # Replace with 1:nrow(grids) when ready for the full run # 1050 is the current end point 
# target_indices <- 1:nrow(grids_missing) # Replace with 1:nrow(grids) when ready for the full run # 1050 is the current end point 


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
  
  # Fallback year logic (Needs to happen before the file check)
  years <- getNAIPYear(aoi)
  actual_year <- target_year
  if (!target_year %in% years) {
    actual_year <- as.character(as.numeric(target_year) - 1)
  }
  
  # Define the expected final files using naip_dir
  buffExport <- file.path(naip_dir, paste0("buffered_", id, "_", actual_year, ".tif"))
  kmExport <- file.path(naip_dir, paste0("oneKM_", id, "_", actual_year, ".tif"))
  
  # Check if BOTH files already exist
  if (file.exists(buffExport) && file.exists(kmExport)) {
    # Exit this iteration early and report as Skipped
    return(list(id = id, year = target_year, status = "Skipped - Files Exist", time = 0))
  }
  
  # Start the timer for this iteration
  tic()
  
  process_status <- tryCatch({
    # 1. Download raw tiles to temp_dir
    downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir)
    
    # 2. Gather the raw tiles
    naip_string <- paste0("^naip_", actual_year, ".*", id, ".*\\.tif$")
    naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
    
    if (length(naip_files) == 0) {
      stop("Download succeeded but no files matched the regex pattern.")
    }
    
    # 3. Merge and Export (Check removed here since it's handled at the top)
    mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi, year = actual_year)
    
    # ==========================================
    # 4. AGGRESSIVE MEMORY & DISK CLEANUP
    # ==========================================
    file.remove(naip_files) 
    terra::tmpFiles(remove = TRUE)
    rm(naip_files, aoi, naip_string)
    gc(reset = TRUE, full = TRUE)
    # ==========================================
    
    "Success"
  }, error = function(cond) {
    # Even if it fails, try to run garbage collection
    gc(reset = TRUE, full = TRUE)
    terra::tmpFiles(remove = TRUE)
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

# if (length(failed_runs) > 0) {
#   cat("\nError Log:\n")
#   for (fail in failed_runs) {
#     cat("ID:", fail$id, "| Year:", fail$year, "| Error:", fail$status, "\n")
#   }
# }