# This is a generalized method for downloading material from the planetary computer
# Swapped doParallel for doSNOW
pacman::p_load(dplyr, sf, terra, tidyr, tictoc, foreach, doSNOW, rstac)

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
# updating for model runs 
grids <-  readr::read_csv("temp/missing_features.csv")
# format for this species request 
grids2 <- grids |>
  dplyr::select(
    index = Id,
    year = year
  )
# generating data for all poential grids 
grids <- readr::read_csv("~/trueNAS/work/naipScrape/data/LLR_F_grid_ids_and_years.csv")
names(grids) <- c("id","year")



# ---------------------------------------------------------
# DIRECTORY & EXECUTION SETUP
# ---------------------------------------------------------
aoi_dir <- file.path("data/aoiExports")
temp_dir <- file.path("data/download") # Raw tiles go here

local <- TRUE
if(local){
  naip_dir <- file.path("data/naipExports") # Final merged unique images go here
}else{
  naip_dir <- "/Volumes/wcnr-network/Research/Ogle/Agroforestry/phase2_sampling/data/raw/mlraF_NAIP"
}

# ---------------------------------------------------------
# TOGGLE EXECUTION METHOD HERE
use_parallel <- FALSE
# ---------------------------------------------------------

# Create directories if they don't exist
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(naip_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(aoi_dir, showWarnings = FALSE, recursive = TRUE)

tic("Total Script Runtime") # Overall timer for the whole process

# --- MULTI-YEAR SETUP ---
target_years <- c("2012", "2016", "2020")
exported_files <- list.files(path = "data/naipExports", pattern = "\\.tif$")
exported_ids <- unique(gsub("^(oneKM|buffered)_(.*)_[0-9]{4}\\.tif$", "\\2", exported_files))

grids_missing <- grids |>
  filter(!id %in% exported_ids)

target_indices <- 12000:15000

# Create a master task list of all index/year combinations
tasks <- expand.grid(index = target_indices, year = target_years, stringsAsFactors = FALSE)
# setting up tasks for unique jobs 
tasks <- expand.grid(index = grids$id, year = target_years, stringsAsFactors = FALSE)



# ---------------------------------------------------------
# BATCHING SETUP
# ---------------------------------------------------------
chunk_size <- 10
# Split the tasks dataframe into a list of smaller dataframes (max 100 rows each)
task_chunks <- split(tasks, ceiling(seq_len(nrow(tasks)) / chunk_size))

all_results <- list() # Master list to store results across all batches

# ---------------------------------------------------------
# EXECUTION BLOCK
# ---------------------------------------------------------

# If parallel, start the cluster ONCE for all batches to save overhead
if (use_parallel) {
  num_cores <- max(1, 6)
  cl <- makeCluster(num_cores)
  registerDoSNOW(cl)
  cat("Starting cluster with", num_cores, "cores...\n")
}

for (batch_idx in seq_along(task_chunks)) {
  
  current_tasks <- task_chunks[[batch_idx]]
  iterations <- nrow(current_tasks)
  
  cat(sprintf("\n--- Starting Batch %d of %d (%d tasks) ---\n", batch_idx, length(task_chunks), iterations))
  
  pb <- txtProgressBar(max = iterations, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  if (use_parallel) {
    # --- PARALLEL METHOD ---
    batch_results <- foreach(
      task_row = 1:iterations,
      .packages = c("terra", "sf", "tictoc"),
      .errorhandling = 'pass',
      .options.snow = opts 
    ) %dopar% {
      
      i <- current_tasks$index[task_row]
      target_year <- current_tasks$year[task_row]
      
      aoi <- getAOI(grid100 = g100, id = grids$id[i])
      id <- aoi$id
      
      years_available <- getNAIPYear(aoi)
      
      # --- ROBUST YEAR HANDLING ---
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
        return(list(id = id, year = target_year, status = "Failed - No imagery found within fallback range", time = 0))
      }
      # ----------------------------
      
      kmExport <- file.path(naip_dir, paste0("oneKM_", id, "_", actual_year, ".tif"))
      
      if (file.exists(kmExport)) {
        return(list(id = id, year = target_year, status = "Skipped - Files Exist", time = 0))
      }
      
      tic()
      process_status <- tryCatch({
        downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir, buffered = FALSE)
        
        naip_string <- paste0("^naip_", actual_year, ".*", id, ".*\\.tif$")
        naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
        
        if (length(naip_files) == 0) stop("Download succeeded but no files matched the regex pattern.")
        
        mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi, year = actual_year)
        
        file.remove(naip_files) 
        terra::tmpFiles(remove = TRUE)
        rm(naip_files, aoi, naip_string)
        gc(reset = TRUE, full = TRUE)
        
        "Success"
      }, error = function(cond) {
        gc(reset = TRUE, full = TRUE)
        terra::tmpFiles(remove = TRUE)
        return(paste("Failed:", conditionMessage(cond)))
      })
      
      t_out <- toc(quiet = TRUE)
      elapsed_time <- t_out$toc - t_out$tic
      
      return(list(id = id, year = target_year, status = process_status, time = elapsed_time))
    }
    close(pb)
    
  } else {
    # --- SEQUENTIAL METHOD ---
    batch_results <- vector("list", iterations)
    
    for (task_row in 1:iterations) {
      setTxtProgressBar(pb, task_row)
      
      i <- current_tasks$index[task_row]
      target_year <- current_tasks$year[task_row]
      
      aoi <- getAOI(grid100 = g100, id = grids$id[task_row])
      id <- aoi$id
      
      years_available <- tryCatch({
        getNAIPYear(aoi)
      }, error = function(cond) {
        message(sprintf("\nSTAC API Error on getNAIPYear for ID %s: %s", id, conditionMessage(cond)))
        return(NULL) 
      })
      
      if (is.null(years_available)) {
        batch_results[[task_row]] <- list(id = id, year = target_year, status = "Failed - STAC API Error", time = 0)
        next 
      }
      
      # --- ROBUST YEAR HANDLING ---
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
        batch_results[[task_row]] <- list(id = id, year = target_year, status = "Failed - No imagery found within fallback range", time = 0)
        next 
      }
      # ----------------------------
      
      kmExport <- file.path(naip_dir, paste0("oneKM_", id, "_", actual_year, ".tif"))
      
      if (file.exists(kmExport)) {
        batch_results[[task_row]] <- list(id = id, year = target_year, status = "Skipped - Files Exist", time = 0)
        next 
      }
      
      tic()
      process_status <- tryCatch({
        downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir, buffered = FALSE)
        
        naip_string <- paste0("^naip_", actual_year, ".*", id, ".*\\.tif$")
        naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
        
        if (length(naip_files) == 0) stop("Download succeeded but no files matched the regex pattern.")
        
        mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi, year = actual_year)
        
        file.remove(naip_files) 
        terra::tmpFiles(remove = TRUE)
        rm(naip_files, aoi, naip_string)
        gc(reset = TRUE, full = TRUE)
        
        "Success"
      }, error = function(cond) {
        gc(reset = TRUE, full = TRUE)
        terra::tmpFiles(remove = TRUE)
        return(paste("Failed:", conditionMessage(cond)))
      })
      
      t_out <- toc(quiet = TRUE)
      elapsed_time <- t_out$toc - t_out$tic
      batch_results[[task_row]] <- list(id = id, year = target_year, status = process_status, time = elapsed_time)
    }
    close(pb)
  }
  
  # Append this batch's results to the master list
  all_results <- c(all_results, batch_results)
  
  # ---------------------------------------------------------
  # POST-BATCH: ZIP AND CLEANUP
  # ---------------------------------------------------------
  cat("\nZipping and cleaning up files for Batch", batch_idx, "...\n")
  
  # Identify all .tif files currently in the naip_dir
  files_to_zip <- list.files(naip_dir, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(files_to_zip) > 0) {
    zip_name <- paste0("naip_batch_", batch_idx, ".zip")
    zip_path <- file.path(naip_dir, zip_name)
    
    # Temporarily set working directory to naip_dir so the zip is flat
    orig_wd <- getwd()
    setwd(naip_dir)
    
    # Zip the files (using base R zip, just passing basenames since we are in the directory)
    utils::zip(zipfile = zip_name, files = basename(files_to_zip), flags = "-q")
    
    # Return to original working directory
    setwd(orig_wd)
    
    # Check if zip was successful, then delete raw files
    if (file.exists(zip_path)) {
      file.remove(files_to_zip)
      cat("--> Successfully created", zip_name, "and removed raw .tif files.\n")
    } else {
      cat("--> Warning: Zip creation failed. Raw .tif files were NOT removed.\n")
    }
  } else {
    cat("--> No new .tif files found to zip for this batch.\n")
  }
}

# Close the cluster if it was opened
if (use_parallel) {
  stopCluster(cl)
  cat("Cluster closed.\n")
}

total_runtime <- toc()

# ---------------------------------------------------------
# REPORTING
# ---------------------------------------------------------
valid_results <- Filter(function(x) is.list(x) && !is.null(x$status), all_results)

successful_runs <- valid_results[sapply(valid_results, function(x) x$status == "Success")]
failed_runs <- valid_results[sapply(valid_results, function(x) grepl("Failed", x$status))]
skipped_runs <- valid_results[sapply(valid_results, function(x) grepl("Skipped", x$status))]

success_times <- sapply(successful_runs, function(x) x$time)

cat("\n==========================================\n")
cat(ifelse(use_parallel, "PARALLEL", "SEQUENTIAL"), "PROCESSING SUMMARY\n")
cat("Total Tasks Attempted: ", nrow(tasks), "\n")
cat("Successful: ", length(successful_runs), "\n")
cat("Failed: ", length(failed_runs), "\n")
cat("Skipped: ", length(skipped_runs), "\n")
cat("------------------------------------------\n")
if (length(success_times) > 0) {
  cat("Avg time per successful task: ", round(mean(success_times), 2), "seconds\n")
}
cat("Total script runtime: ", round(total_runtime$toc - total_runtime$tic, 2), "seconds\n")
cat("==========================================\n")