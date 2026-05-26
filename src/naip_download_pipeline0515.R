# ---------------------------------------------------------
# SEQUENTIAL NAIP DOWNLOAD PIPELINE (AOI Folders)
# ---------------------------------------------------------
pacman::p_load(dplyr, sf, terra, tidyr, tictoc, rstac)

# source files
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# establish grid features
g100 <- sf::st_read("data/grid100km_aea.gpkg")

# Load only the specified' input data
grids <- readr::read_csv("data/LRR_sampleGrids/selectedSample_lrr_F_05_2026.csv")


# missing locations from the bulk download 
aoi_table <- read.csv("data/downloadChecks/missing_naip_datasets.csv")
unique_grid_ids <- unique(aoi_table$id)




# ---------------------------------------------------------
# DIRECTORY, EXECUTION & PARAMETER SETUP
# ---------------------------------------------------------
aoi_dir <- file.path("data/aoiExports")
temp_dir <- file.path("data/download") # Raw tiles go here

local <- TRUE
if(local){
  naip_dir <- file.path("data/naipExports") # Final merged unique images go here
}else{
  naip_dir <- "/Volumes/wcnr-network/Research/Ogle/Agroforestry/phase2_sampling/data/raw/mlraF_NAIP"
}

# --- TOGGLE BUFFER & NAMING CONVENTION ---
use_buffer <- TRUE

# Create main directories if they don't exist
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(naip_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(aoi_dir, showWarnings = FALSE, recursive = TRUE)

tic("Total Script Runtime") # Overall timer for the whole process

# --- MULTI-YEAR SETUP ---
multiYear <- TRUE 
if(isTRUE(multiYear)){
  target_years <- c("2012", "2016", "2020")
  # Create a master task list of all grid ID / year combinations
  tasks <- expand.grid(id = unique_grid_ids, year = target_years, stringsAsFactors = FALSE)
}else{
  tasks <- grids
}

# filter the grids to only include the missing years 
# 1. Expand the comma-separated string into individual rows, keeping it as character
aoi_expanded <- aoi_table %>%
  select(id, missing_target_years) %>%
  # Split the string by comma and optional space
  separate_rows(missing_target_years, sep = ",\\s*")

# 2. Match them up (both sides are now <character> type)
filtered_tasks <- tasks %>%
  semi_join(aoi_expanded, by = c("id" = "id", "year" = "missing_target_years"))

# reaasign the tasks object 
tasks <- filtered_tasks
  

iterations <- nrow(tasks)
all_results <- vector("list", iterations)
# ---------------------------------------------------------
# EXECUTION BLOCK (SEQUENTIAL ONLY)
# ---------------------------------------------------------

cat(sprintf("\n--- Starting Sequential Processing (%d tasks) ---\n", iterations))
pb <- txtProgressBar(max = iterations, style = 3)

for (task_row in 1:iterations) {
  setTxtProgressBar(pb, task_row)
  
  current_id <- tasks$id[task_row]
  target_year <- tasks$year[task_row]
  
  # Retrieve AOI using the ID from the file
  aoi <- getAOI(grid100 = g100, id = current_id)
  id <- aoi$id
  
  # --- 1. DIRECTORY & GEOPACKAGE EXPORT ---
  # Create a dedicated folder for this AOI
  aoi_out_dir <- file.path(naip_dir, id)
  dir.create(aoi_out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Save the AOI geometry as a GeoPackage within the AOI folder
  gpkg_path <- file.path(aoi_out_dir, paste0("aoi-", id, ".gpkg"))
  if (!file.exists(gpkg_path)) {
    sf::st_write(aoi, dsn = gpkg_path, driver = "GPKG", quiet = TRUE, append = FALSE)
  }
  
  # --- 2. API YEAR CHECK ---
  years_available <- tryCatch({
    getNAIPYear(aoi)
  }, error = function(cond) {
    message(sprintf("\nSTAC API Error on getNAIPYear for ID %s: %s", id, conditionMessage(cond)))
    return(NULL) 
  })
  
  if (is.null(years_available)) {
    all_results[[task_row]] <- list(id = id, year = target_year, status = "Failed - STAC API Error", time = 0)
    next 
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
    all_results[[task_row]] <- list(id = id, year = target_year, status = "Failed - No imagery found within fallback range", time = 0)
    next 
  }
  
  # --- 4. EXPORT NAMING & EXISTENCE CHECK ---
  prefix <- ifelse(use_buffer, "buffered", "1km")
  expected_tif_name <- paste0(prefix, "_", id, "_", actual_year, ".tif")
  expected_tif_path <- file.path(aoi_out_dir, expected_tif_name)
  
  if (file.exists(expected_tif_path)) {
    all_results[[task_row]] <- list(id = id, year = target_year, status = "Skipped - Files Exist", time = 0)
    next 
  }
  
  # --- 5. DOWNLOAD & MERGE ---
  tic()
  process_status <- tryCatch({
    # Pass the toggle to the updated VSI download function
    downloadNAIP_vsi(aoi = aoi, year = actual_year, exportFolder = temp_dir, buffer_m = 250)
    
    naip_string <- paste0("^naip_", actual_year, "_id_", id, "_[0-9]+\\.tif$")
    naip_files <- list.files(path = temp_dir, pattern = naip_string, full.names = TRUE)
    
    if (length(naip_files) == 0) stop("Download succeeded but no files matched the regex pattern.")
    
    # Merge and export directly into the new AOI-specific folder
    mergeAndExportNAIP(files = naip_files, out_path = aoi_out_dir, aoi = aoi, year = actual_year, buffer_m = 250, buffer_only = TRUE)
    
    # --- STRICT NAMING ENFORCEMENT ---
    # Find the newly generated .tif file in the folder for this specific year
    new_files <- list.files(aoi_out_dir, pattern = paste0(".*", actual_year, ".*\\.tif$"), full.names = TRUE)
    
    # If a file was generated and it doesn't match our exact 1km/buffered convention, rename it
    if (length(new_files) > 0 && !(expected_tif_path %in% new_files)) {
      file.rename(new_files[1], expected_tif_path)
    }
    
    # Cleanup raw tiles
    file.remove(naip_files) 
    terra::tmpFiles(remove = TRUE)
    rm(naip_files, naip_string)
    gc(reset = TRUE, full = TRUE)
    
    "Success"
  }, error = function(cond) {
    gc(reset = TRUE, full = TRUE)
    terra::tmpFiles(remove = TRUE)
    return(paste("Failed:", conditionMessage(cond)))
  })
  
  # --- 6. SQLITE TEMPORARY FILE CLEANUP ---
  # Delete the lingering -shm and -wal GeoPackage files to keep the directory clean
  temp_gpkg_files <- list.files(aoi_out_dir, pattern = "\\.gpkg-(shm|wal)$", full.names = TRUE)
  if (length(temp_gpkg_files) > 0) {
    file.remove(temp_gpkg_files)
  }
  
  t_out <- toc(quiet = TRUE)
  elapsed_time <- t_out$toc - t_out$tic
  all_results[[task_row]] <- list(id = id, year = target_year, status = process_status, time = elapsed_time)
}

close(pb)
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
cat("SEQUENTIAL PROCESSING SUMMARY\n")
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



# ---------------------------------------------------------
# POST-DOWNLOAD MISSING DATA AUDIT
# ---------------------------------------------------------
cat("\n==========================================\n")
cat("POST-DOWNLOAD AUDIT: REMAINING MISSING IMAGERY\n")
cat("==========================================\n")

# 1. Identify which unique IDs were processed in this script
distinct_ids <- unique(tasks$id)

# 2. Map through the localized output folders to verify actual file presence
remaining_missing <- lapply(distinct_ids, function(current_id) {
  
  aoi_out_dir <- file.path(naip_dir, current_id)
  
  # If the folder doesn't exist at all, all target years are missing
  if (!dir.exists(aoi_out_dir)) {
    return(tibble(
      id = current_id, 
      remaining_missing_years = paste(target_years, collapse = ", ")
    ))
  }
  
  # Scan the directory for downloaded .tif files
  files <- list.files(aoi_out_dir, pattern = "\\.tif$", ignore.case = TRUE)
  
  # Extract years from the filenames (matching your regex format)
  found_years <- sub(".*_(\\d{4})\\.tif$", "\\1", files, ignore.case = TRUE)
  found_years <- unique(found_years[grepl("^\\d{4}$", found_years)])
  
  missing_targets <- character(0)
  
  # Re-evaluate using your preferred fallback hierarchy
  for (target_year in target_years) {
    target_num <- as.numeric(target_year)
    preferred_years <- as.character(c(
      target_num,     
      target_num - 1, 
      target_num - 2, 
      target_num + 1  
    ))
    
    if (!any(preferred_years %in% found_years)) {
      missing_targets <- c(missing_targets, target_year)
    }
  }
  
  # Return a row only if there are still missing years
  if (length(missing_targets) > 0) {
    return(tibble(
      id = current_id,
      remaining_missing_years = paste(missing_targets, collapse = ", ")
    ))
  } else {
    return(NULL)
  }
}) %>% 
  bind_rows()

# 3. Print the diagnostic report to the console
if (nrow(remaining_missing) > 0) {
  cat(sprintf("Warning: %d IDs are still missing target imagery windows after running.\n\n", nrow(remaining_missing)))
  print(as.data.frame(remaining_missing), row.names = FALSE)
  
  # Optional: Save a delta report so you don't overwrite your primary tracker
  delta_output_file <- file.path("data", "downloadChecks", "still_missing_after_run.csv")
  write.csv(remaining_missing, delta_output_file, row.names = FALSE)
  cat(sprintf("\nDetailed delta log saved to: %s\n", delta_output_file))
} else {
  cat("Success! All attempted task IDs now fulfill their target imagery windows.\n")
}
cat("==========================================\n")

