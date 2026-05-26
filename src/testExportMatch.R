pacman::p_load(
  dplyr,
  sf,
  terra,
  tidyr,
  furrr,
  tmap,
  tictoc
)

# ---------------------------------------------------------
# 1. SETUP & DIRECTORIES
# ---------------------------------------------------------
# Source functions first so get_env_config() is available
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# TOGGLE ENVIRONMENT HERE (TRUE = MacBook via Tailscale | FALSE = Ubuntu VM via 10G)
env_config <- get_env_config(MAC = FALSE) 

message(sprintf("Initializing in %s mode...", env_config$os_env))

aoi_table <- read.csv("data/LRR_sampleGrids/selectedSample_lrr_F_05_2026.csv")
local_working_dir <- "data/processing_batches"
dir.create(local_working_dir, showWarnings = FALSE, recursive = TRUE)

# establish grid features
g100 <- sf::st_read("data/grid100km_aea.gpkg")

# Check if the mount point appears in the list of currently mounted drives
is_mounted <- any(grepl(env_config$mount_point, system("mount", intern = TRUE)))

if (!is_mounted) {
  message("Connecting to TrueNAS network drive...")
  exit_status <- system(env_config$mount_cmd)
  
  if (exit_status != 0) {
    stop("Failed to mount the network drive. Check your permissions, VPN connection, and paths.")
  }
  message("Drive mounted successfully.")
} else {
  message("TrueNAS drive is already mounted. Proceeding...")
}

dir.create(env_config$network_storage_dir, showWarnings = FALSE, recursive = TRUE)


# list of all unique folders in the NAIP directoruy 
naip_folders <- list.dirs(env_config$network_storage_dir)
# for each folder I want to see 

# ---------------------------------------------------------
# 2. DIRECTORY AUDIT (UPDATED WITH HIERARCHY LOGIC)
# ---------------------------------------------------------
expected_years <- c("2012", "2016", "2020")
imagery_extension <- "\\.tif$"

# Get all batch folders within the network storage dir
batch_folders <- list.dirs(env_config$network_storage_dir, recursive = FALSE)

# Get all ID folders nested within those batch folders
all_id_folders <- list.dirs(batch_folders, recursive = FALSE)

# Filter the discovered folders to only those present in your AOI table
target_id_folders <- all_id_folders[basename(all_id_folders) %in% aoi_table$id]

message(sprintf("Scanning %d ID folders across batches...", length(target_id_folders)))

# Define the audit function for a single directory
audit_folder <- function(folder_path) {
  
  folder_id <- basename(folder_path)
  files <- list.files(folder_path, full.names = FALSE)
  
  # 1. Check for GeoPackage
  has_gpkg <- any(grepl("\\.gpkg$", files, ignore.case = TRUE))
  
  # 2. Check for Imagery Files
  imagery_files <- files[grepl(imagery_extension, files, ignore.case = TRUE)]
  
  # 3. Extract years from the imagery filenames
  found_years <- sub(".*_(\\d{4})\\.tif$", "\\1", imagery_files, ignore.case = TRUE)
  found_years <- unique(found_years[grepl("^\\d{4}$", found_years)])
  
  # 4. Evaluate against the Hierarchy Logic
  missing_targets <- character(0)
  
  if (length(found_years) < 3) {
    # Apply assessment to target years to find which are missing
    for (target_year in expected_years) {
      target_num <- as.numeric(target_year)
      
      # The exact testing hierarchy provided
      preferred_years <- as.character(c(
        target_num,     # 2. Test initial year (e.g., 2012)
        target_num - 1, # 3. Move one year down (e.g., 2011)
        target_num - 2, # 4. Move two years down (e.g., 2010)
        target_num + 1  # 5. Move one year up (e.g., 2013)
      ))
      
      # If none of the found years align with this target's preferred window, it's missing
      if (!any(found_years %in% preferred_years)) {
        missing_targets <- c(missing_targets, target_year)
      }
    }
  }
  
  # Area is complete if it has the GPKG and 3 or more distinct imagery years
  is_complete <- has_gpkg && (length(found_years) >= 3)
  
  # Return a single-row tibble for this folder
  tibble(
    id = folder_id,
    folder_path = folder_path,
    has_gpkg = has_gpkg,
    total_images_found = length(found_years),
    found_years = paste(found_years, collapse = ", "),
    missing_target_years = paste(missing_targets, collapse = ", "),
    is_complete = is_complete
  )
}

# Execute the audit across all folders
audit_results <- purrr::map_dfr(target_id_folders, audit_folder)

# Filter for missing datasets to generate the final report
missing_data_report <- audit_results %>%
  filter(!is_complete)

# lets remove any AOI's that are note present in the establish grid draw 



# Export the missing data report to the local working directory
output_file <- file.path("data","downloadChecks", "missing_naip_datasets.csv")
write.csv(missing_data_report, output_file, row.names = FALSE)

message(sprintf("Audit complete. %d incomplete folders found. Report saved to %s", 
                nrow(missing_data_report), output_file))



