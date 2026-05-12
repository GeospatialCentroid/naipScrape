# =========================================================
# NAIP BULK DOWNLOAD & PROCESSING PIPELINE
# =========================================================

pacman::p_load(
  dplyr,
  sf,
  terra,
  tidyr,
  furrr,
  future,
  DBI,
  RSQLite,
  tools,
  tmap,
  tictoc
)


# ---------------------------------------------------------
# 1. SETUP & DIRECTORIES
# ---------------------------------------------------------
aoi_table <- read.csv("data/LRR_sampleGrids/selectedSample_lrr_F_05_2026.csv")
local_working_dir <- "data/processing_batches"
network_storage_dir <- "mnt/fileShare/NAIP" # Update to your mount path

dir.create(local_working_dir, showWarnings = FALSE, recursive = TRUE)

# source functions
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# establish grid features
g100 <- sf::st_read("data/grid100km_aea.gpkg")

mount_point <- "/home/dune/trueNAS/work/naipScrape/mnt/fileShare"

# Check if the mount point appears in the list of currently mounted drives
is_mounted <- any(grepl(mount_point, system("mount", intern = TRUE)))

if (!is_mounted) {
  message("Connecting to TrueNAS network drive...")
  
  # The command to run
  # Note: See the steps below on how to handle the 'sudo' password
  cmd <- "sudo mount -t cifs -o guest,uid=$(id -u),gid=$(id -g) //192.168.20.101/fileShare /home/dune/trueNAS/work/naipScrape/mnt/fileShare"
  
  # Execute the command
  exit_status <- system(cmd)
  
  if (exit_status != 0) {
    stop("Failed to mount the network drive. Check your permissions and network connection.")
  }
  message("Drive mounted successfully.")
} else {
  message("TrueNAS drive is already mounted. Proceeding...")
}

# ---------------------------------------------------------
# 2. SQLITE DATABASE INITIALIZATION
# ---------------------------------------------------------
db_path <- "data/download_tracker.sqlite"
con <- dbConnect(RSQLite::SQLite(), db_path)

# Create table if it doesn't exist
dbExecute(
  con,
  "
  CREATE TABLE IF NOT EXISTS aoi_tracker (
    aoi_id TEXT PRIMARY KEY,
    batch_id INTEGER,
    year_1 TEXT,
    year_2 TEXT,
    year_3 TEXT,
    status TEXT,
    timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
  )
"
)
dbDisconnect(con)

#
# ---------------------------------------------------------
# 4. EXECUTION: BATCHING & FURRR
# ---------------------------------------------------------

# Setup parallel backend (adjust workers to your CPU, leaving a few free)
plan(multisession, workers = 28) # 12 worker around ~20gb ram usage
#

# Create batches of 50
batch_size <- 50
aoi_table <- aoi_table |>
  mutate(batch_id = ceiling(row_number() / batch_size))

target_years <- c("2012", "2016", "2020")
unique_batches <- unique(aoi_table$batch_id)

for (current_batch in seq_along(unique_batches)) {  
  # START OVERALL BATCH TIMER
  tic(paste("Total Time for Batch", current_batch))
  
  batch_folder_name <- paste0("naip_batch_", current_batch)
  batch_folder <- file.path(local_working_dir, batch_folder_name)
  dir.create(batch_folder, showWarnings = FALSE)
  
  # PRE-PROCESSING FILTER: Check local and network drives
  batch_data <- aoi_table |> 
    filter(batch_id == current_batch) |>
    mutate(
      # Assuming process_aoi() creates a directory named after the ID.
      # If it outputs a specific file, change dir.exists() to file.exists() 
      # and append the extension to the paths (e.g., paste0(id, ".tif")).
      local_path = file.path(batch_folder, id),
      network_path = file.path(network_storage_dir, batch_folder_name, id),
      is_processed = dir.exists(local_path) | dir.exists(network_path)
    )
  
  to_process <- batch_data |> filter(!is_processed)
  
  cat("\n==========================================\n")
  cat("Starting Batch", current_batch, "\n")
  cat("Total AOIs:", nrow(batch_data), "| Skipping:", nrow(batch_data) - nrow(to_process), "| Processing:", nrow(to_process), "\n")
  
  if (nrow(to_process) > 0) {
    # START IMAGE PROCESSING TIMER
    tic("Image Processing (Furrr)")
    
    results <- future_map(
      to_process$id,
      ~ process_aoi(
        aoi_id = .x,
        target_years = target_years,
        local_dir = batch_folder,
        g100_grid = g100,
        db_path = db_path,
        batch_id = current_batch,
        network_dir = network_storage_dir
      ),
      .progress = TRUE,
      .options = furrr_options(seed = TRUE)
    )
    
    # END IMAGE PROCESSING TIMER
    toc()
  } else {
    cat("  [✓] All AOIs in this batch already exist locally or on the network.\n")
  }
  
  # ---------------------------------------------------------
  # 5. DIRECTORY TRANSFER & CLEANUP
  # ---------------------------------------------------------
  
  # Only trigger rsync if there is actually data inside the local batch folder
  if (length(list.files(batch_folder)) > 0) {
    
    # START NETWORK TRANSFER TIMER
    tic("Network Transfer (Raw Directory)")
    cat("\n  [->] Transferring raw directory via rsync...\n")
    
    transfer_status <- system2(
      "rsync",
      args = c(
        "-avW",
        "--bwlimit=700M", # Capping at ~5.6 Gbps so it doesn't totally saturate the 10Gb link
        batch_folder,
        network_storage_dir
      )
    )
    
    if (transfer_status == 0) {
      cat("  [✓] Batch", current_batch, "successfully transferred to network.\n")
      
      # Clean up local files upon verified successful transfer
      cat("  [->] Removing local batch directory...\n")
      unlink(batch_folder, recursive = TRUE)
    } else {
      warning(
        "Transfer failed for Batch ",
        current_batch,
        ". Local files retained for manual review."
      )
    }
    
    # END NETWORK TRANSFER TIMER
    toc()
    
  } else {
    # If the folder was created but nothing was downloaded (e.g., everything was skipped),
    # remove the empty local batch directory to keep things clean.
    unlink(batch_folder, recursive = TRUE)
  }
  
  # END OVERALL BATCH TIMER
  toc()
  cat("==========================================\n")
}
