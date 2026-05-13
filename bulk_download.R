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
# Source functions first so get_env_config() is available
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# TOGGLE ENVIRONMENT HERE (TRUE = MacBook via Tailscale | FALSE = Ubuntu VM via 10G)
# Update the Tailscale IP to match your TrueNAS Tailscale address
env_config <- get_env_config(MAC = TRUE) 

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
  
  # Execute the OS-specific mount command
  exit_status <- system(env_config$mount_cmd)
  
  if (exit_status != 0) {
    stop("Failed to mount the network drive. Check your permissions, VPN connection, and paths.")
  }
  message("Drive mounted successfully.")
} else {
  message("TrueNAS drive is already mounted. Proceeding...")
}

# Ensure the target NAIP directory exists on the network drive
dir.create(env_config$network_storage_dir, showWarnings = FALSE, recursive = TRUE)

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

# ---------------------------------------------------------
# 3. EXECUTION: BATCHING & FURRR
# ---------------------------------------------------------

# Setup parallel backend dynamically based on OS config
plan(multisession, workers = env_config$workers)
message(sprintf("Parallel workers set to: %s", env_config$workers))

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
      local_path = file.path(batch_folder, id),
      network_path = file.path(env_config$network_storage_dir, batch_folder_name, id),
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
        network_dir = env_config$network_storage_dir
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
  # 4. DIRECTORY TRANSFER & CLEANUP
  # ---------------------------------------------------------
  
  # Only trigger rsync if there is actually data inside the local batch folder
  if (length(list.files(batch_folder)) > 0) {
    
    # START NETWORK TRANSFER TIMER
    tic("Network Transfer (Raw Directory)")
    cat(sprintf("\n  [->] Transferring via rsync (Bandwidth limit: %s)...\n", env_config$bwlimit))
    
    transfer_status <- system2(
      "rsync",
      args = c(
        "-avW",
        sprintf("--bwlimit=%s", env_config$bwlimit), 
        batch_folder,
        env_config$network_storage_dir
      )
    )
    
    if (transfer_status == 0) {
      cat("  [✓] Batch", current_batch, "successfully transferred to network.\n")
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
    # Remove the empty local batch directory to keep things clean
    unlink(batch_folder, recursive = TRUE)
  }
  
  # END OVERALL BATCH TIMER
  toc()
  cat("==========================================\n")
}