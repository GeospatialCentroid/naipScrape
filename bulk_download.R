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
# testing spatail objects
tmap_mode(mode = "view")

# ---------------------------------------------------------
# 1. SETUP & DIRECTORIES
# ---------------------------------------------------------
aoi_table <- read.csv("data/LRR_sampleGrids/LRR_F_selectedSample.csv")
local_working_dir <- "mnt/fileShare/NAIP"
network_storage_dir <- "mnt/fileShare/NAIP" # Update to your mount path

dir.create(local_working_dir, showWarnings = FALSE, recursive = TRUE)

# source functions
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# establish grid features
g100 <- sf::st_read("data/grid100km_aea.gpkg")
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
plan(multisession, workers = 8) # 12 worker around ~20gb ram usage
#

# Create batches of 50
batch_size <- 50
aoi_table <- aoi_table |>
  mutate(batch_id = ceiling(row_number() / batch_size))

target_years <- c("2012", "2016", "2020")
unique_batches <- unique(aoi_table$batch_id)

for (current_batch in 4:28) {
  # START OVERALL BATCH TIMER
  tic(paste("Total Time for Batch", current_batch))

  batch_data <- aoi_table |> filter(batch_id == current_batch)

  batch_folder_name <- paste0("naip_batch_", current_batch)
  batch_folder <- file.path(local_working_dir, batch_folder_name)
  dir.create(batch_folder, showWarnings = FALSE)

  cat("\n==========================================\n")
  cat("Starting Batch", current_batch, "with", nrow(batch_data), "AOIs...\n")

  # START IMAGE PROCESSING TIMER
  tic("Image Processing (Furrr)")

  results <- future_map(
    batch_data$id,
    ~ process_aoi(
      aoi_id = .x,
      target_years = target_years,
      local_dir = batch_folder,
      g100_grid = g100,
      db_path = db_path,
      batch_id = current_batch
    ),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )

  # END IMAGE PROCESSING TIMER
  toc()

  # ---------------------------------------------------------
  # 5. DIRECTORY TRANSFER (NO ZIPPING)
  # ---------------------------------------------------------

  # # START NETWORK TRANSFER TIMER
  # tic("Network Transfer (Raw Directory)")

  # cat("\n  [->] Transferring raw directory via rsync...\n")

  # # Transfer via system rsync directly to the network storage dir
  # # Note: No trailing slash on batch_folder ensures the directory itself is copied
  # transfer_status <- system2(
  #   "rsync",
  #   args = c(
  #     "-avW",
  #     # "--remove-source-files", # seems like it ran but didn't not present so
  #     "--bwlimit=400M",
  #     batch_folder,
  #     network_storage_dir
  #   ),
  #   stdout = FALSE,
  #   stderr = FALSE
  # )

  # if (transfer_status == 0) {
  #   cat("  [✓] Batch", current_batch, "successfully transferred to network.\n")

  #   # Note: rsync's '--remove-source-files' deletes the files but leaves the empty
  #   # directory tree intact on the local drive. We use unlink to wipe the empty folders.
  #   unlink(batch_folder, recursive = TRUE)
  # } else {
  #   warning(
  #     "Transfer failed for Batch ",
  #     current_batch,
  #     ". Local files retained."
  #   )
  # }

  # END NETWORK TRANSFER TIMER
  # toc()

  # END OVERALL BATCH TIMER
  toc()
  cat("==========================================\n")
}
