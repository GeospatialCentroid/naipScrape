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
local_working_dir <- "data/processing_batches"
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
plan(multisession, workers = 12) # 12 worker around ~20gb ram usage
#

# Create batches of 50
batch_size <- 50
aoi_table <- aoi_table |>
  mutate(batch_id = ceiling(row_number() / batch_size))

target_years <- c("2012", "2016", "2020")
unique_batches <- unique(aoi_table$batch_id)

for (current_batch in 1:1) {
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
      db_path = db_path
    ),
    .progress = TRUE,
    .options = furrr_options(seed = TRUE)
  )

  # END IMAGE PROCESSING TIMER
  toc()

  # ---------------------------------------------------------
  # 5. ZIPPING & NETWORK TRANSFER
  # ---------------------------------------------------------

  # START NETWORK TRANSFER TIMER
  tic("Zipping and Network Transfer")

  zip_name <- paste0(batch_folder_name, ".zip")
  zip_path_network <- file.path(network_storage_dir, zip_name)

  original_wd <- getwd()
  setwd(local_working_dir)
  zip(zipfile = zip_name, files = batch_folder_name, flags = "-r9Xq")
  setwd(original_wd)

  zip_path_local <- file.path(local_working_dir, zip_name)

  transfer_status <- system2(
    "rsync",
    args = c(
      "-avW",
      "--remove-source-files",
      zip_path_local,
      network_storage_dir
    ),
    stdout = FALSE, # Silences the rsync output in the console
    stderr = FALSE
  )

  if (transfer_status == 0) {
    unlink(batch_folder, recursive = TRUE)
  } else {
    warning(
      "Transfer failed for Batch ",
      current_batch,
      ". Local files retained."
    )
  }

  # END NETWORK TRANSFER TIMER
  toc()

  # END OVERALL BATCH TIMER
  toc()
  cat("==========================================\n")
}
