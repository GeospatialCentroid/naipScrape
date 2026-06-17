# =============================================================
# LOCAL DOWNLOAD TEST SCRIPT
# Tests the core download + packaging pipeline with no network
# drive, TailScale, SQLite, or rsync involved.
#
# Prerequisites:
#   - data/grid100km_aea.gpkg must exist (same as production)
#   - Packages: pacman, dplyr, sf, terra, tictoc, rstac, purrr
#
# Outputs land in data/test_output/ and are safe to delete.
# =============================================================

pacman::p_load(dplyr, sf, terra, tictoc, rstac, purrr, stringr)

# Source only the three files this test needs.
# Deliberately excludes getSQL.R (RSQLite), process_aoi.R (DBI/R.utils),
# and get_env_config.R (yaml/config.yml) — none are used here.
source("function/generateAOI.R")
source("function/naipDownload.R")
source("function/postDownloadFunctions.R")

# -------------------------------------------------------------
# TEST PARAMETERS — change these to iterate
# -------------------------------------------------------------

# Grid cell ID from grid100km_aea.gpkg (five-part hex string, e.g. "a-3-2-1-c")
TEST_GRID_ID <- "a-3-2-1-c"

# Year to request; the pipeline will fall back ±2 years if needed
TEST_YEAR <- "2020"

# Buffer applied to the 1km AOI cell (metres). 250 → 1.5km total
TEST_BUFFER_M <- 250

# -------------------------------------------------------------
# 1. SANITY CHECK — data dependencies
# -------------------------------------------------------------
grid_path <- "data/grid100km_aea.gpkg"

if (!file.exists(grid_path)) {
  stop(
    "Required file not found: ", grid_path, "\n",
    "Copy it from the production data directory before running this test."
  )
}

# -------------------------------------------------------------
# 2. OUTPUT DIRECTORIES (local only, safe to wipe)
# -------------------------------------------------------------
test_root <- "data/test_output"
temp_dir  <- file.path(test_root, "tiles_raw")
out_dir   <- file.path(test_root, "naip_merged")

dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

cat("\n==============================================\n")
cat("NAIP LOCAL DOWNLOAD TEST\n")
cat("Grid ID:", TEST_GRID_ID, "\n")
cat("Year   :", TEST_YEAR, "\n")
cat("Buffer :", TEST_BUFFER_M, "m\n")
cat("Output :", out_dir, "\n")
cat("==============================================\n\n")

# -------------------------------------------------------------
# 3. RESOLVE AOI
# -------------------------------------------------------------
tic("Total test runtime")

cat("[1/5] Loading grid and resolving AOI...\n")
g100 <- sf::st_read(grid_path, quiet = TRUE)
aoi  <- getAOI(grid100 = g100, id = TEST_GRID_ID)
id   <- aoi$id

# -------------------------------------------------------------
# 4. CHECK AVAILABLE YEARS VIA STAC
# -------------------------------------------------------------
cat("[2/5] Querying Planetary Computer for available years...\n")
years_available <- tryCatch(
  getNAIPYear(aoi),
  error = function(e) {
    stop("STAC API error — check your internet connection.\n  ", e$message)
  }
)
cat("      Available years:", paste(years_available, collapse = ", "), "\n")

# Replicate production fallback logic
target_num      <- as.numeric(TEST_YEAR)
preferred_years <- as.character(c(target_num, target_num - 1, target_num - 2, target_num + 1))
actual_year     <- preferred_years[preferred_years %in% years_available][1]

if (is.null(actual_year) || is.na(actual_year)) {
  stop("No imagery found for ", TEST_YEAR, " or any fallback year within ±2.")
}
cat("      Using year:", actual_year, "\n")

# -------------------------------------------------------------
# 5. DOWNLOAD RAW TILES
# -------------------------------------------------------------
cat("[3/5] Downloading raw tiles to", temp_dir, "...\n")
tic("Download")
tile_meta <- downloadNAIP_vsi(
  aoi          = aoi,
  year         = actual_year,
  exportFolder = temp_dir,
  buffer_m     = TEST_BUFFER_M
)
toc()

naip_pattern <- paste0("^naip_", actual_year, "_id_", id, "_[0-9]+\\.tif$")
raw_tiles    <- list.files(temp_dir, pattern = naip_pattern, full.names = TRUE)
cat("      Tiles downloaded:", length(raw_tiles), "\n")

if (length(raw_tiles) == 0) {
  stop("Download appeared to succeed but no tile files were found matching the expected pattern.")
}

# Save collection metadata CSV
meta_path <- file.path(out_dir, paste0("collection_meta_", id, "_", actual_year, ".csv"))
write.csv(tile_meta, meta_path, row.names = FALSE)
cat("      Metadata CSV saved:", basename(meta_path), "\n")

# -------------------------------------------------------------
# 6. MERGE, EXPORT, AND EMBED STATS
# -------------------------------------------------------------
cat("[4/5] Merging tiles and exporting...\n")
tic("Merge + export")
mergeAndExportNAIP(
  files     = raw_tiles,
  out_path  = out_dir,
  aoi       = aoi,
  year      = actual_year,
  buffer_m  = TEST_BUFFER_M,
  buffer_only = FALSE       # produce both the buffered and the 1km crop
)
toc()

# Clean up raw tiles
file.remove(raw_tiles)
terra::tmpFiles(remove = TRUE)
gc(reset = TRUE, full = TRUE)

# -------------------------------------------------------------
# 7. VALIDATION CHECKS
# -------------------------------------------------------------
cat("[5/5] Validating outputs...\n\n")

total_width_km  <- (1000 + (2 * TEST_BUFFER_M)) / 1000
label_km        <- paste0(total_width_km, "km")
expected_buf    <- file.path(out_dir, paste0("naip_", label_km,  "_", id, "_", actual_year, ".tif"))
expected_1km    <- file.path(out_dir, paste0("naip_1km_", id, "_", actual_year, ".tif"))
expected_stats_buf  <- paste0(expected_buf, ".aux.xml")
expected_stats_1km  <- paste0(expected_1km, ".aux.xml")

checks <- list(
  "Buffered raster exists"       = file.exists(expected_buf),
  "1km raster exists"            = file.exists(expected_1km),
  "Buffered stats (.aux.xml)"    = file.exists(expected_stats_buf),
  "1km stats (.aux.xml)"         = file.exists(expected_stats_1km),
  "Collection metadata CSV"      = file.exists(meta_path)
)

all_passed <- TRUE
for (label in names(checks)) {
  ok <- checks[[label]]
  cat(sprintf("  [%s] %s\n", ifelse(ok, "PASS", "FAIL"), label))
  if (!ok) all_passed <- FALSE
}

# Quick band/value sanity check on the buffered raster
if (file.exists(expected_buf)) {
  r      <- terra::rast(expected_buf)
  n_band <- terra::nlyr(r)
  mins   <- terra::global(r, "min",  na.rm = TRUE)$min
  maxs   <- terra::global(r, "max",  na.rm = TRUE)$max

  band_ok  <- n_band == 4
  range_ok <- all(mins >= 0) && all(maxs <= 255)

  cat(sprintf("  [%s] Raster has 4 bands (R,G,B,NIR) — found %d\n",
              ifelse(band_ok,  "PASS", "FAIL"), n_band))
  cat(sprintf("  [%s] Pixel values in valid INT1U range (0–255)\n",
              ifelse(range_ok, "PASS", "FAIL")))
  cat(sprintf("       Per-band min: %s\n", paste(round(mins, 1), collapse = ", ")))
  cat(sprintf("       Per-band max: %s\n", paste(round(maxs, 1), collapse = ", ")))

  if (!band_ok || !range_ok) all_passed <- FALSE
}

# Print collection metadata preview
if (file.exists(meta_path)) {
  meta <- read.csv(meta_path)
  cat("\n  Collection metadata preview:\n")
  print(meta[, c("tile_index", "collection_date", "naip_state", "item_id")])
}

toc()
cat("\n==============================================\n")
cat(ifelse(all_passed, "ALL CHECKS PASSED", "ONE OR MORE CHECKS FAILED"), "\n")
cat("Output directory:", normalizePath(out_dir), "\n")
cat("==============================================\n")
