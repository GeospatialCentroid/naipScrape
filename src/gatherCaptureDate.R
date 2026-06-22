# ---------------------------------------------------------
# OPTIMIZED PARALLEL NAIP CAPTURE DATE EXTRACTION 
# (WITH CACHING, BATCHING & ERROR HANDLING)
# ---------------------------------------------------------
pacman::p_load(dplyr, sf, rstac, tictoc, tidyr, future, furrr, purrr, tools, readr)

# source external functions (Ensure getAOI is loaded)
lapply(list.files(path = "function", pattern = ".R", full.names = TRUE), source)

# --- SET UP GLOBAL PARALLEL BACKEND ---
workers <- max(1, future::availableCores() - 10)
future::plan(future::multisession, workers = workers)

# ---------------------------------------------------------
# 1. DATA PREPARATION & GLOBAL SPATIAL OPERATIONS
# ---------------------------------------------------------
# Load the grid in its native projection (AEA) as required by getAOI()
g100_aea <- sf::st_read("data/grid100km_aea.gpkg", quiet = TRUE)

# Define input path and load target grids
grid_file_path <- "data/LRR_sampleGrids/selectedSample_lrr_F_05_2026.csv"
grids <- readr::read_csv(grid_file_path, show_col_types = FALSE)

# Extract base name to dynamically name both output files
grid_base_name <- tools::file_path_sans_ext(basename(grid_file_path))

groundTruth <- FALSE

# Define unique IDs globally so the bounding box generator always has access
unique_ids <- unique(grids$id)

if(groundTruth){
  tasks <- grids
} else {
  # Generate the initial task list
  target_years <- c("2012", "2016", "2020")
  tasks <- expand.grid(id = unique_ids, year = target_years, stringsAsFactors = FALSE)
}

# --- PRE-CALCULATE OR LOAD BOUNDING BOXES ---
cat("\n--- Checking for Cached Bounding Boxes ---\n")

bbox_cache_file <- file.path("data", "metadata", paste0("bboxes_", grid_base_name, ".csv"))

if (file.exists(bbox_cache_file)) {
  cat(sprintf("Loading cached bounding boxes from: %s\n", bbox_cache_file))
  bboxes <- readr::read_csv(bbox_cache_file, show_col_types = FALSE)
} else {
  cat("No cache found. Pre-calculating AOI Bounding Boxes in parallel (this may take a while)...\n")
  
  # Utilize furrr to distribute the spatial operations across available cores
  bboxes <- furrr::future_map_dfr(unique_ids, function(current_id) {
    
    # 1. Generate AOI using native AEA grid
    aoi_aea <- getAOI(grid100 = g100_aea, id = current_id)
    
    # 2. Transform the resulting specific AOI to EPSG:4326 for STAC compatibility
    aoi_4326 <- sf::st_transform(aoi_aea, crs = 4326)
    
    # 3. Extract bounding box coordinates
    bbox <- sf::st_bbox(aoi_4326)
    
    # Return as a single-row data frame
    data.frame(
      id = current_id,
      xmin = as.numeric(bbox["xmin"]),
      ymin = as.numeric(bbox["ymin"]),
      xmax = as.numeric(bbox["xmax"]),
      ymax = as.numeric(bbox["ymax"]),
      stringsAsFactors = FALSE
    )
  }, .options = furrr::furrr_options(seed = TRUE))
  
  # Save the calculated bounding boxes to the metadata folder for future use
  dir.create(dirname(bbox_cache_file), showWarnings = FALSE, recursive = TRUE)
  write.csv(bboxes, bbox_cache_file, row.names = FALSE)
  cat(sprintf("Bounding boxes cached successfully to: %s\n", bbox_cache_file))
}

# Join the calculated BBox coordinates back to the task list
tasks <- tasks |>
  left_join(bboxes, by = "id")

# ---------------------------------------------------------
# 2. CORE EXTRACTION FUNCTION
# ---------------------------------------------------------
extract_dates_worker <- function(current_id, target_year, xmin, ymin, xmax, ymax) {
  
  # Reconstruct the bbox locally
  bbox_4326 <- c(xmin, ymin, xmax, ymax)
  names(bbox_4326) <- c("xmin", "ymin", "xmax", "ymax")
  
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  con <- rstac::stac(stac_endpoint)
  
  max_retries <- 10
  retry_count <- 0
  request_success <- FALSE
  search_results <- NULL
  
  while (!request_success && retry_count < max_retries) {
    tryCatch({
      search_results <- con |>
        rstac::stac_search(
          collections = "naip",
          bbox = bbox_4326,
          limit = 500 
        ) |>
        rstac::get_request()
      
      request_success <- TRUE 
    }, error = function(e) {
      retry_count <<- retry_count + 1
      if (retry_count < max_retries) {
        wait_time <- (10 * retry_count) + runif(1, 0, 5) 
        Sys.sleep(wait_time)
      } else {
        warning(sprintf("STAC API failed for ID %s after %d attempts.", current_id, max_retries))
      }
    })
  }
  
  if (!request_success || length(search_results$features) == 0) {
    return(data.frame(
      id = current_id, target_year = target_year, actual_year = NA, 
      capture_date = NA, status = "Failed - No Imagery Found (Any Year) / API Error", 
      stringsAsFactors = FALSE
    ))
  }
  
  # --- IN-MEMORY FALLBACK LOGIC ---
  all_datetimes <- rstac::items_datetime(search_results)
  all_dates <- as.Date(substr(all_datetimes, 1, 10))
  available_years <- unique(format(all_dates, "%Y"))
  
  target_num <- as.numeric(target_year)
  preferred_years <- as.character(c(
    target_num,     
    target_num - 1, 
    target_num - 2, 
    target_num + 1  
  ))
  
  actual_year <- NULL
  for (test_year in preferred_years) {
    if (test_year %in% available_years) {
      actual_year <- test_year
      break 
    }
  }
  
  if (is.null(actual_year)) {
    return(data.frame(
      id = current_id, target_year = target_year, actual_year = NA, 
      capture_date = NA, status = "Failed - No imagery in fallback range", 
      stringsAsFactors = FALSE
    ))
  }
  
  target_dates <- all_dates[format(all_dates, "%Y") == actual_year]
  unique_dates <- sort(unique(target_dates))
  
  return(data.frame(
    id = current_id,
    target_year = target_year,
    actual_year = actual_year,
    capture_date = unique_dates,
    status = "Success",
    stringsAsFactors = FALSE
  ))
}

# --- SAFETY WRAPPER ---
error_fallback <- function(current_id, target_year, ...) {
  data.frame(
    id = current_id,
    target_year = target_year,
    actual_year = NA,
    capture_date = NA,
    status = "Catastrophic Worker Failure",
    stringsAsFactors = FALSE
  )
}

safe_extract_worker <- purrr::possibly(
  .f = extract_dates_worker,
  otherwise = error_fallback,
  quiet = FALSE 
)

# ---------------------------------------------------------
# 3. PARALLEL EXECUTION (CHUNKED)
# ---------------------------------------------------------
out_dir <- "data/metadata"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_file <- file.path(out_dir, paste0("capture_dates_", grid_base_name, ".csv"))

chunk_size <- 200
task_chunks <- split(tasks, ceiling(seq_len(nrow(tasks)) / chunk_size))

cat(sprintf("\n--- Starting Optimized Parallel Date Extraction (%d chunks, %d total tasks) ---\n", length(task_chunks), nrow(tasks)))
tic("Total Extraction Time")

for (i in seq_along(task_chunks)) {
  current_chunk <- task_chunks[[i]]
  cat(sprintf("Processing chunk %d of %d...\n", i, length(task_chunks)))
  
  chunk_results <- furrr::future_pmap_dfr(
    list(
      current_id = current_chunk$id,
      target_year = current_chunk$year,
      xmin = current_chunk$xmin,
      ymin = current_chunk$ymin,
      xmax = current_chunk$xmax,
      ymax = current_chunk$ymax
    ),
    .f = safe_extract_worker,
    .options = furrr::furrr_options(seed = TRUE) 
  )
  
  # Append results to the CSV (Write headers only on the first chunk)
  write.table(
    chunk_results, 
    file = out_file, 
    append = (i > 1), 
    sep = ",", 
    row.names = FALSE, 
    col.names = (i == 1) 
  )
  
  # Clear memory
  gc(reset = TRUE, full = TRUE)
}

# Safely close the parallel backend
future::plan(future::sequential)

toc()

# ---------------------------------------------------------
# 4. EXPORT SUMMARY
# ---------------------------------------------------------
cat(sprintf("\nExtraction complete. Results saved to: %s\n", out_file))

# Read the full compiled dataset back into memory to print the summary
final_df <- readr::read_csv(out_file, show_col_types = FALSE)
print(table(final_df$status))