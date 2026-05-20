pacman::p_load(
  terra,
  furrr,
  future,
  dplyr,
  tictoc
)

# ---------------------------------------------------------
# 1. SETUP
# ---------------------------------------------------------
# Define the root directory containing all the naip_batch_X folders
naip_root_dir <- "~/trueNAS/work/naipScrape/mnt/fileShare/NAIP"

# folders 
folders <- list.dirs(path = naip_root_dir, 
                     full.names = TRUE, 
                     recursive = FALSE)

# Recursively find all TIFF files in the structure
all_tifs <- list.files(
  path = naip_root_dir,
  pattern = "\\.tif$",
  recursive = TRUE,
  full.names = TRUE
)

cat(sprintf("Found %d total TIFF files to evaluate.\n", length(all_tifs)))

# ---------------------------------------------------------
# 2. CONVERSION FUNCTION (PATCHED)
# ---------------------------------------------------------
convert_to_8bit_inplace <- function(file_path) {
  
  # Prevent GDAL Error 4 by verifying the file still exists 
  # before terra tries to open it.
  if (!file.exists(file_path)) {
    return("Skipped: File missing or inaccessible")
  }
  
  tryCatch({
    r <- rast(file_path)
    dtypes <- datatype(r)
    
    # Check if any layer is 32-bit (FLT4S, INT4S) or 64-bit (FLT8S, INT8S)
    if (any(grepl("4S|8S", dtypes))) {
      
      tmp_file <- paste0(file_path, ".tmp.tif")
      
      writeRaster(r, tmp_file, datatype = "INT1U", overwrite = TRUE)
      
      rm(r)
      gc() 
      
      rename_status <- file.rename(from = tmp_file, to = file_path)
      
      if(rename_status) {
        return("Converted")
      } else {
        return("Error: Failed to rename temp file")
      }
      
    } else {
      return("Already 8-bit")
    }
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}
# ---------------------------------------------------------
# 3. EXECUTION
# ---------------------------------------------------------
# Set up parallel workers. Adjust 'workers' based on your environment.
plan(multisession, workers = 10 )

cat("\nStarting bulk conversion process...\n")
tic("Total Bulk 8-bit Conversion Time")

# Map the function across all discovered TIFFs
conversion_results <- future_map_chr(
  all_tifs,
  convert_to_8bit_inplace,
  .progress = TRUE,
  .options = furrr_options(seed = TRUE)
)

toc()

# ---------------------------------------------------------
# 4. SUMMARY
# ---------------------------------------------------------
cat("\nConversion Summary:\n")
print(table(conversion_results))