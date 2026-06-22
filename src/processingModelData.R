
# 
process_raster_folders <- function(base_dir) {
  # Get a list of all subdirectories within the base directory
  # Based on your screenshot, this would target the folders like "1869-4-16-8-3"
  sub_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  
  for (dir in sub_dirs) {
    # Locate the .gpkg and the 2km .tif file in the current directory
    gpkg_file <- list.files(dir, pattern = "\\.gpkg$", full.names = TRUE)
    tif_file <- list.files(dir, pattern = "_2km_.*\\.tif$", full.names = TRUE)
    
    # Ensure exactly one of each file exists before proceeding to avoid errors
    if (length(gpkg_file) == 1 && length(tif_file) == 1) {
      
      # Use a tryCatch block so one bad file doesn't stop the entire loop
      tryCatch({
        # 1. Read the vector and raster data
        v <- terra::vect(gpkg_file)
        r <- terra::rast(tif_file)
        
        # Ensure Coordinate Reference Systems (CRS) match
        if (terra::crs(v) != terra::crs(r)) {
          v <- terra::project(v, terra::crs(r))
        }
        
        # 2. Crop (reduces extent) and Mask (sets pixels outside polygon to NA)
        r_cropped <- terra::crop(r, v)
        r_masked <- terra::mask(r_cropped, v)
        
        # 3. Generate the new filename by substituting "2km" with "1km"
        new_tif_file <- sub("_2km_", "_1km_", tif_file)
        
        # 4. Write the new raster to disk
        terra::writeRaster(r_masked, new_tif_file, overwrite = TRUE)
        
        # 5. Delete the original 2km raster
        file.remove(tif_file)
        
        message(sprintf("Successfully processed: %s", basename(dir)))
        
      }, error = function(e) {
        warning(sprintf("Error processing directory %s: %s", basename(dir), e$message))
      })
      
    } else {
      # Skip folders that don't have exactly one vector and one matching raster
      warning(sprintf("Skipped %s: Missing target files or multiple matches found.", basename(dir)))
    }
  }
  
  message("Processing complete.")
}

