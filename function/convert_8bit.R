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