# quick test of segmentation work on the drone imagery 
pacman::p_load(terra)

# calibration files 
files <- list.files("~/trueNAS/work/wheat_drone/DJI_202506251414_063", 
                    full.names = TRUE)
t1 <- terra::rast(files[8])
terra::plot(t1)

# captured imagery 
files2 <- list.files(path = "~/trueNAS/work/wheat_drone/DJI_202506251414_064_FC25-300-0625-MS",
                     full.names = TRUE)
t2 <- terra::rast(files2[432])
terra::plot(t2)
terra::writeRaster(t2, "droneExample.tif")


process_dji_multispectral <- function(files, image_id) {
  
  # 1. List all TIF files for the specific image ID
  # This avoids the .JPG file which usually has different dimensions
  pattern <- paste0(image_id, ".*\\.TIF$")
  sel_files <- files[grepl(pattern = pattern,x = files)]
  
  if (length(sel_files) == 0) {
    stop("No TIF files found for the provided Image ID.")
  }
  
  # 2. Create the SpatRaster
  # terra::rast() automatically creates a multi-layered object from a vector
  img_stack <- terra::rast(sel_files)
  
  # 3. Rename bands based on the file suffixes
  # DJI typically uses _G, _R, _RE, _NIR
  layer_names <- names(img_stack)
  new_names <- gsub(".*_MS_(.*)$", "\\1", layer_names)
  names(img_stack) <- new_names
  
  # 4. Calculate Indices
  # NDVI = (NIR - Red) / (NIR + Red)
  # NDRE = (NIR - RedEdge) / (NIR + RedEdge)
  
  # Note: Use [[]] to select layers by the names we just assigned
  img_stack$NDVI <- (img_stack[["NIR"]] - img_stack[["R"]]) / (img_stack[["NIR"]] + img_stack[["R"]])
  img_stack$NDRE <- (img_stack[["NIR"]] - img_stack[["RE"]]) / (img_stack[["NIR"]] + img_stack[["RE"]])
  
  return(img_stack)
}

# --- Usage Example ---
files <- files2
my_rast <- process_dji_multispectral(files2, "_0086_")
plot(my_rast[["NDRE"]], main = "NDRE for Image 0117")
# export image 

terra::writeRaster(my_rast, "test_sixBand_0086.tif")

# run a segementation 
source("~/trueNAS/work/naipScrape/function/snicElements.R") # only works for 2016 and 2020

# generate seeds 
t3 <- prepRastForSNIC(my_rast)

seeds <- generate_scaled_seeds(t3)

process_segmentations(r = t3, seed_list = seeds, output_dir = getwd(), file_id = "testDrone_6band", year = "2025")
inspect_seed_density(r = t3,seed_list = seeds, seed_name = "s100" )
