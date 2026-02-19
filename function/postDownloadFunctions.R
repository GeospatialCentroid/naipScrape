
mergeAndExportNAIP <- function(files, out_path, aoi) {
  # generates two images 
  ## one with 200m buffer 
  ## one strict the naip aoi 
  
  # buffer to 200 meters
  aoi200 <- sf::st_buffer(aoi, dist = 200)
  
  # get a template rast for CRS information 
  r1 <- terra::rast(files[1])
  # reprojecthe aoi object
  aoi_proj <- terra::project(terra::vect(aoi), crs(r1))
  aoi200_proj <- terra::project(terra::vect(aoi200), crs(r1))
  
  
  # generate a template raster 1 m
  temp <- terra::rast(
    extent = ext(aoi200_proj),
    crs = crs(aoi200_proj),
    nlyrs = 4,
    resolution = 1 # 1 meter
  )
  if (length(files) > 1) {
    rast <- purrr::map(.x = files, .f = readAndName) |>
      terra::sprc() |>
      terra::mosaic(fun = "mean")
  } else {
    rast <- terra::rast(files)
  }
  # crop and resample
  m1 <- terra::crop(rast, aoi200_proj) |>
    terra::resample(
      temp,
      method = "bilinear"
    )
  # crop to 1km area 
  m2 <- terra::crop(rast, aoi_proj) |>
    terra::resample(
      temp,
      method = "bilinear"
    )
  # export data 
  buffExport <- paste0(out_path,"/buffered_",aoi$id, ".tif")
  kmExport <- paste0(out_path,"/oneKM_",aoi$id, ".tif")
  # write out, 
  terra::writeRaster(x = m1, buffExport)
  terra::writeRaster(x = m2, kmExport)
  
}

readAndName <- function(path) {
  r1 <- terra::rast(path)
  names(r1) <- c("red", "green", "blue", "nir")
  return(r1)
}



mergeAndExportLidar<- function(files, out_path, aoi) {
  # generates one image crop to the 1km areas 
  # get a template rast for CRS information 
  r1 <- terra::rast(files[1])
  # reprojecthe aoi object
  aoi_proj <- terra::project(terra::vect(aoi), crs(r1))
  
  # generate a template 
  temp <- terra::rast(
    extent = ext(aoi_proj),
    crs = crs(aoi_proj),
    nlyrs = 1,
    resolution = terra::res(r1)[1] # pull from the lidar image 
  )
  if (length(files) > 1) {
    rast <- files |>
      terra::sprc() |>
      terra::mosaic(fun = "mean")
  } else {
    rast <- terra::rast(files)
  }
  # crop and resample
  m1 <- terra::crop(rast, aoi_proj) |>
    terra::resample(
      temp,
      method = "bilinear"
    )
  # export Path 
  pattern_string <- gsub("_[0-9]+(?=\\.tif$)", "", basename(files[1]), perl = TRUE)
  dsm_Export <- paste0(out_path,"/",pattern_string)
  # write out, 
  terra::writeRaster(x = m1, dsm_Export)
  
}


