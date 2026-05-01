mergeAndExportNAIP <- function(files, out_path, aoi, year, buffer_only = TRUE) {
  # Buffer to 500 meters for a 2x2km area
  aoi500 <- sf::st_buffer(aoi, dist = 500)

  # Get a template rast for CRS information
  r1 <- terra::rast(files[1])
  aoi_proj <- terra::project(terra::vect(aoi), crs(r1))

  # Export the 1km vector geometry so you can reproduce the strict crop later
  terra::writeVector(
    aoi_proj,
    file.path(out_path, paste0("aoi-", aoi$id, ".gpkg")),
    overwrite = TRUE
  )

  aoi500_proj <- terra::project(terra::vect(aoi500), crs(r1))

  # Generate a template raster (1m resolution)
  temp <- terra::rast(
    extent = ext(aoi500_proj),
    crs = crs(aoi500_proj),
    nlyrs = 4,
    resolution = 1
  )

  if (length(files) > 1) {
    rast <- purrr::map(.x = files, .f = readAndName) |>
      terra::sprc() |>
      terra::mosaic(fun = "mean")
  } else {
    rast <- terra::rast(files)
  }

  # Crop, resample, and mask to the 2km buffered area
  m1 <- terra::crop(rast, aoi500_proj) |>
    terra::resample(temp, method = "bilinear") |>
    terra::mask(aoi500_proj)

  # Export 2km data
  export_2km <- file.path(
    out_path,
    paste0("naip_2km_", aoi$id, "_", year, ".tif")
  )
  terra::writeRaster(x = m1, export_2km, overwrite = TRUE)

  # Conditionally process and export the 1km data
  if (!buffer_only) {
    m2 <- terra::crop(m1, aoi_proj) |>
      terra::mask(aoi_proj)

    export_1km <- file.path(
      out_path,
      paste0("naip_1km_", aoi$id, "_", year, ".tif")
    )
    terra::writeRaster(x = m2, export_1km, overwrite = TRUE)
  }
}
readAndName <- function(path) {
  r1 <- terra::rast(path)
  names(r1) <- c("red", "green", "blue", "nir")
  return(r1)
}


mergeAndExportLidar <- function(files, out_path, aoi) {
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
  pattern_string <- gsub(
    "_[0-9]+(?=\\.tif$)",
    "",
    basename(files[1]),
    perl = TRUE
  )
  dsm_Export <- paste0(out_path, "/", pattern_string)
  # write out,
  terra::writeRaster(x = m1, dsm_Export, overwrite = TRUE)
}


# compile data for export
copyToExport <- function(id, year) {
  # Define the source directories to check
  source_dirs <- file.path(
    "data",
    c("aoiExports", "lidarExports", "naipExports", "snicExports")
  )

  # 2. Find all files containing the target ID in the target folders
  files_to_move <- list.files(
    path = source_dirs,
    pattern = id,
    full.names = TRUE
  )

  # 3. Create the destination directory inside exportData
  dest_dir <- paste0("data/exportData/aoi_", id, "_", year)

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  # file copy then remore
  for (i in files_to_move) {
    file.copy(i, to = dest_dir)
    # test for success
    newFile <- file.path(dest_dir, basename(i))
    if (file.exists(newFile)) {
      file.remove(i)
    } else {
      message("file was not successful copied")
      stop()
    }
  }
}
