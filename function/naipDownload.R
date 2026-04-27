# functions  --------------------------------------------------------------

getNAIPYear <- function(aoi) {
  # prep aoi object
  bbox <- aoi |>
    st_transform(crs = "EPSG:4326") |>
    sf::st_bbox()
  # Connect to STAC API
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  con <- rstac::stac(stac_endpoint)
  # see what comes ups
  message("pulled results from the specific aoi.")
  search_results <- con |>
    rstac::stac_search(
      collections = "naip",
      bbox = bbox,
      limit = 200 # A high limit to get all records
    ) |>
    rstac::get_request() # Execute the search
  if (length(search_results$features) == 0) {
    stop("No NAIP imagery found for the specified AOI.")
  }
  
  # pull dates
  all_datetimes <- rstac::items_datetime(search_results)
  # pull specific year
  all_years_str <- substr(all_datetimes, 1, 4)
  # return only unique values
  available_years <- sort(unique(all_years_str))
  ## 5. Show Results
  message("Query complete.")
  message("Naip is available at the following years")
  print(available_years)
}
downloadNAIP_vsi <- function(aoi, year, exportFolder) {
  
  # buffer to 500m (Note: comment originally said 2000m, keeping your 500m code)
  aoi_buffered <- aoi |> 
    sf::st_buffer(dist = 500)
  
  # Create the Lat/Lon bbox for the STAC search
  bbox_4326 <- aoi_buffered |> 
    sf::st_transform(crs = 4326) |> 
    sf::st_bbox()
  
  # 2. Search Planetary Computer
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  
  search_results <- rstac::stac(stac_endpoint) |>
    rstac::stac_search(
      collections = "naip",
      bbox =  bbox_4326,
      datetime = paste0(year, "-01-01T00:00:00Z/", year, "-12-31T23:59:59Z"),
      limit = 10
    ) |>
    rstac::get_request() |>
    rstac::items_sign(rstac::sign_planetary_computer())
  
  if (length(search_results$features) == 0) {
    stop("No NAIP imagery found for this area/year.")
  }
  
  # 3. Extract Signed URLs (VSI compatible)
  image_urls <- rstac::assets_url(search_results, asset_names = "image")
  
  # Ensure export directory exists
  if (!dir.exists(exportFolder)) dir.create(exportFolder, recursive = TRUE)
  
  # 4. Process each intersecting tile
  for (i in seq_along(image_urls)) {
    
    # Define output filename FIRST so we can check if it exists
    out_file <- file.path(exportFolder, paste0("naip_", year, "_id_", aoi$id[1], "_", i, ".tif"))
    
    # Check if file already exists
    if (file.exists(out_file)) {
      message("File already exists, skipping: ", basename(out_file))
      next # Skips the rest of the loop and moves to the next 'i'
    }
    
    # Prepend the VSI curl prefix for remote reading
    vsi_path <- paste0("/vsicurl/", image_urls[i])
    
    # Open the remote raster (this only reads the header, no download yet)
    remote_rast <- terra::rast(vsi_path)
    
    # Project our AOI to match the NAIP tile's CRS (usually UTM)
    aoi_proj <- sf::st_transform(aoi_buffered, crs = terra::crs(remote_rast))
    
    message("Cropping and downloading area from tile ", i, "...")
    
    # This step ONLY downloads the pixels within the crop extent
    terra::crop(remote_rast, aoi_proj, filename = out_file, overwrite = TRUE)
  }
  
  message("Success! Cropped images saved to: ", exportFolder)
}