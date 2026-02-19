
download_lidar_dsm <- function(aoi, exportFolder) {
  
  # 1. Prepare AOI (Metric for cropping, Lat/Lon for searching)
  # Check if AOI is not 4326 (Lat/Lon) for the crop step
  if (sf::st_crs(aoi)$wkt == sf::st_crs(4326)$wkt) {
    warning("Input AOI is in Lat/Lon (4326). Projecting to EPSG:5070 for accurate cropping.")
    aoi_metric <- sf::st_transform(aoi, 5070)
  } else {
    aoi_metric <- aoi
  }
  
  bbox_4326 <- aoi_metric |> 
    sf::st_transform(crs = 4326) |> 
    sf::st_bbox()
  
  # 2. Search Planetary Computer (3DEP LiDAR DSM)
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  
  search_results <- rstac::stac(stac_endpoint) |>
    rstac::stac_search(
      collections = "3dep-lidar-dsm", 
      bbox = bbox_4326,
      limit = 10 
    ) |>
    rstac::get_request() |>
    rstac::items_sign(rstac::sign_planetary_computer())
  
  if (length(search_results$features) == 0) {
    message("No 3DEP LiDAR DSM data found for this AOI.")
    return(NULL)
  }
  
  # Ensure export directory exists
  if (!dir.exists(exportFolder)) dir.create(exportFolder, recursive = TRUE)
  
  # 3. Process Each Feature (Tile)
  # We loop through the features directly to keep metadata (date) and URL synced
  for (i in seq_along(search_results$features)) {
    
    item <- search_results$features[[i]]
    
    # --- EXTRACT YEAR METADATA ---
    # The datetime format is ISO 8601 (e.g., "2020-04-21T00:00:00Z")
    # We strip the first 4 characters to get the year.
    item_datetime <- item$properties$start_datetime
    
    # Fallback if datetime is null (rare, but good safety)
    if (is.null(item_datetime)) {
      item_year <- "unknown"
    } else {
      item_year <- substr(item_datetime, 1, 4)
    }
    
    # Extract the signed asset URL for the 'data' asset
    asset_url <- item$assets$data$href
    
    # --- CROP AND SAVE ---
    vsi_path <- paste0("/vsicurl/", asset_url)
    remote_rast <- terra::rast(vsi_path)
    
    # Transform AOI to match tile's projection
    aoi_proj <- sf::st_transform(aoi_metric, crs = terra::crs(remote_rast))
    
    # Construct filename WITH THE YEAR
    # Naming convention: lidar_dsm_[YEAR]_id_[ID]_[INDEX].tif
    file_name <- paste0("lidar_dsm_", item_year, "_id_", aoi$id[1], "_", i, ".tif")
    out_file <- file.path(exportFolder, file_name)
    
    message("Processing LiDAR tile ", i, " from year ", item_year, "...")
    
    tryCatch({
      terra::crop(remote_rast, aoi_proj, filename = out_file, overwrite = TRUE)
      message("Saved: ", out_file)
    }, error = function(e) {
      message("Skipping tile ", i, " (likely no data overlap in crop area).")
    })
  }
}

