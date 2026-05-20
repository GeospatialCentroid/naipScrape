getNAIPYear <- function(aoi) {
  # prep aoi object
  bbox <- aoi |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_bbox()

  # Connect to STAC API
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  con <- rstac::stac(stac_endpoint)

  # --- EXPONENTIAL BACKOFF RETRY LOGIC ---
  max_retries <- 10
  retry_count <- 0
  request_success <- FALSE
  search_results <- NULL

  while (!request_success && retry_count < max_retries) {
    tryCatch(
      {
        search_results <- con |>
          rstac::stac_search(
            collections = "naip",
            bbox = bbox,
            limit = 200 # A high limit to get all records
          ) |>
          rstac::get_request() # Execute the search

        request_success <- TRUE # If we get here without an error, it worked!
      },
      error = function(e) {
        retry_count <<- retry_count + 1
        if (retry_count < max_retries) {
          # Progressive wait: 10s, 20s, 30s, 40s...
          wait_time <- 10 * retry_count
          message(sprintf(
            "STAC API Server Overloaded. Waiting %d seconds to retry (Attempt %d of %d)...",
            wait_time,
            retry_count,
            max_retries
          ))
          Sys.sleep(wait_time)
        } else {
          stop(sprintf(
            "STAC API failed after %d attempts. Original error: %s",
            max_retries,
            e$message
          ))
        }
      }
    )
  }
  # -------------------------------

  if (length(search_results$features) == 0) {
    stop("No NAIP imagery found for the specified AOI.")
  }

  # pull dates
  all_datetimes <- rstac::items_datetime(search_results)
  # pull specific year
  all_years_str <- substr(all_datetimes, 1, 4)
  # return only unique values
  available_years <- sort(unique(all_years_str))

  return(available_years)
}

downloadNAIP_vsi <- function(aoi, year, exportFolder, buffer_m = 0) {
  Sys.setenv(GDAL_HTTP_RETRY = "YES")
  Sys.setenv(GDAL_HTTP_MAX_RETRIES = "4")
  
  # --- DYNAMIC BUFFER LOGIC ---
  if (buffer_m > 0) {
    target_aoi <- aoi |>
      sf::st_buffer(dist = buffer_m)
  } else {
    target_aoi <- aoi
  }
  
  # Create the Lat/Lon bbox for the STAC search using the target geometry
  bbox_4326 <- target_aoi |>
    sf::st_transform(crs = 4326) |>
    sf::st_bbox()
  
  # 2. Search Planetary Computer
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  
  search_results <- rstac::stac(stac_endpoint) |>
    rstac::stac_search(
      collections = "naip",
      bbox = bbox_4326,
      datetime = paste0(year, "-01-01T00:00:00Z/", year, "-12-31T23:59:59Z"),
      limit = 100
    ) |>
    rstac::get_request() |>
    rstac::items_sign(rstac::sign_planetary_computer())
  
  if (length(search_results$features) == 0) {
    stop("No NAIP imagery found for this area/year.")
  }
  
  # 3. Extract Signed URLs (VSI compatible)
  image_urls <- rstac::assets_url(search_results, asset_names = "image")
  
  if (!dir.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  # 4. Process each intersecting tile
  for (i in seq_along(image_urls)) {
    vsi_path <- paste0("/vsicurl/", image_urls[i])
    remote_rast <- terra::rast(vsi_path)
    
    aoi_proj <- sf::st_transform(target_aoi, crs = terra::crs(remote_rast))
    
    out_file <- file.path(
      exportFolder,
      paste0("naip_", year, "_id_", aoi$id[1], "_", i, ".tif")
    )
    
    terra::crop(
      x = remote_rast, 
      y = aoi_proj, 
      filename = out_file, 
      datatype = "INT1U", 
      overwrite = TRUE
    )
  }
}