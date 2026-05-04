getNAIPYear <- function(aoi) {
  # prep aoi object
  bbox <- aoi |>
    sf::st_transform(crs = "EPSG:4326") |>
    sf::st_bbox()

  # Connect to STAC API
  stac_endpoint <- "https://planetarycomputer.microsoft.com/api/stac/v1"
  con <- rstac::stac(stac_endpoint)

  # --- NEW PAUSE & RETRY LOGIC ---
  max_retries <- 3
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
          message(sprintf(
            "STAC API 'text/plain' Error. Waiting 10 seconds to retry (Attempt %d of %d)...",
            retry_count,
            max_retries
          ))
          Sys.sleep(10)
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

# aoi <- getAOI(grid100 = grid100, id = "1415-3-12-4-1")
# year <- "2020"
# exportFolder <- "temp/mp_testing/"
downloadNAIP_vsi <- function(aoi, year, exportFolder) {
  Sys.setenv(GDAL_HTTP_RETRY = "YES")
  Sys.setenv(GDAL_HTTP_MAX_RETRIES = "4")
  # buffer to 2000m
  aoi_buffered <- aoi |>
    sf::st_buffer(dist = 500)
  # st_bbox(aoi_buffered),
  # Create the Lat/Lon bbox for the STAC search
  bbox_4326 <- aoi_buffered |>
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

  # Ensure export directory exists
  if (!dir.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  # 4. Process each intersecting tile
  for (i in seq_along(image_urls)) {
    # Prepend the VSI curl prefix for remote reading
    vsi_path <- paste0("/vsicurl/", image_urls[i])

    # Open the remote raster (this only reads the header, no download yet)
    remote_rast <- terra::rast(vsi_path)

    # Project our AOI to match the NAIP tile's CRS (usually UTM)
    aoi_proj <- sf::st_transform(aoi_buffered, crs = terra::crs(remote_rast))

    # Define output filename
    out_file <- file.path(
      exportFolder,
      paste0("naip_", year, "_id_", aoi$id[1], "_", i, ".tif")
    )

    # message("Cropping and downloading area from tile ", i, "...")

    # This step ONLY downloads the pixels within the crop extent
    terra::crop(remote_rast, aoi_proj, filename = out_file, overwrite = TRUE)
  }

  # message("Success! Cropped images saved to: ", exportFolder)
}
