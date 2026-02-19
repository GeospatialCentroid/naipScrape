###
# download NAIP for a location then generate the SNIC objects for validation
###

# libraries
pacman::p_load(
  rstac,
  snic,
  sf,
  terra,
  dplyr,
  tmap,
  rlang,
  httr,
  tictoc,
  purrr,
  furrr,
  tidyr
)
tmap::tmap_mode("view")

source("functions/naipScrape.R") # only works for 2016 and 2020
source("functions/snicParameters.R") # snic processing
source("functions/compileAndExportSNICResults.R") # snic processing

# required inputs  --------------------------------------------------------
grid100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")
# qtm(grid100)
# 1478 - nw NE
# 1351 - se NE

# need to pull some 1k grids from the 2mile areas
grid2 <- sf::st_read("data/derived/grids/two_sq_grid.gpkg")
# subgrids_2010 <- c(13860, 12560, 17182, 22744, 23045, 2510, 6465)
# subgrids_2016 <- c(30823, 6621)
# subgrids_2020 <- c(17663, 10625, 24675)

mlra <- sf::st_read("data/raw/mlra/MLRA_52_2022/MLRA_52.shp")
# working with 76 for next set of values 
mlra76 <- mlra[mlra$MLRA_ID == "76", ]
sf::st_area(mlra76)
qtm(mlra76)

# generate random points within the mlra objec 
samplePoints <- st_sample(mlra76, size = 15, type = "random")
# parse out lat long 
pointsDF <- samplePoints |> 
  st_as_sf() |>
  mutate(
    lon = st_coordinates(samplePoints)[,1],
    lat = st_coordinates(samplePoints)[,2],
    year = c(rep("2016", 7), rep("2020", 8))
  )
m76_16 <- pointsDF[pointsDF$year == "2016", ]
m76_20 <- pointsDF[pointsDF$year == "2020", ]

# do a random selection of grid2 features
random_rows <- grid2[sample(nrow(grid2), 16), ]
random_ids <- random_rows$FID_two_grid

#
grids16 <- random_ids[1:8]
grids20 <- random_ids[9:16]

get_grid_centroid <- function(target_id, data) {
  # 1. Filter for the specific ID
  feature <- data %>%
    filter(FID_two_grid == target_id)

  # 2. Safety check: ensure the feature exists
  if (nrow(feature) == 0) {
    stop("ID not found in the dataset.")
  }

  # 3. Calculate Centroid
  # Note: st_centroid works on s2 (spherical) geometry by default in modern sf versions
  centroid_geometry <- st_centroid(st_geometry(feature))

  # 4. Extract Coordinates (returns a matrix)
  coords <- st_coordinates(centroid_geometry)

  # 5. Format output
  return(data.frame(
    ID = target_id,
    Lon = coords[1, "X"],
    Lat = coords[1, "Y"]
  ))
}

# gather data from sources
p20 <- purrr::map_dfr(.x = subgrids_2020, .f = get_grid_centroid, data = grid2)
p16 <- purrr::map_dfr(.x = subgrids_2016, .f = get_grid_centroid, data = grid2)
p10 <- purrr::map_dfr(.x = subgrids_2010, .f = get_grid_centroid, data = grid2)

g16 <- purrr::map_dfr(.x = grids16, .f = get_grid_centroid, data = grid2)
g20 <- purrr::map_dfr(.x = grids20, .f = get_grid_centroid, data = grid2)
# download and process NAIP image ----------------------------------------------------------------
process_naip_snic <- function(
  year,
  lat,
  lon,
  grid100,
  export_base = "data/derived"
) {
  # 1. Setup paths and AOI
  point <- c(lon, lat)
  aoi <- getAOI(grid100 = grid100, point = point)
  gridID <- aoi$id

  # Define directory structure
  naip_dir <- file.path(export_base, "naipExports")
  snic_dir <- file.path(export_base, "snicExports")
  temp_download_dir <- "naip_grids_1km"

  # Ensure directories exist
  if (!dir.exists(naip_dir)) {
    dir.create(naip_dir, recursive = TRUE)
  }
  if (!dir.exists(snic_dir)) {
    dir.create(snic_dir, recursive = TRUE)
  }

  out_path <- file.path(
    naip_dir,
    paste0("naip_", year, "_id_", gridID, "_wgs84.tif")
  )

  # 2. Download and Merge if file doesn't exist
  if (!file.exists(out_path)) {
    message(paste(
      "--- Downloading & Merging Grid:",
      gridID,
      "Year:",
      year,
      "---"
    ))

    downloadNAIP(aoi = aoi, year = year, exportFolder = temp_download_dir)

    files <- list.files(
      temp_download_dir,
      pattern = paste0(year, "_id_", gridID),
      full.names = TRUE
    )

    if (length(files) == 0) {
      stop(paste("No NAIP files found for ID:", gridID, "in year:", year))
    }

    mergeAndExport(files = files, out_path = out_path, aoi = aoi)
  } else {
    message(paste(
      "--- Existing TIF found for Grid:",
      gridID,
      ". Skipping Download. ---"
    ))
  }

  # 3. SNIC Processing
  message(paste("--- Generating SNIC Segmentation for:", gridID, "---"))
  r1 <- terra::rast(out_path)

  # Generate seeds (lat/lon spacing)
  seeds <- generate_scaled_seeds(r = r1)

  # Process segmentations
  process_segmentations(
    r = r1,
    seed_list = seeds,
    output_dir = snic_dir,
    year = year,
    file_id = gridID
  )

  # 4. Final Bundle
  message(paste("--- Bundling Final Data for ID:", gridID, "---"))
  bundle_and_export(grid_id = gridID, year = year)

  return(invisible(out_path))
}


# download and process NAIP image ----------------------------------------------------------------
## probably best to make this a function accepting either a point or a grid id for better integration into
## the snic workflow

# naip imagery for MLRA 76 --- just assumed that 2016 would have imagery 
for (i in 1:nrow(m76_16)) {
  process_naip_snic(
    year = 2016,
    lat = m76_16$lat[i],
    lon = m76_16$lon[i],
    grid100 = grid100
  )
}
for (i in 1:nrow(m76_20)) {
  process_naip_snic(
    year = 2020,
    lat = m76_20$lat[i],
    lon = m76_20$lon[i],
    grid100 = grid100
  )
}


## sites within validation imagery
for (i in 1:nrow(p20)) {
  process_naip_snic(
    year = 2020,
    lat = p20$Lat[i],
    lon = p20$Lon[i],
    grid100 = grid100
  )
}
#
for (i in 1:nrow(p16)) {
  process_naip_snic(
    year = 2016,
    lat = p16$Lat[i],
    lon = p16$Lon[i],
    grid100 = grid100
  )
}

# random selection 2016
for (i in 1:nrow(g16)) {
  process_naip_snic(
    year = 2016,
    lat = g16$Lat[i],
    lon = g16$Lon[i],
    grid100 = grid100
  )
}
# random selection 2020
for (i in 1:nrow(g20)) {
  process_naip_snic(
    year = 2020,
    lat = g20$Lat[i],
    lon = g20$Lon[i],
    grid100 = grid100
  )
}


# one off example for visualization
gridID <- "1415-3-12-4-1"
image <- terra::rast(
  "data/ready_for_export/grid_1415-3-12-4-1_2020_bundle/naip_2020_id_1415-3-12-4-1_wgs84.tif"
)
s40 <- terra::rast(
  "data/ready_for_export/grid_1415-3-12-4-1_2020_bundle/naip_2020_id_1415-3-12-4-1_wgs84.tif"
)
s10 <- image <- terra::rast(
  "data/ready_for_export/grid_1415-3-12-4-1_2020_bundle/naip_2020_id_1415-3-12-4-1_wgs84.tif"
)

seedList <- generate_scaled_seeds(r = image)
naip <- process_segmentations(
  r = image,
  seed_list = seedList,
  output_dir = "temp",
  file_id = gridID,
  year = "2020"
)

inspect_seed_density(r = image, seed_list = seedList, "s40")
inspect_seed_density(r = image, seed_list = seedList, "s10")
