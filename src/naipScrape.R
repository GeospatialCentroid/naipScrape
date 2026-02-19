###
# over all goal; a standalone workflow that can be used to download NAIP imagery from mircosoft planetary computer for a set AOI

# libraries
pacman::p_load(rstac, sf, terra, dplyr, tmap, rlang, httr, tictoc, purrr, furrr)
tmap::tmap_mode("view")

source("functions/naipScrape.R")

# required inputs  --------------------------------------------------------
grid100 <- sf::st_read("data/derived/grids/grid100km_aea.gpkg")
# test the 2mile grid
# Validated_X12-624_26060_2020_dealiased
twoMile <- sf::st_read("data/products/modelGrids/two_sq_grid.gpkg")

# regenerating 2 mile grids  ---------------------------------------------
# establish table of naip dates and grid elements
n20 <- c(19763)
# store as df
df <- data.frame(
  year = 2020,
  gridID = n20
)

process_naip_2mile <- function(
  year,
  gridID,
  twoMilePath = "data/products/modelGrids/two_sq_grid.gpkg"
) {
  # Create a specific temp sub-folder
  temp_dir <- file.path("temp", paste0(year, "_", gridID))
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  # set aoi
  ## readin in within function to help with paralization
  twoMile <- sf::st_read(twoMilePath)
  aoi <- twoMile[twoMile$FID_two_grid == gridID, ] |>
    dplyr::select(id = FID_two_grid)

  # 3. Check NAIP availability
  naipYears <- getNAIPYear(aoi = aoi)

  if (year %in% naipYears) {
    message(paste("Processing Grid:", gridID, "Year:", year))
    out_path <- paste0(
      "data/derived/naipExports/naip_",
      year,
      "_id_",
      gridID,
      "_wgs84.tif"
    )
    if (!file.exists(out_path)) {
      # 4. Download
      downloadNAIP(aoi = aoi, year = year, exportFolder = temp_dir)
      # grab files
      files <- list.files(
        temp_dir,
        pattern = paste0(year, "_id_", gridID),
        full.names = TRUE
      )
      # process imagery
      mergeAndExport(files = files, out_path = out_path, aoi = aoi)
    }
  } else {
    warning(paste("Year", year, "not found for Grid", gridID))
  }

  # 9. Cleanup
  unlink(temp_dir, recursive = TRUE)
}

# Run the process for each year and grid ID
# purrr::pwalk(
#   df,
#   .f = process_naip_2mile
# )

future::plan(multisession, workers = 8)

furrr::future_pwalk(
  .l = df,
  .f = process_naip_2mile,
  .options = furrr::furrr_options(seed = TRUE)
)

# workflow ----------------------------------------------------------------
point <- c(-99.55915251753592,40.24362217062013)
aoi <- getAOI(grid100 = grid100, point = point)
qtm(aoi)
# 
# test for year
getNAIPYear(aoi = aoi)

# set year 
year <- "2020"
exportFolder <- "temp"
gridID <- aoi$id

# # download naip
downloadNAIP(aoi = aoi, year = 2020, exportFolder = exportFolder)
# files 
files <- list.files(
  "temp",
  pattern = paste0(year, "_id_", gridID),
  full.names = TRUE
) 

message(paste("Processing Grid:", gridID, "Year:", year))
out_path <- paste0(
  "data/derived/naipExports/naip_",
  year,
  "_id_",
  gridID,
  "_wgs84.tif"
)

mergeAndExport(files = files, out_path =out_path, aoi = aoi )

# #standard
# standardizeNAIP(
#   importPath = "temp/naip_2021_id_1998-3-12-4-4.tif",
#   exportPath = "naip_2021_id_1998-3-12-4-4_wgs84.tif"
# )

# # download naip for the specific aoi for a specific year
# year <- "2021"
# exportFolder <- "temp"

# # test what years are available at the aoi
# getNAIPYear(aoi = aoi)

# # download area
# downloadNAIP(aoi = aoi, year = "2021", exportFolder = "temp")

# importPath <- "temp/naip_2021_id_1344-4-12-f-4.tif"

# # extra for the two mile grid testing  ------------------------------------
# # testing for the two mile
# files <- list.files("temp", pattern = "2020_id__", full.names = TRUE)
# export <- paste0(stringr::str_remove(files, pattern = "\\.tif$"), "_wgs84.tif")
# # only around 10g ram allocation so will work well for furrr distribution
# tic()
# purrr::map2(.x = files, .y = export, .f = standardizeNAIP)
# toc()
# # run time on 4 images : 5.5 minutes

# rasters <- terra::sprc(lapply(export, terra::rast))

# # high memory allocation with merge 20gb
# tic()
# m <- terra::merge(rasters)
# toc()
# # 4 features to 1 in 67 seconds
