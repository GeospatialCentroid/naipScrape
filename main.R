# This is a generalized method for downloading material from the planetary computer 

pacman::p_load(dplyr, sf, terra, tidyr)
# testing 
library(tmap)
tmap_mode(mode = "view")
# source files 
lapply(list.files(path = "function", pattern = ".R",full.names = TRUE), source)

# establish grid features 
g100 <- sf::st_read("data/grid100km_aea.gpkg")


# build index table 
# years must be present, either gridID or lat,lon pair is required 
years <- c("2020")
gridID <- c()
lat <- c(43.91339704169197)
lon <- c(-100.42413375586572)
# construct your table 
table <- build_index_table(years = years,  lat = lat, lon = lon)


# setup some storage 
# Define directory structure
naip_dir <- file.path("data/naipExports")
snic_dir <- file.path("data/snicExports")
lidar_dir <- file.path("data/lidarExports")
temp_dir <- file.path("data/download")


# assuming lat lon for now 
for(i in 1:nrow(table)){
  year <- table$year[i]
  point <- c(table$lon[i], table$lat[i])
  # grab the aoi object 
  aoi <- getAOI(grid100 = g100, point = point)
  # using the aoi ID from here 
  id <- aoi$id
  # test the name year 
  years <- getNAIPYear(aoi)
  cat("NAIP is available for the following years", years)
  # Condition to test if the year is present in the example 
  if(year %in% years){
    print("NAIP is available for the defined year")
  }else{
    print("NAIP is not available for the year skipping the process")
    next()
  }
  # download the naip 
  ## this buffers the aoi by 200m before download
  downloadNAIP_vsi(aoi = aoi, year = year, exportFolder = temp_dir)
  # download lidar 
  ## this downloads the aoi extent 
  download_lidar_dsm(aoi = aoi, exportFolder = temp_dir)
  
  # gather naip files 
  naip_string <- paste0("^naip.*", id, ".*\\.tif$")
  
  # List the files
  naip_files <- list.files(
    path = "data/download", # Replace with your actual directory path
    pattern = naip_string, 
    full.names = TRUE
  )
  
  # processing 
  mergeAndExportNAIP(files =naip_files, out_path = naip_dir, aoi = aoi )
  
  # snic processing 
  
  
  # gather naip files 
  lidar_string <- paste0("^lidar.*", id, ".*\\.tif$")
  
  # List the files
  lidar_files <- list.files(
    path = "data/download", # Replace with your actual directory path
    pattern = lidar_string, 
    full.names = TRUE
  )
  mergeAndExportLidar(
    
  )
  
}




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


