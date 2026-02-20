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
years <- c("2016", "2016" ,"2016", "2016", "2016", "2016", "2016", "2020", "2020", "2020", "2020", "2020", "2020", "2020", "2020")
gridID <- c()
lat <- c(46.72545, 43.03202, 46.29652, 45.69943, 48.50232, 46.21340, 46.41275, 47.96102, 48.97448, 48.87591, 48.07496, 48.46634, 46.86937, 48.95677,48.5877069197)
lon <- c(-103.85334, -97.92205, -102.07305, -102.64953, -104.90845,  -96.82743, -102.04462, -102.04371,  -99.36962, -108.56748, -101.08079,
-111.29966, -102.45198, -107.21443,  -96.78875)
# construct your table 
table <- build_index_table(years = years,  lat = lat, lon = lon)


# setup some storage 
# Define directory structure
aoi_dir <- file.path("data/aoiExports")
naip_dir <- file.path("data/naipExports")
snic_dir <- file.path("data/snicExports")
lidar_dir <- file.path("data/lidarExports")
temp_dir <- file.path("data/download")
export_dir <- file.path("data/exportData")


# So eventually we'll want s to be a bit more flexble so the either the 
#  lat long value or e ID value could be used. But for now I know we just have id 



# assuming lat lon for now 
for(i in 1:nrow(table)){
  year <- table$year[i]
  point <- c(table$lon[i], table$lat[i])
  # grab the aoi object 
  aoi <- getAOI(grid100 = g100, point = point)
  # using the aoi ID from here 
  id <- aoi$id
  if(dir.exists(paste0("data/exportData/aoi_",id,"_",year))){
    print("AOI complete")
    next()
  }


  # test the name year 
  years <- getNAIPYear(aoi)
  cat("NAIP is available for the following years", years)
  # Condition to test if the year is present in the example 
  if(year %in% years){
    print("NAIP is available for the defined year")
  }else{
    print("NAIP is not available for the year skipping the process")
    year <- as.character(as.numeric(year) - 1)
    # next()
  }
  # download the naip 
  ## this buffers the aoi by 200m before download
  downloadNAIP_vsi(aoi = aoi, year = year, exportFolder = temp_dir)
  
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

  ## select 1km  rast for areas 
  r1 <- terra::rast(list.files(
    path = naip_dir, 
    pattern = paste0("^oneKM_.*", id, ".*\\.tif$"), 
    full.names = TRUE
  ))
  # generate seeds 
  seeds <- generate_scaled_seeds(r = r1)

  # generate the snic objects 
  process_segmentations(r = r1,
  seed_list =  seeds,
  output_dir = "data/snicExports",
  file_id = id,
  year = year)
 
  # download lidar 
  ## this downloads the aoi extent 
  download_lidar_dsm(aoi = aoi, exportFolder = temp_dir)
  
  lidar_string <- paste0("^lidar.*", id, ".*\\.tif$")
  
  # List the files
  lidar_files <- list.files(
    path = "data/download", # Replace with your actual directory path
    pattern = lidar_string, 
    full.names = TRUE
  )
  try(mergeAndExportLidar(files = lidar_files, out_path = lidar_dir, aoi = aoi ))
  
  # compile data for export 
  ## note that this will delete the processed data from the export folders
  copyToExport(id = id, year = year)
  ## from here folders are still manually downloaded to local pc for addition to the teams folder 
  # for zipping export 
  ## for d in */; do zip -r "${d%/}.zip" "$d"; done
}


# options 
## clear download 
if(FALSE){
  files_to_remove <- list.files("data/download", full.names = TRUE)
  for(file in files_to_remove){
    unlink(file)
  }

}


