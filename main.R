# This is a generalized method for downloading material from the planetary computer 

pacman::p_load(dplyr, sf, terra, tidyr, tictoc)
# testing 
library(tmap)
tmap_mode(mode = "view")
# source files 
lapply(list.files(path = "function", pattern = ".R",full.names = TRUE), source)

# establish grid features 
g100 <- sf::st_read("data/grid100km_aea.gpkg")


# random sampling with an LRR 
mlra <- sf::st_read(dsn = "data/mlra/lower48MLRA.gpkg") |>
  dplyr::filter(LRRSYM == "F")
# change seed for difference 
set.seed(12348)
# past seeds 
# 12345, 12346, 12347, 12348
# generate 20 random samples 
points <- sf::st_sample(x = mlra, size = 24, by_polygon = TRUE)
coords_df <- as.data.frame(st_coordinates(points))
table <- build_index_table(years = rep(c("2012", "2016", "2020"), 8),
                           lat = coords_df$Y,
                           lon = coords_df$X)

# setup some storage 
# Define directory structure
aoi_dir <- file.path("data/aoiExports")
naip_dir <- file.path("data/naipExports")
snic_dir <- file.path("data/snicExports")
lidar_dir <- file.path("data/lidarExports")
temp_dir <- file.path("data/download")
# final directory for the folders 
export_dir <- file.path("data/exportData")


# So eventually we'll want s to be a bit more flexble so the either the 
#  lat long value or e ID value could be used. But for now I know we just have id 


library(tictoc)

# Initialize storage for NAIP-specific timings
naip_iteration_times <- numeric()

tic("Total Script Runtime") # Overall timer for the whole process

for(i in 1:nrow(table)){
  # --- Setup (Not Timed) ---
  year <- table$year[i]
  point <- c(table$lon[i], table$lat[i])
  aoi <- getAOI(grid100 = g100, point = point)
  id <- aoi$id
  
  if(dir.exists(paste0("data/exportData/aoi_",id,"_",year))){
    print("AOI complete")
    next()
  }
  
  # ---------------------------------------------------------
  # START NAIP TIMING
  # ---------------------------------------------------------
  tic() 
  
  years <- getNAIPYear(aoi)
  if(year %in% years){
    print("NAIP is available for the defined year")
  } else {
    year <- as.character(as.numeric(year) - 1)
  }
  
  # Download
  downloadNAIP_vsi(aoi = aoi, year = year, exportFolder = temp_dir)
  
  # Gather and Merge
  naip_string <- paste0("^naip.*", id, ".*\\.tif$")
  naip_files <- list.files(path = "data/download", pattern = naip_string, full.names = TRUE)
  mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi)
  
  # End NAIP Timer and store result
  naip_timer <- toc(quiet = TRUE)
  naip_iteration_times <- c(naip_iteration_times, naip_timer$toc - naip_timer$tic)
  # ---------------------------------------------------------
  # END NAIP TIMING
  # ---------------------------------------------------------
  
  # --- SNIC Processing (Untimed) ---
  r1 <- terra::rast(list.files(path = naip_dir, pattern = paste0("^oneKM_.*", id, ".*\\.tif$"), full.names = TRUE))
  seeds <- generate_scaled_seeds(r = r1)
  process_segmentations(r = r1, seed_list = seeds, output_dir = "data/snicExports", file_id = id, year = year)
  
  # --- Export (Untimed) ---
  copyToExport(id = id, year = year)
}

# End total runtime
total_runtime <- toc()

# --- Reporting ---
cat("\n==========================================\n")
cat("NAIP PROCESSING SUMMARY\n")
cat("Total Loops Processed: ", length(naip_iteration_times), "\n")
cat("Avg NAIP time per loop: ", round(mean(naip_iteration_times), 2), "seconds\n")
cat("Total NAIP time (all loops): ", round(sum(naip_iteration_times), 2), "seconds\n")
cat("Total script runtime: ", round(total_runtime$toc - total_runtime$tic, 2), "seconds\n")
cat("==========================================\n")
# options 
## clear download 
if(FALSE){
  files_to_remove <- list.files("data/download", full.names = TRUE)
  for(file in files_to_remove){
    unlink(file)
  }
}


