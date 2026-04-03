# This is a generalized method for downloading material from the planetary computer 

pacman::p_load(dplyr, sf, terra, tidyr, tictoc, foreach, doParallel)
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
set.seed(12446)
# past seeds 
# 12345, 12346, 12347, 12348
# generate 20 random samples 
points <- sf::st_sample(x = mlra, size = 96, by_polygon = TRUE)
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

# Initialize storage for NAIP-specific timings
naip_iteration_times <- numeric()

tic("Total Script Runtime") # Overall timer for the whole process

# 1. Setup Parallel Backend
# Detect cores and leave a couple free to keep your system responsive
num_cores <- max(1, parallel::detectCores() - 24) 
cl <- makeCluster(num_cores)
registerDoParallel(cl)

cat("Starting cluster with", num_cores, "cores...\n")

tic("Total Parallel Runtime")

# 2. Execute Parallel Loop
# .errorhandling = 'pass' ensures that if the loop itself crashes, it doesn't break the whole foreach
# We use .packages to ensure the worker nodes have the libraries they need
results <- foreach(i = 1:nrow(table), 
                   .packages = c("terra", "sf", "tictoc"), 
                   .errorhandling = 'pass') %dopar% {
                     
                     year <- table$year[i]
                     point <- c(table$lon[i], table$lat[i])
                     aoi <- getAOI(grid100 = g100, point = point)
                     id <- aoi$id
                     
                     if(dir.exists(paste0("data/exportData/aoi_",id,"_",year))){
                       return(list(id = id, status = "Skipped - Already Exists", time = 0))
                     }
                     
                     years <- getNAIPYear(aoi)
                     if(!year %in% years){
                       year <- as.character(as.numeric(year) - 1)
                     }
                     
                     # ---------------------------------------------------------
                     # START TIMING AND ERROR HANDLING
                     # ---------------------------------------------------------
                     tic() 
                     
                     # tryCatch attempts the code in the first block. 
                     # If it fails, it immediately jumps to the 'error' function.
                     process_status <- tryCatch({
                       
                       # 1. Download
                       downloadNAIP_vsi(aoi = aoi, year = year, exportFolder = temp_dir)
                       
                       # 2. Gather and Merge
                       naip_string <- paste0("^naip.*", id, ".*\\.tif$")
                       naip_files <- list.files(path = "data/download", pattern = naip_string, full.names = TRUE)
                       
                       if(length(naip_files) == 0) {
                         stop("Download succeeded but no files matched the regex pattern.")
                       }
                       
                       mergeAndExportNAIP(files = naip_files, out_path = naip_dir, aoi = aoi)
                       
                       # If it makes it here, it was successful
                       "Success"
                       
                     }, error = function(cond) {
                       # This block executes ONLY if something above throws an error
                       # It returns the error message as a string
                       return(paste("Failed:", conditionMessage(cond)))
                     })
                     
                     naip_timer <- toc(quiet = TRUE)
                     elapsed_time <- naip_timer$toc - naip_timer$tic
                     
                     # Return a list of data for this specific iteration
                     return(list(id = id, 
                                 status = process_status, 
                                 time = elapsed_time))
                   }

# Stop the cluster once finished
stopCluster(cl)
total_runtime <- toc()

# ---------------------------------------------------------
# REPORTING
# ---------------------------------------------------------

# Extract data from the results list
successful_runs <- results[sapply(results, function(x) x$status == "Success")]
failed_runs <- results[sapply(results, function(x) grepl("Failed", x$status))]
skipped_runs <- results[sapply(results, function(x) grepl("Skipped", x$status))]

success_times <- sapply(successful_runs, function(x) x$time)

cat("\n==========================================\n")
cat("PARALLEL PROCESSING SUMMARY\n")
cat("Total Tasks Attempted: ", nrow(table), "\n")
cat("Successful: ", length(successful_runs), "\n")
cat("Failed: ", length(failed_runs), "\n")
cat("Skipped: ", length(skipped_runs), "\n")
cat("------------------------------------------\n")
if(length(success_times) > 0){
  cat("Avg NAIP time per successful loop: ", round(mean(success_times), 2), "seconds\n")
}
cat("Total script runtime: ", round(total_runtime$toc - total_runtime$tic, 2), "seconds\n")
cat("==========================================\n")

# Print the specific errors if any occurred so you can debug MPC limits
if(length(failed_runs) > 0) {
  cat("\nError Log:\n")
  for(fail in failed_runs) {
    cat("ID:", fail$id, "| Error:", fail$status, "\n")
  }
}
