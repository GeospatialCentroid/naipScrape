source("function/transferToNDrive.R")
library("tictoc")

# --- Example Usage for macOS ---
localExportFolder <- "data/naipExports"
networkTarget <- "/Volumes/wcnr-network/Research/Ogle/Agroforestry/phase2_sampling/data/raw/mlraF_NAIP"

files <- list.files(localExportFolder, full.names = TRUE)

# Adjusting to process either the first 100 or all files available
num_to_process <- length(files)

tic("Transfer")
cat(paste0("Starting the data transfer process with ", num_to_process, " files\n"))

for(i in 1:num_to_process){
  cat(paste0("feature: ", i, " out of ", num_to_process, "\n"))
  
  file <- files[i]
  name <- basename(file)
  dest_path <- file.path(networkTarget, name)
  
  # Copy file
  val <- file.copy(from = file, to = dest_path, overwrite = FALSE)
  
  # Check the transfer
  if(file.exists(dest_path)){
    file.remove(file) # Uncomment when ready to delete local copies
    print("file copied")
  }
  
  # --- Throttling Logic ---
  # After every 10th file, wait 2 seconds (but not after the very last file)
  if (i %% 10 == 0 && i < num_to_process) {
    cat("Pausing for 2 seconds to prevent throttling...\n")
    Sys.sleep(2)
  }
}

full_time <- toc()
# Note: tictoc stores the message in the object if you need to reference it later


# using Rsync, which check for changes and new files then transferd features. 
# syncToNetwork(localExportFolder = localExportFolder, networkMountPath = networkTarget)