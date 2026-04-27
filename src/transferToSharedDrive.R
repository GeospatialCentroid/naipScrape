source("function/transferToNDrive.R")

# --- Example Usage for macOS ---
localExportFolder <- "data/naipExports"
networkTarget <- "/Volumes/wcnr-network/Research/Ogle/Agroforestry/phase2_sampling/data/raw/mlraF_NAIP"

syncToNetwork(localExportFolder = localExportFolder, networkMountPath = networkTarget)