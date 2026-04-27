syncToNetwork <- function(localExportFolder, networkMountPath) {
  
  # Ensure the source path ends with a trailing slash so rsync transfers the 
  # contents of the folder, rather than the folder itself
  src <- if (grepl("/$", localExportFolder)) localExportFolder else paste0(localExportFolder, "/")
  
  # Ensure destination directory exists (in case the Mac dropped the SMB connection)
  if (!dir.exists(networkMountPath)) {
    stop("Destination path does not exist. Check if the SMB share is mounted in Finder.")
  }
  
  # Define rsync arguments
  # -a : Archive mode (preserves permissions, times, and structure)
  # -v : Verbose output
  # --ignore-existing : Skips files that already exist at the destination
  # --progress : Shows progress for large files
  rsync_args <- c(
    "-a", 
    "-v", 
    "--ignore-existing", 
    "--progress",
    src, 
    networkMountPath
  )
  
  message(sprintf("Starting rsync from %s to %s...", src, networkMountPath))
  
  # Execute the rsync command
  # stdout = "" and stderr = "" print the output directly to the R console
  result <- system2("rsync", args = rsync_args, stdout = "", stderr = "")
  
  if (result == 0) {
    message("Rsync completed successfully.")
  } else {
    warning("Rsync encountered an issue. Exit code: ", result)
  }
}