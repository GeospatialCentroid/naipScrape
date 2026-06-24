#' Compile all JSON status trackers into a master data frame
#'
#' @param local_working_dir Character. Path to the local processing/export directory.
#' @return A data frame containing all completed/partial/failed status records.
compileStatus <- function(local_working_dir = "/run/media/dan/T7/naip_bulk_export") {
  status_files <- list.files(
    path = local_working_dir,
    pattern = "^status\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(status_files) == 0) {
    message("No status.json files found on disk.")
    return(NULL)
  }
  
  results_list <- lapply(status_files, function(f) {
    tryCatch({
      jsonlite::fromJSON(f)
    }, error = function(e) NULL)
  })
  
  results_list <- results_list[!sapply(results_list, is.null)]
  
  if (length(results_list) > 0) {
    master_df <- dplyr::bind_rows(results_list)
    return(master_df)
  } else {
    return(NULL)
  }
}

#' Clear all JSON status trackers on disk
#'
#' @param local_working_dir Character. Path to the local processing/export directory.
clearStatus <- function(local_working_dir = "/run/media/dan/T7/naip_bulk_export") {
  status_files <- list.files(
    path = local_working_dir,
    pattern = "^status\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(status_files) == 0) {
    message("No status.json files found to clear.")
    return(invisible(FALSE))
  }
  
  file.remove(status_files)
  message(sprintf("Successfully removed %d status.json files.", length(status_files)))
  return(invisible(TRUE))
}
