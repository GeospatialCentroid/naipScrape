


build_index_table <- function(years, gridID = NULL, lat = NULL, lon = NULL) {
  
  # 1. Validation: years is required
  if (missing(years) || is.null(years)) {
    stop("The 'years' argument is required.")
  }
  
  # 2. Logic for choosing the spatial component
  if (!is.null(gridID)) {
    # If gridID is provided, prioritize it
    index <- tidyr::crossing(year = years, gridID = gridID)
    
  } else if (!is.null(lat) && !is.null(lon)) {
    # If no gridID, use lat/lon (must have both)
    if (length(lat) != length(lon)) {
      stop("lat and lon must be the same length to create coordinate pairs.")
    }
    
    # Create a coordinates reference and cross with years
    coords <- data.frame(lat = lat, lon = lon)
    index <- tidyr::crossing(year = years, coords)
    
  } else {
    # If only years were provided (or lat/lon was incomplete)
    message("No spatial identifiers provided. Returning year-only index.")
    index <- data.frame(year = years)
  }
  
  return(index)
}