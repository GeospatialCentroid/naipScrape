library(DBI)
library(RSQLite)

# This allows you to view the current tracker dataframe to see how things are progressing
checkSQL <- function() {
  # Connect to the DB (read-only is fine here)
  con <- dbConnect(RSQLite::SQLite(), "data/download_tracker.sqlite")

  # Pull the whole table into R
  tracker_df <- dbReadTable(con, "aoi_tracker")

  # Disconnect
  dbDisconnect(con)

  # View the raw data
  View(tracker_df)
}

# required when testing, to clear out previous results and ensure that the method will run again
clearSQL <- function() {
  # Connect to the database
  con <- dbConnect(RSQLite::SQLite(), "data/download_tracker.sqlite")

  # Drop the tracking table completely

  dbExecute(con, "DROP TABLE IF EXISTS aoi_tracker")

  # Disconnect
  dbDisconnect(con)
}
