#' Add variable scrape_time to old existing data
#'
#' In version 3.1 of this package a new variable "scrape_time" was added. It
#' records the Sys.time() when the data was scraped. This functions adds this
#' variable (with NA) to all files in an folder.
#'
#' @param dir A path to a directory containing data files generated with (older)
#'            versions of this package.
#'
#' @export
add_scraped_time <- function(dir) {
  # Checking parameters
  if (is.null(dir)){
    stop("A directory with social media data has to be provided.")
  }
  if (!dir.exists(dir)) {
    stop("Directory given for data does not exist.")
  }

  # Adjust directory path
  if (!endsWith(dir, "/")) { dir <- paste0(dir, "/") }

  # Read and check file names
  files <- list.files(path = dir, pattern = "*.rds")
  if (length(files) < 1) {
    stop(paste0("There are no rds files in ", dir, "."))
  }

  # Add scraped_time variable if it does not already exist
  for (i in files) {
    file <- paste0(dir, i)
    # If the file exists, it is loaded
    if (file.exists(file)) {
      data <- readRDS(file)
      # If scrape_time (data from old versions) does not exist, add empty column
      # Also save data set
      if (!any(colnames(data) == "scrape_time")) {
        data[, "scrape_time"] <- as.POSIXct(character(0))
        saveRDS(data, file)
        message(paste0("Added empty variable scrape_time for ", i, "!"))
      }
    }
  }
}
