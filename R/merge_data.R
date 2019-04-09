#' Merge rds files from different folders
#'
#' This function merges rds files of the same name from two different folders.
#' It is intended for usage with social media data collected through my update
#' functions in this package. Data from the file in old_folder is kept, data
#' from new_folder is added but only kept if rows with the same id do not
#' exist already.
#'
#' @param old_folder Folder to look for old rds files.
#' @param new_folder Folder to look for new rds files.
#' @param output_folder Folder to save merged rds files to.
#' @param id Unique id(s), only data where id is distinct is kept. Defaults to
#'   "id" for Facebook data.
#' @param sort Merged data is sorted by this parameter. Defaults to
#'   "created_time" for Facebook data.
#' @return A data.frame with the results.
#'
#' @export
merge_data <- function(old_folder, new_folder, output_folder,
                       id = "id", sort = "created_time") {
  if (!require(dplyr)) { stop("Package dplyr is missing.") }

  # Checking parameters
  if (is.null(old_folder) | is.null(new_folder) | is.null(output_folder)) {
    stop("All three folders have to be provided.")
  }

  if (!dir.exists(old_folder)) {
    stop("Folder given for old data does not exist.")
  }

  if (!dir.exists(new_folder)) {
    stop("Folder given for new data does not exist.")
  }

  if (!endsWith(old_folder, "/")) { old_folder <- paste0(old_folder, "/") }
  if (!endsWith(new_folder, "/")) { new_folder <- paste0(new_folder, "/") }
  if (!endsWith(output_folder, "/")) {
    output_folder <- paste0(output_folder, "/")
  }

  old_files <- list.files(path = old_folder, pattern = "*.rds")
  new_files <- list.files(path = new_folder, pattern = "*.rds")

  if (length(old_files) < 1) {
    stop(paste0("There are no rds files in ", old_folder, "."))
  }
  if (length(new_files) < 1) {
    stop(paste0("There are no rds files in ", new_folder, "."))
  }

  files <- c(old_files, new_files)
  files <- unique(files)

  # Creating output data directory if necessary
  if (!dir.exists(output_folder)) {
    message(paste0(output_folder,
                   " does not exist. Creating a new directory ..."))
    dir.create(output_folder)
  }
  results <- data.frame(file = files, result = "No data available.",
                        stringsAsFactors = FALSE)
  for (i in files) {
    old_file <- paste0(old_folder, i)
    new_file <- paste0(new_folder, i)
    output_file <- paste0(output_folder, i)
    old_data <- NULL
    new_data <- NULL
    # If the file exists, it is loaded
    if (file.exists(old_file)) {
      old_data <- readRDS(old_file)
      # If there is an empty file, it is set to NULL and not included in the
      # output folder
      if (nrow(old_data) < 1) {
        old_data <- NULL
        message(paste0("An old file for ", i,
                       " exists, but does not contain data.",
                       " It is treated as non-existing."))
      }
    }
    if (file.exists(new_file)) {
      new_data <- readRDS(new_file)
      # If there is an empty file, it is set to NULL and not included in the
      # output folder
      if (nrow(new_data) < 1) {
        new_data <- NULL
        message(paste0("A new file for ", i,
                       " exists, but does not contain data.",
                       " It is treated as non-existing."))
      }
    }
    # If there is only new data
    if (is.null(old_data) & !is.null(new_data)) {
      message(paste0("For ", i, " only new data exists. Saving it to the ",
                     "directory for merged files."))
      saveRDS(new_data, output_file)
      results[results$file == i, ]$result <- "New data is kept unchanged."
    }
    # If there is only old data
    if (!is.null(old_data) & is.null(new_data)) {
      message(paste0("For ", i, " only old data exists. Saving it to the ",
                     "directory for merged files."))
      saveRDS(old_data, output_file)
      results[results$file == i, ]$result <- "Old data is kept unchanged."
    }
    # If there is old and new data
    if (!is.null(old_data) & !is.null(new_data)) {
      # Combine newer data with older data
      message(paste0("For ", i, " new and old data exist. Data is merged and ",
                     "saved to to the directory for merged files."))
      merged_data <- dplyr::bind_rows(old_data, new_data)
      merged_data <- merged_data[!duplicated(merged_data[, id]), ]
      merged_data <- dplyr::arrange(merged_data,
                                    dplyr::desc(merged_data[, sort]))
      results[results$file == i, ]$result <-
        paste0("Data merged, ",
               nrow(merged_data) - nrow(old_data),
               " lines added.")
    }
  }
  return(results)
}

#' Merge Facebook rds files from different folders
#'
#' This function merges rds files of the same name from two different folders.
#' It calls merge_data with parameters for Facebook data collected through this
#' package's update function. Data from the file in old_folder is kept, data
#' from new_folder is added but only kept if rows with the same id do not
#' exist already.
#'
#' @param old_folder Folder to look for old rds files.
#' @param new_folder Folder to look for new rds files.
#' @param output_folder Folder to save merged rds files to.
#' @return A data.frame with the results.
#'
#' @export
merge_facebook_data <- function(old_folder, new_folder, output_folder) {
  merge_data(old_folder = old_folder,
             new_folder = new_folder,
             output_folder = output_folder,
             id = "id",
             sort = "created_time")
}

#' Merge Twitter rds files from different folders
#'
#' This function merges rds files of the same name from two different folders.
#' It calls merge_data with parameters for Facebook data collected through this
#' package's update function. Data from the file in old_folder is kept, data
#' from new_folder is added but only kept if rows with the same id do not
#' exist already.
#'
#' @param old_folder Folder to look for old rds files.
#' @param new_folder Folder to look for new rds files.
#' @param output_folder Folder to save merged rds files to.
#' @return A data.frame with the results.
#'
#' @export
merge_twitter_data <- function(old_folder, new_folder, output_folder) {
  merge_data(old_folder = old_folder,
             new_folder = new_folder,
             output_folder = output_folder,
             id = "status_id",
             sort = "created_at")
}