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
#' @param sort Merged data is sorted by these variable(s). Defaults to
#'   "created_time", then "scrape_time", and then "id" for Facebook data.
#' @param sort_direction Sort parameters are applied in this directions. Should
#'   be length 1 (all parameters are sorted this way) or the same length as
#'   sort. Possible values are "desc" for descending and "asc" or "" for
#'   ascending.
#' @param keep_newest Logical, indicating which version of a duplicate text is
#'   kept. If TRUE (default), the newest texts according to scrape date are
#'   kept if ignore_scrape_time is not TRUE. Furthermore, texts from files in
#'   the new_folder are preferred over those from old_folder. FALSE prefers
#'   older data.
#' @param ignore_scrape_time Logical, indicating whether the scrape time should
#'   be ignored for deciding which texts to keep. Defaults to FALSE. If TRUE,
#'   only age only depends on where the file is stored (old or new folder).
#' @return A data.frame with the results.
#'
#' @export
merge_data <- function(old_folder, new_folder, output_folder,
                       id = "id",
                       sort = c("created_time", "scrape_time", "id"),
                       sort_direction = c("desc", "desc", "asc"),
                       keep_newest = TRUE,
                       ignore_scrape_time = FALSE) {
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

  if (length(sort_direction) != length(sort) &
      length(sort_direction) != 1) {
    stop(paste0("Number of strings for sort_direction does not match ",
                "the number of parameters in sort. Length of sort_direction ",
                "should be 1 or the same length as sort."))
  }
  if (length(sort_direction) == 1) {
    sort_direction <- rep(sort_direction[1], length(sort))
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
      # If scrape_time (data from old versions) does not exist, add empty column
      if (!any(colnames(old_data) == "scrape_time")) {
        old_data[, "scrape_time"] <- as.POSIXct(character(0))
      }

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
      # If scrape_time (data from old versions) does not exist, add empty column
      if (!any(colnames(new_data) == "scrape_time")) {
        new_data[, "scrape_time"] <- as.POSIXct(character(0))
      }

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
      saveRDS(object = new_data, file = output_file)
      results[results$file == i, ]$result <- "New data is kept unchanged."
    }
    # If there is only old data
    if (!is.null(old_data) & is.null(new_data)) {
      message(paste0("For ", i, " only old data exists. Saving it to the ",
                     "directory for merged files."))
      saveRDS(object = old_data, file = output_file)
      results[results$file == i, ]$result <- "Old data is kept unchanged."
    }
    # If there is old and new data
    if (!is.null(old_data) & !is.null(new_data)) {
      # Combine newer data with older data
      message(paste0("For ", i, " new and old data exist. Data is merged and ",
                     "saved to the directory for merged files."))
      if (keep_newest) {
        merged_data <- dplyr::bind_rows(new_data, old_data)
        if (!ignore_scrape_time) {
          merged_data <- dplyr::arrange(merged_data,
                                        dplyr::desc(.data$scrape_time))
        }
      } else {
        merged_data <- dplyr::bind_rows(old_data, new_data)
        if (!ignore_scrape_time) {
          merged_data <- dplyr::arrange(merged_data,
                                        .data$scrape_time)
        }
      }
      merged_data <- merged_data[!duplicated(merged_data[, id]), ]
      for (j in length(sort):1) {
        if (sort_direction[j] == "desc") {
          merged_data <- dplyr::arrange(
            merged_data,
            dplyr::desc(dplyr::pull(merged_data, sort[j]))
          )
        } else {
          merged_data <- dplyr::arrange(
            merged_data,
            dplyr::pull(merged_data, sort[j])
          )
        }
      }
      saveRDS(object = merged_data, file = output_file)
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
#' @param keep_newest Logical, indicating which version of a duplicate Tweet is
#'   kept. If TRUE (default), the newest Tweets according to scrape date are
#'   kept. Furthermore, Tweets from files in the new_folder are preferred over
#'   those from old_folder. FALSE prefers older data.
#' @param ignore_scrape_time Logical, indicating whether the scrape time should
#'   be ignored for deciding which texts to keep. Defaults to FALSE. If TRUE,
#'   only age only depends on where the file is stored (old or new folder).
#' @return A data.frame with the results.
#'
#' @export
merge_facebook_data <- function(old_folder, new_folder, output_folder,
                                keep_newest = TRUE,
                                ignore_scrape_time = FALSE) {
  merge_data(old_folder = old_folder,
             new_folder = new_folder,
             output_folder = output_folder,
             id = "id",
             sort = c("created_time", "scrape_time", "id"),
             sort_direction = c("desc", "desc", "asc"),
             keep_newest = keep_newest,
             ignore_scrape_time = ignore_scrape_time)
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
#' @param keep_newest Logical, indicating which version of a duplicate post is
#'   kept. If TRUE (default), the newest posts according to scrape date are
#'   kept. Furthermore, posts from files in the new_folder are preferred over
#'   those from old_folder. FALSE prefers older data.
#' @param ignore_scrape_time Logical, indicating whether the scrape time should
#'   be ignored for deciding which texts to keep. Defaults to FALSE. If TRUE,
#'   only age only depends on where the file is stored (old or new folder).
#' @return A data.frame with the results.
#'
#' @export
merge_twitter_data <- function(old_folder, new_folder, output_folder,
                               keep_newest = TRUE, ignore_scrape_time = FALSE) {
  merge_data(old_folder = old_folder,
             new_folder = new_folder,
             output_folder = output_folder,
             id = "status_id",
             sort = c("created_at", "scrape_time", "status_id"),
             sort_direction = c("desc", "desc", "asc"),
             keep_newest = keep_newest,
             ignore_scrape_time = ignore_scrape_time)
}
