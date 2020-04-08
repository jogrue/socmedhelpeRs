#' Add variable scrape_time to old existing data
#'
#' In version 3.1 of this package a new variable "scrape_time" was added. It
#' records the Sys.time() when the data was scraped. This functions adds this
#' variable (with NA) to all files in an folder (if it does not exist already).
#'
#' @param dir A path to a directory containing data files generated with (older)
#'            versions of this package.
#'
#' @export
add_scraped_time <- function(dir) {
  # Checking parameters
  if (missing(dir)){
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

#' Add variable fan_count to old existing data
#'
#' In version 3.2 of this package a new variable "fan_count" was added to
#' Facebook data. It records the fan count of pages when scraping posts. This
#' functions adds this variable (with NA) to all files in an folder (if it does
#' not exist already).
#'
#' @param dir A path to a directory containing data files generated with (older)
#'            versions of this package.
#'
#' @export
add_fan_count <- function(dir) {
  # Checking parameters
  if (missing(dir)){
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

  # Add fan_count variable if it does not already exist
  for (i in files) {
    file <- paste0(dir, i)
    # If the file exists, it is loaded
    if (file.exists(file)) {
      data <- readRDS(file)
      # If scrape_time (data from old versions) does not exist, add empty column
      # Also save data set
      if (!any(colnames(data) == "fan_count")) {
        data[, "fan_count"] <- as.numeric(character(0))
        saveRDS(data, file)
        message(paste0("Added empty variable fan_count for ", i, "!"))
      }
    }
  }
}

#' Add variables for reactions to old existing data
#'
#' If previous data was scraped without reactions, empty reaction variables are
#' added here if such variables to not exist already.
#'
#' @param dir A path to a directory containing data files generated with this
#' package.
#' @param sort Should variables be sorted according to current package default?
#' Defaults to TRUE.
#'
#' @export
add_reactions <- function(dir, sort = TRUE) {
  # Checking parameters
  if (missing(dir)){
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

  # Add reaction variables if they do not already exist
  for (i in files) {
    file <- paste0(dir, i)
    # If the file exists, it is loaded
    if (file.exists(file)) {
      data <- readRDS(file)
      change_made <- FALSE
      # If scrape_time (data from old versions) does not exist, add empty column
      # Also save data set
      if (!any(colnames(data) == "love_count")) {
        data[, "love_count"] <- as.numeric(character(0))
        change_made <- TRUE
      }
      if (!any(colnames(data) == "haha_count")) {
        data[, "haha_count"] <- as.numeric(character(0))
        change_made <- TRUE
      }
      if (!any(colnames(data) == "wow_count")) {
        data[, "wow_count"] <- as.numeric(character(0))
        change_made <- TRUE
      }
      if (!any(colnames(data) == "sad_count")) {
        data[, "sad_count"] <- as.numeric(character(0))
        change_made <- TRUE
      }
      if (!any(colnames(data) == "angry_count")) {
        data[, "angry_count"] <- as.numeric(character(0))
        change_made <- TRUE
      }
      if (change_made) {
        if (sort) {
          data <- sort_data(data = data)
        }
        saveRDS(data, file)
        message(paste0("Added empty reaction variables for ", i, "!"))
      }
    }
  }
}


#' Current order of variables in Facebook data
#'
#' @return The current list of variable names for sorting Facebook data.
#' @export
current_facebook_sort <- function() {
  c(
    "id",
    "likes_count",
    "from_id",
    "from_name",
    "message",
    "created_time",
    "type",
    "link",
    "story",
    "comments_count",
    "shares_count",
    "love_count",
    "haha_count",
    "wow_count",
    "sad_count",
    "angry_count",
    "scrape_time",
    "fan_count"
  )
}

#' Sort a dataset
#'
#' A social media dataset is ordered according to a list of variable names.
#' Unmentioned variables are attached at the end.
#'
#' @param data A data.frame generated with this package.
#' @param sorted_variables A list of variable names according to which the
#' dataset is sorted. Defaults to the package default for Facebook data.
#'
#' @return The sorted dataset.
#' @export
sort_data <- function(data, sorted_variables) {
  # Checking parameters
  if (missing(data)){
    stop(paste0("A data.frame with social media data has to be provided."))
  }
  if (missing(sorted_variables)) {
    sorted_variables <- current_facebook_sort()
  }
  dplyr::select(
    data,
    tidyselect::all_of(sorted_variables[sorted_variables %in% names(data)]),
    dplyr::everything()
  )
}

#' Sort data stored in a rds file
#'
#' A dataset stored in an rds file is ordered according to a list of variable
#' names. Unmentioned variables are attached at the end and the original file
#' is overwritten with the sorted dataset.
#'
#' @param file A path to an rds file generated with this package and containing
#' social media data.
#' @param sorted_variables A list of variable names according to which the
#' dataset is sorted. Defaults to the package default for Facebook data.
#'
#' @export
sort_file <- function(file, sorted_variables) {
  # Checking parameters
  if (missing(file)){
    stop(paste0("A file path to social media data has to be provided."))
  }
  if (!file.exists(file)) {
    stop("File path given for data does not exist.")
  }
  if (missing(sorted_variables)) {
    sorted_variables <- current_facebook_sort()
  }
  data <- readRDS(file)
  if (!identical(names(data), sorted_variables)) {
    data <- sort_data(data, sorted_variables)
    saveRDS(data, file)
    message(paste0("Sorted dataset in ", file, "!"))
  }
}


#' Sort all data files from a directory
#'
#' All datasets from one folder with rds files are ordered according to a list
#' of variable names. Unmentioned variables are attached at the end and the
#' original files are overwritten with the sorted datasets.
#'
#' @param dir A path to a directory containing data files generated with this
#' package.
#' @param sorted_variables A list of variable names according to which the
#' dataset is sorted. Defaults to the package default for Facebook data.
#'
#' @export
sort_dir <- function(dir, sorted_variables) {
  # Checking parameters
  if (missing(dir)){
    stop(paste0("A directory containing social media data has to be provided."))
  }
  if (!dir.exists(dir)) {
    stop("Directory given for data does not exist.")
  }
  if (missing(sorted_variables)) {
    sorted_variables <- current_facebook_sort()
  }
  # Adjust directory path
  if (!endsWith(dir, "/")) { dir <- paste0(dir, "/") }
  # Read and check file names
  files <- list.files(path = dir, pattern = "*.rds")
  if (length(files) < 1) {
    stop(paste0("There are no rds files in ", dir, "."))
  }
  # Sort all files
  for (i in files) {
    file <- paste0(dir, i)
    # If the file exists, it is loaded
    if (file.exists(file)) {
      sort_file(file = file, sorted_variables = sorted_variables)
    }
  }
}

#' Clean Facebook data duplicates
#'
#' Somehow duplicates ended up in the data, were the same post is stored with
#' two different message IDs. Here, only messages where the sender (from_id),
#' message text (message), time of the posting (created_time), and message type
#' (type) are distinct. You can provide either a directory or a file
#'
#' @param dir A path to a directory containing Facebook data files.
#' @param file A file path to one Facebook data file (as rds file).
#' @param sort Merged data is sorted by these variable(s). Defaults to
#'   c("created_time", "scrape_time", "id") to sort data by these variables.
#'   The sort is applied before duplicates are removed. Therefore by default
#' @param sort_direction Sort parameters are applied in this directions. Should
#'   be length 1 (all parameters are sorted this way) or the same length as
#'   sort. Possible values are "desc" for descending and "asc" or "" for
#'   ascending. Defaults to c("desc", "desc", "asc"). Thus, by default,
#'   created_time and scrape_time are sorted descendingly and if both are the
#'   same they are sorted by message id ascendingly.
#' @export
remove_facebook_duplicates <-
  function(dir, file,
           sort = c("created_time", "scrape_time", "id"),
           sort_direction = c("desc", "desc", "asc")) {
    # Checking parameters
    if (missing(dir) & missing(file)){
      stop("A directory or a file with social media data has to be provided.")
    }
    run_dir = (!missing(dir))
    run_file = (!missing(file))

    if (length(sort_direction) != length(sort) &
        length(sort_direction) != 1) {
      stop(paste0("Number of strings for sort_direction does not match ",
                  "the number of parameters in sort. Length of sort_direction ",
                  "should be 1 or the same length as sort."))
    }
    if (length(sort_direction) == 1) {
      sort_direction <- rep(sort_direction[1], length(sort))
    }

    # Run for one file
    if (run_file) {
      if (!file.exists(file)) {
        stop("Data file given does not exist.")
      }
      data <- readRDS(file)
      if (
        !all(
          c("id","from_id", "message", "created_time", "type") %in%
          names(data)
        )
      ) {
        stop(paste0("File ", file,
                    " does not include all Facebook data variables."))
      }
      for (j in length(sort):1) {
        if (sort_direction[j] == "desc") {
          data <- dplyr::arrange(
            data,
            dplyr::desc(dplyr::pull(data, sort[j]))
          )
        } else {
          data <- dplyr::arrange(
            data,
            dplyr::pull(data, sort[j])
          )
        }
      }
      data <- dplyr::distinct(data, .data$id, .keep_all = TRUE)
      data <- dplyr::distinct(
        data,
        .data$from_id,
        .data$message,
        .data$created_time,
        .data$type,
        .keep_all = TRUE)
      saveRDS(data, file)
      message(paste0("Removed duplicate entries from ", file, "!"))
    }

    # Run for the directory
    if (run_dir) {
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
          if (
            !all(
              c("id","from_id", "message", "created_time", "type") %in%
              names(data)
            )
          ) {
            stop(paste0("File ", i,
                        " does not include all Facebook data variables."))
          }
          for (j in length(sort):1) {
            if (sort_direction[j] == "desc") {
              data <- dplyr::arrange(
                data,
                dplyr::desc(dplyr::pull(data, sort[j]))
              )
            } else {
              data <- dplyr::arrange(
                data,
                dplyr::pull(data, sort[j])
              )
            }
          }
          data <- dplyr::distinct(data, .data$id, .keep_all = TRUE)
          data <- dplyr::distinct(
            data,
            .data$from_id,
            .data$message,
            .data$created_time,
            .data$type,
            .keep_all = TRUE)
          saveRDS(data, file)
          message(paste0("Removed duplicate entries from ", i, "!"))
        }
      }
    }
  }
