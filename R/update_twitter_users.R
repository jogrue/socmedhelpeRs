#' Update or get tweets from a Twitter wall
#'
#' This function updates one Twitter user's wall with \link{rtweet}'s
#' get_timeline() function. It looks for a previously stored data.frame
#' (readable by readRDS). If there is one, it tries to scrape newer and older
#' tweets than the ones already stored in the data.frame. Otherwise it creats a
#' new data.frame.
#'
#' @param user A twitter user name.
#' @param datafile The full path to an RDS data file.
#' @param n The number of tweets to scrape from the wall, defaults to 100.
#' @param token Provide a token, defaults to NULL.
#' @param debug Show more messages during the updating process, default is
#' FALSE.
#'
#' @return TRUE if it ran through.
#'
#' @export
#'
#' @examples
#' # You have to authenticate with Twitter's API first. For more in this visit
#' # rtweet documentation.
#' rtweet::create_token(app = "R_app",
#'                      consumer_key = "1234",
#'                      consumer_secret = "1234",
#'                      access_token = "1234",
#'                      access_secret = "1234")
#'
#' # The BBC World news Twitter feed is scraped and stored in the user's home
#' # folder in the directory "temp" as "bbc_world.rds". If the file does not
#' # already exist a new file is created. If it exists, the existing data is
#' # updated.
#' update_twitter_feed("BBCWorld", "~/temp/bbc_world.rds")
#'
#' # The data set can be loaded with readRDS
#' readRDS("~/temp/bbc_world.rds")
update_twitter_user <- function(user, datafile, n = 100, token = NULL,
                                debug = FALSE) {
  message(paste0("### Updating Twitter timeline for ", user, "."))
  existing_tweets <- 0
  # Load necessary libraries
  if (!require(dplyr)) { stop("Package dplyr is missing.") }
  if (!require(rtweet)) { stop("Package rtweet is missing.") }

  # Check parameters
  if (missing(user)) {
    stop("Parameter user is missing, please provide a Twitter user name.")
  }
  if (missing(datafile)) {
    stop("Parameter datafile is missing, please provide a file path.")
  }

  # Run for the first time or get new tweets until the previously newest tweet
  # is reached (or initialize)
  if (file.exists(datafile)) {

    message(paste0(datafile, " found. Updating data ..."))
    old_data <- readRDS(datafile)
    existing_tweets <- nrow(old_data)

    data <- tryCatch(
      {
        rtweet::get_timeline(user = user, n = n, home = FALSE,
                             parse = TRUE, check = TRUE, token = token)
      }, warning = function(w) {
        message(paste0("A warning occured while calling get_timeline: ", w))
        NULL # Return NULL
      }, error = function(e) {
        message(paste0("An error occured while calling get_timeline: ", e))
        NULL # Return NULL
      })
    if (is.null(data)) { return(FALSE) } # Stop function and return FALSE

    data <- arrange(data, dplyr::desc(created_at))

    if (debug) {
      message(paste0("DEBUG: Newest tweet from pre-existing data: ",
                     max(old_data$created_at)))
    }
    if (debug) {
      message(paste0("DEBUG: Oldest tweet from new data: ",
                     min(data$created_at)))
    }

    # Get new data until newest date from pre-existing data is reached
    while (min(data$created_at) > max(old_data$created_at)) {

      message(paste0("Newer data found for ", user, ". Downloading ", n,
                     " new tweets."))

      max_id <- data[nrow(data), ]$status_id
      if (debug) {
        message(paste0("DEBUG: max_id for updating new tweets: ", max_id))
      }

      new_data <- tryCatch(
        {
          rtweet::get_timeline(user = user, n = n,
                               max_id = max_id,
                               home = FALSE,
                               parse = TRUE, check = TRUE,
                               token = token)
        }, warning = function(w) {
          message(paste0("A warning occured while calling get_timeline: ", w))
          NULL # Return NULL
        }, error = function(e) {
          message(paste0("An error occured while calling get_timeline: ", e))
          NULL # Return NULL
        })
      if (is.null(new_data)) { return(FALSE) } # Stop function and return FALSE

      data <- dplyr::bind_rows(new_data, data)
      data <- dplyr::distinct(data, status_id, .keep_all = TRUE)
      data <- dplyr::arrange(data, dplyr::desc(created_at))
    }

    # Combine new data with previously retrieved data
    data <- dplyr::bind_rows(data, old_data)
    data <- dplyr::distinct(data, status_id, .keep_all = TRUE)
    data <- dplyr::arrange(data, dplyr::desc(created_at))

    # else: Run get_timeline for the first time
  } else {

    message(paste0("No data file found. First run for ", user, " ..."))
    data <- tryCatch(
      {
        rtweet::get_timeline(user = user, n = n, home = FALSE,
                             parse = TRUE, check = TRUE, token = token)
      }, warning = function(w) {
        message(paste0("A warning occured while calling get_timeline: ", w))
        NULL # Return NULL
      }, error = function(e) {
        message(paste0("An error occured while calling get_timeline: ", e))
        NULL # Return NULL
      })
    if (is.null(data)) { return(FALSE) } # Stop function and return FALSE

  }

  # Get older tweets
  repeat {
    # ID of oldest tweet in existing data
    max_id <- data[nrow(data), ]$status_id
    if (debug) {
      message(paste0("DEBUG: max_id for getting older tweets: ", max_id))
    }

    # Get older tweets
    older_data <- tryCatch(
      {
        rtweet::get_timeline(user = user, n = n,
                             max_id = max_id,
                             home = FALSE,
                             parse = TRUE, check = TRUE,
                             token = token)
      }, warning = function(w) {
        message(paste0("A warning occured while calling get_timeline: ", w))
        NULL # Return NULL
      }, error = function(e) {
        message(paste0("An error occured while calling get_timeline: ", e))
        NULL # Return NULL
      })
    if (is.null(older_data)) { return(FALSE) } # Stop function and return FALSE

    if (debug) {
      message(paste0("DEBUG: Number of retrieved older tweets: ",
                     nrow(older_data)))
    }

    # No more older tweets returned: break loop
    if (nrow(older_data) <= 1) {
      break
    }
    message(paste0("There is still older data. Adding up to ", n,
                   " old tweets."))
    # Combine older data with existing data
    data <- dplyr::bind_rows(data, older_data)
    data <- dplyr::distinct(data, status_id, .keep_all = TRUE)
    data <- dplyr::arrange(data, dplyr::desc(created_at))
  }

  # Save updated data
  added_tweets <- nrow(data) - existing_tweets
  message(paste0(added_tweets, " tweets downloaded for ", user, "."))
  message(paste0("### Saving retrieved data for ", user, "."))
  saveRDS(data, file = datafile)
  return(TRUE)
}


#' Update multiple Twitter users' timelines
#'
#' This function updates multiple Twitter users' timelines with \link{rtweet}'s
#' get_timeline() function. It makes use of the \link{update_twitter_user}
#' function in this package. For each user in the provided character vector
#' users it looks for a previously stored data.frame (stored in the datadir
#' under the name provided in the named vector "users" with the file extension
#' .rds). If there is one, it tries to scrape newer and older tweets than the
#' ones already stored in the data.frame. Otherwise it creates a new
#' data.frame.
#'
#' @param users A named character vector. Names are the data file names,
#'   characters are the Twitter user names that will be updated (see example).
#' @param datadir A directory containing the stored data sets (as "name.rds").
#' @param n The number of posts to scrape from each page, defaults to 100.
#' @param token Provide a token, defaults to NULL.
#' @param debug Show more messages during the updating process, default is
#' FALSE.
#'
#' @return TRUE for all pages the update went through.
#'
#' @export
#'
#' @examples
#' # You have to authenticate with Twitter's API first. For more in this visit
#' # rtweet documentation.
#' rtweet::create_token(app = "R_app",
#'                      consumer_key = "1234",
#'                      consumer_secret = "1234",
#'                      access_token = "1234",
#'                      access_secret = "1234")
#'
#' # A character vector is created
#' my_users <- c(cnn = "CNN",
#'               bbc_world = "BBCWorld")
#'
#' # CNN's and BBC's Twitter walls are updated and the results are stored in
#' # the user's home directory in the folder "temp". They are named cnn.rds and
#' # bbc_world.rds. If they do not already exist, both data sets are created.
#' update_twitter_users(users = my_users, datadir = "~/temp")
update_twitter_users <- function(users, datadir = "./raw-data", n = 100,
                                 token = NULL, debug = FALSE) {
  if (!require(purrr)) {stop("Package purrr is not installed.")}

  # Checking parameters
  finished <- FALSE
  if (missing(users)) {
    stop("No (named) character vector containing Twitter accounts provided.")
  }
  # Creating data directory if necessary
  if (!dir.exists(datadir)) {
    message(paste0(datadir," does not exist. Creating a new directory ..."))
    dir.create(datadir)
  }
  # Reading data set names and creating full paths to files
  datafiles <- names(users)
  if (is.null(datafiles)) {
    stop("Provided character vector is not named. See documentation for an
         example.")
  }
  datafiles <- file.path(datadir, paste0(datafiles, ".rds"))
  # Run update for every account
  finished <- purrr::map2(users, datafiles, update_twitter_user,
                          n = n, token = token, debug = debug)
  return(finished)
}
