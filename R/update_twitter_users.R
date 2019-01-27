#' Update or get tweets from a Twitter wall
#'
#' This function updates one Twitter user's wall with \link{rtweet}'s
#' get_timeline() function. It looks for a previously stored data.frame
#' (readable by readRDS). If there is one, it tries to scrape newer and older
#' tweets than the ones already stored in the data.frame. Otherwise it creats a
#' new data.frame.
#'
#' @param user A twitter user name or ID.
#' @param datafile The full path to an RDS data file.
#' @param n The number of tweets to scrape from the wall, defaults to 100.
#' @param token Provide a token, defaults to NULL.
#' @param max_repeats The number of repeated calls to scrape data backwards. If
#'   go_back is TRUE, there will be repeated calls to go back in time (each call
#'   gets the number of posts in n_posts). The backward scraping will stop after
#'   max_repeats is reached. Defaults to 100.
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
                                max_repeats = 100, debug = FALSE) {
  message(paste0("### Updating Twitter timeline for ", user, "."))
  existing_tweets <- 0
  # Load necessary libraries
  if (!require(dplyr)) { stop("Package dplyr is missing.") }
  if (!require(rtweet)) { stop("Package rtweet is missing.") }
  if (!require(digest)) { stop("Package digest is missing.")}

  # Check parameters
  if (missing(user)) {
    stop("Parameter user is missing, please provide a Twitter user ID or name.")
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
    if (nrow(data) < 1) {
      message("No tweets downloadable.")
      return(FALSE)
    }

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
      if (nrow(data) < 1) {
        message("No new tweets downloadable.")
        return(FALSE)
      }

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

    message(paste0(datafile, " not found. First run for ", user, " ..."))
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
    if (nrow(data) < 1) {
      message("No tweets downloadable.")
      return(FALSE)
    }
  }

  # Get older tweets
  message("Try to get older data.")
  repeat_counter <- 0
  hash <- ""

  repeat {
    # counter is increased by 1
    repeat_counter <- repeat_counter + 1

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
    if (nrow(older_data) < 1) {
      message("No old tweets downloadable.")
    }

    if (debug) {
      message(paste0("DEBUG: Number of retrieved older tweets: ",
                     nrow(older_data)))
    }

    # hash from previous run is set as previous
    previous_hash <- hash
    # hash from this run is computed
    hash <- digest::digest(older_data, algo = "md5")

    # Debug messages
    if (debug) {
      message(paste0("DEBUG: Loop for getting older data ran for ",
                     repeat_counter, " time(s)."))
      message(paste0("DEBUG: Previously oldest ID is ",
                     data[nrow(data), c("status_id")],
                     "."))
      message(paste0("DEBUG: Previously oldest post is from ",
                     min(data$created_time), "."))
    }

    if (nrow(older_data) <= 1) {
      break
    }

    # Debug messages
    if (debug) {
         message(paste0("DEBUG: Now downloaded oldest ID is ",
                     older_data[nrow(older_data), c("status_id")], "."))
      message(paste0("DEBUG: Now downloaded oldest post is from ",
                     min(older_data$created_time), "."))
    }

    # Check if retrieved data did not change after last loop
    if (hash == previous_hash)
    {
      message(paste0("Loop produced the same result twice. Emergency break!"))
      break
    }

    message(paste0("There is still older data. Adding up to ", n,
                   " old tweets."))
    # Combine older data with existing data
    data <- dplyr::bind_rows(data, older_data)
    data <- dplyr::distinct(data, status_id, .keep_all = TRUE)
    data <- dplyr::arrange(data, dplyr::desc(created_at))

    # Break if max_repeats are reached
    if (repeat_counter >= max_repeats) {
      message("Loop ran for ", repeat_counter,
              " time(s). Max. number of repeats has been reached!")
      break
    }

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
#' @param max_repeats The number of repeated calls to scrape data backwards. If
#'   go_back is TRUE, there will be repeated calls to go back in time (each call
#'   gets the number of posts in n_posts). The backward scraping will stop after
#'   max_repeats is reached. Defaults to 100.
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
                                 token = NULL, max_repeats = 100,
                                 debug = FALSE) {
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
                          n = n, token = token, max_repeats = max_repeats,
                          debug = debug)
  return(finished)
}
