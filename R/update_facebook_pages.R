#' Update or get posts from a Facebook page
#'
#' This function updates one Facebook page with \link{Rfacebook}'s getPage. It
#' looks for a previously stored data.frame (readable by readRDS, e.g.,
#' bbcnews.rds). If there is one, it tries to scrape newer (and older) posts
#' than the ones already stored in the data.frame. Otherwise it creats a new
#' data.frame.
#'
#' @param page A page name or ID.
#' @param token Either a temporary access token created at
#'   https://developers.facebook.com/tools/explorer or the OAuth token created
#'   with fbOAuth.
#' @param datafile The full path to a RDS data file.
#' @param go_back Go back in time, not only update newer posts, but also get
#'   older ones. Default is TRUE.
#' @param n_posts The number of posts to scrape from the page, defaults to 100.
#' @param feed If TRUE, the function will also return posts on the page that
#'   were made by others (not only the admin of the page).
#' @param reactions If TRUE, will add variables to the data frame with the total
#'   count of reactions: love, haha, wow, sad, angry.
#' @param max_repeats The number of repeated calls to scrape data backwards. If
#'   go_back is TRUE, there will be repeated calls to go back in time (each call
#'   gets the number of posts in n_posts). The backward scraping will stop after
#'   max_repeats is reached. Defaults to 100.
#' @param debug If TRUE, more information will be printed.

#'
#' @return TRUE if it ran through.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' # You have to authenticate with Facebook's API first. For more in this visit
#' # Rfacebooks documentation.
#' fb_oauth <- Rfacebook::fbOAuth(app_id="123456789",
#'                                app_secret="1A2B3C4D",
#'                                extended_permissions = TRUE)
#'
#' # The BBC news page's posts are updated and stored in the user's home
#' # folder in the directory "temp" as "bbc.rds". If the file does not already
#' # exist a new file is created
#' update_page("bbcnews", fb_oauth, "~/temp/bbc.rds")
#'
#' # The data set can be loaded with readRDS
#' readRDS("~/temp/bbc.rds")
#' }
update_page <- function(page, token, datafile, go_back = TRUE,
                        n_posts = 100, feed = FALSE, reactions = FALSE,
                        max_repeats = 100, debug = FALSE) {

  message(paste0("### Updating Facebook posts for ", page, "."))
  existing_posts <- 0

  # Check parameters
  if (missing(page)) {
    stop("Parameter page is missing, please provide a Facebook page ID or name.")
  }
  if (missing(datafile)) {
    stop("Parameter datafile is missing, please provide a file path.")
  }
  if (missing(token)) {
    stop("Parameter token is missing, please provide a Facebook auth token.")
  }

  # Run for the first time or get new posts until the previously newest post
  # is reached (or initialize)
  if (file.exists(datafile)) {

    message(paste0(datafile, " found. Updating data ..."))
    old_data <- readRDS(datafile)
    existing_posts <- nrow(old_data)

    # If scrape_time (data from old versions) does not exist, add empty column
    if (!any(colnames(old_data) == "scrape_time")) {
      old_data[, "scrape_time"] <- as.POSIXct(character(0))
    }

    # # oldest post
    # oldest <- substr(min(old_data$created_time), 1, 10)
    # # newest post
    # newest <- substr(max(old_data$created_time), 1, 10)
    # # Issue 158: https://github.com/pablobarbera/Rfacebook/issues/158
    # #newest <- as.numeric(lubridate::ymd_hms(max(page_data$created_time)))

    data <- tryCatch(
      {
        Rfacebook::getPage(page = page, token = token, n = n_posts,
                           reactions = reactions, feed = feed)
      # }, warning = function(w) {
      #   message(paste0("A warning occured while calling getPage: ", w))
      #   NULL # Return NULL
      }, error = function(e) {
        message(paste0("An error occured while calling getPage: ", e))
        NULL # Return NULL
      })
    if (is.null(data)) { return(FALSE) } # Stop function and return FALSE
    if (nrow(data) < 1) {
      message("No posts downloadable.")
      return(FALSE)
    }
    data[, "scrape_time"] <- Sys.time()

    data <- dplyr::arrange(data, dplyr::desc(.data$created_time))

    if (debug) {
      message(paste0("DEBUG: Newest post from pre-existing data: ",
                     max(old_data$created_time)))
    }
    if (debug) {
      message(paste0("DEBUG: Oldest post from new data: ",
                     min(data$created_time)))
    }

    # Hash for data downloaded in first run.
    hash <- digest::digest(data, algo = "md5")
    # Get new data until newest date from pre-existing data is reached
    repeat_counter <- 0
    while (min(data$created_time) > max(old_data$created_time)) {

      message(paste0("Newer data found for ", page, ". Downloading ", n_posts,
                     " new posts."))

      oldest_in_data <- substr(min(data$created_time), 1, 10)
      oldest_in_data <- lubridate::date(oldest_in_data) + 1
      oldest_in_data <- as.character(oldest_in_data)

      if (debug) {
        message(paste0("DEBUG: Oldest date for updating new posts: ", oldest_in_data))
      }

      new_data <- tryCatch(
        {
          Rfacebook::getPage(page, token = token, n = n_posts,
                             reactions = reactions, feed = feed,
                             until = oldest_in_data)
        # }, warning = function(w) {
        #   message(paste0("A warning occured while calling getPage: ", w))
        #   NULL # Return NULL
        }, error = function(e) {
          message(paste0("An error occured while calling getPage: ", e))
          NULL # Return NULL
        })
      if (is.null(new_data)) {
        message("No downloadable data found while trying to get more new data.")
        break
      } # Stop loop
      if (nrow(new_data) < 1) {
        message("No more new posts downloadable.")
        break
      }
      new_data[, "scrape_time"] <- Sys.time()

      repeat_counter <- repeat_counter + 1
      # Debug messages
      if (debug) {
        message(paste0("DEBUG: Loop for getting older data ran for ",
                       repeat_counter, " time(s)."))
        message(paste0("DEBUG: Previously oldest ID is ",
                       data[nrow(data), c("id")],
                       "."))
        message(paste0("DEBUG: Previously oldest post is from ",
                       min(data$created_time), "."))
        message(paste0("DEBUG: Now downloaded oldest ID is ",
                       new_data[nrow(new_data), c("id")], "."))
        message(paste0("DEBUG: Now downloaded oldest post is from ",
                       min(new_data$created_time), "."))
      }

      # hash from previous run is set as previous
      previous_hash <- hash
      # hash from this run is computed
      hash <- digest::digest(new_data, algo = "md5")

      # Check if retrieved data did not change after last loop
      if (hash == previous_hash)
      {
        message(paste0("Loop to get all newer data produced the same result twice. The previously newest Tweet is therfore not reachable (likely because it was deleted). Emergency break!"))
        break
      }

      data <- dplyr::bind_rows(new_data, data)
      data <- dplyr::distinct(data, .data$id, .keep_all = TRUE)
      data <- dplyr::arrange(data, dplyr::desc(.data$created_time))
    }

    # Combine new data with previously retrieved data
    data <- dplyr::bind_rows(data, old_data)
    data <- dplyr::distinct(data, .data$id, .keep_all = TRUE)
    data <- dplyr::arrange(data, dplyr::desc(.data$created_time))

    # else: Run getPage for the first time
  } else {

    message(paste0(datafile, " not found. First run for ", page, " ..."))
    data <- tryCatch(
      {
        Rfacebook::getPage(page = page, token = token, n = n_posts,
                           reactions = reactions, feed = feed)
      # }, warning = function(w) {
      #   message(paste0("A warning occured while calling getPage: ", w))
      #   NULL # Return NULL
      }, error = function(e) {
        message(paste0("An error occured while calling getPage: ", e))
        NULL # Return NULL
      })
    if (is.null(data)) { return(FALSE) } # Stop function and return FALSE
    if (nrow(data) < 1) {
      message("No posts downloadable.")
      return(FALSE)
    }
    data[, "scrape_time"] <- Sys.time()
  }

  # Get older posts
  if (go_back) {
    message("Try to get older data.")
    repeat_counter <- 0
    oldest_id <- data[nrow(data), c("id")]
    hash <- ""

    repeat {
      # counter is increased by 1
      repeat_counter <- repeat_counter + 1

      # Date of oldest post in existing data
      oldest <- substr(min(data$created_time), 1, 10)
      oldest <- lubridate::date(oldest)+1
      oldest <- as.character(oldest)
      # Issue 158: https://github.com/pablobarbera/Rfacebook/issues/158
      # oldest <- as.numeric(lubridate::ymd_hms(min(page_data$created_time)))

      if (debug) {
        message(paste0("DEBUG: Oldest post for getting older posts: ", oldest))
      }

      # Get older tweets
      older_data <- tryCatch(
        {
          Rfacebook::getPage(page, token = token, n = n_posts,
                             reactions = reactions, feed = feed,
                             until = oldest)
        # }, warning = function(w) {
        #   message(paste0("A warning occured while calling getPage: ", w))
        #   NULL # Return NULL
        }, error = function(e) {
          message(paste0("An error occured while calling getPage: ", e))
          NULL # Return NULL
        })
      if (is.null(older_data)) {
        # break loop if an error occurred
        break
      }
      if (nrow(older_data) < 1) {
        message("No older posts downloadable.")
        break
      }
      older_data[, "scrape_time"] <- Sys.time()

      if (debug) {
        message(paste0("DEBUG: Number of retrieved older posts: ",
                       nrow(older_data)))
      }

      # oldest ID from existing data or previous run is set as previous
      previous_oldest_id <- oldest_id
      # oldest ID from this run
      oldest_id <- older_data[nrow(older_data), c("id")]
      # hash from previous run is set as previous
      previous_hash <- hash
      # hash from this run is computed
      hash <- digest::digest(older_data, algo = "md5")

      # Debug messages
      if (debug) {
        message(paste0("DEBUG: Loop for getting older data ran for ",
                       repeat_counter, " time(s)."))
        message(paste0("DEBUG: Previously oldest ID is ", previous_oldest_id,
                       "."))
        message(paste0("DEBUG: Previously oldest post is from ",
                       min(data$created_time), "."))
        message(paste0("DEBUG: Now downloaded oldest ID is ", oldest_id, "."))
        message(paste0("DEBUG: Now downloaded oldest post is from ",
                       min(older_data$created_time), "."))
      }

      # No more older tweets returned: break loop
      if (oldest_id == previous_oldest_id) {
        message(paste0("Oldest post downloaded for ", page, " is from ",
                       min(older_data$created_time), "."))
        break
      }

      # Check if retrieved data did not change after last loop
      if (hash == previous_hash)
      {
        message(paste0("Loop produced the same result twice. Emergency break!"))
        break
      }

      message(paste0("There is still older data. Adding up to ", n_posts,
                     " old posts"))

      # Combine older data with existing data
      data <- dplyr::bind_rows(data, older_data)
      data <- dplyr::distinct(data, .data$id, .keep_all = TRUE)
      data <- dplyr::arrange(data, dplyr::desc(.data$created_time))

      # Break if max_repeats are reached
      if (repeat_counter >= max_repeats) {
        message("Loop ran for ", repeat_counter,
                " time(s). Max. number of repeats has been reached!")
        break
      }
    }
  }
  # Save updated data
  added_posts <- nrow(data) - existing_posts
  message(paste0(added_posts, " posts downloaded for ", page, "."))
  message(paste0("### Saving retrieved data for ", page, "."))
  saveRDS(data, file = datafile)
  return(TRUE)
}


#' Update multiple Facebook pages
#'
#' This function updates multiple Facebook pages with \link{Rfacebook}'s
#' getPage. makes use of the \link{update_page} function in this package. For
#' each page in the provided character vector pages it looks for a previously
#' stored data.frame (stored in the datadir under the name provided in the named
#' vector pages with the file extension .rds). If there is one, it tries to
#' scrape newer (and older) posts than the ones already stored in the
#' data.frame. Otherwise it creats a new data.frame.
#'
#' @param pages A named character vector. Names are the data file names,
#'   characters are the page names that will be updated (see example).
#' @param datadir A directory containing the stored data sets (as "name.rds").
#' @param token Either a temporary access token created at
#'   https://developers.facebook.com/tools/explorer or the OAuth token created
#'   with fbOAuth.
#' @param go_back Go back in time, not only update newer posts, but also get
#'   older ones. Default is TRUE.
#' @param n_posts The number of posts to scrape from each page, defaults to 100.
#' @param feed If TRUE, the function will also return posts from each page that
#'   were made by others (not only the admin of the page).
#' @param reactions If TRUE, will add variables to the data frame with the total
#'   count of reactions: love, haha, wow, sad, angry.
#' @param max_repeats The number of repeated calls to scrape data backwards. If
#'   go_back is TRUE, there will be repeated calls to go back in time (each call
#'   gets the number of posts in n_posts). The backward scraping will stop after
#'   max_repeats is reached. Defaults to 100.
#' @param debug If TRUE, more information will be printed.
#' @return TRUE for all pages the update went through.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # You have to authenticate with Facebook's API first. For more in this visit
#' # Rfacebooks documentation.
#' fb_oauth <- Rfacebook::fbOAuth(app_id="123456789",
#'                                app_secret="1A2B3C4D",
#'                                extended_permissions = TRUE)
#'
#' # A character vector is created
#' my_pages <- c(cnn = "cnn",
#'               bbc = "bbcnews")
#' # The two pages CNN and BBC news are updated and the results are stored in
#' # the user's home directory in the folder "temp". They are named cnn.rds and
#' # bbc.rds. If they do not already exist, both data sets are created.
#' update_pages(pages = my_pages, token = fb_outh, datadir = "~/temp")
#' }
update_pages <- function(pages = NULL, token = NULL, datadir = "./data",
                         go_back = TRUE, n_posts = 100, feed = FALSE,
                         reactions = FALSE, max_repeats = 100, debug = FALSE) {
  # Checking parameters
  finished <- FALSE
  if (is.null(pages)) {
    stop("No (named) character vector containing Facebook pages provided.")
  }
  # Creating data directory if necessary
  if (!dir.exists(datadir)) {
    message(paste0(datadir," does not exist. Creating a new directory ..."))
    dir.create(datadir)
  }
  # Reading data set names and creating full paths to files
  datafiles <- names(pages)
  if (is.null(datafiles)) {
    stop("Provided character vector is not named. See documentation for an
         example.")
  }
  datafiles <- file.path(datadir, paste0(datafiles, ".rds"))
  # Run update for every page
  finished <- purrr::map2(pages, datafiles, update_page,
                          token = token, n_posts = n_posts, go_back = go_back,
                          max_repeats = max_repeats, debug = debug)
  return(finished)
}
