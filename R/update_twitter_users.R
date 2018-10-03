#' Update or get tweets from a Twitter wall
#'
#' This function updates one Twitter user's wall with \link{TwitteR}'s
#' userTimeline() function. It looks for a previously stored data.frame
#' (readable by readRDS). If there is one, it tries to scrape newer (and older)
#' tweets than the ones already stored in the data.frame. Otherwise it creats a new
#' data.frame.
#'
#' @param user A twitter user name.
#' @param datafile The full path to an RDS data file.
#' @param go_back Go back in time, not only update newer tweets, but also get
#'   older ones. Default is TRUE.
#' @param n_posts The number of tweets to scrape from the wall, defaults to 100.
#'
#' @return TRUE if it ran through.
#' @export
#'
#' @examples
#' # You have to authenticate with Twitter's API first. For more in this visit
#' # TwitteR documentation.
#' setup_twitter_oauth(consumer_key = "1234",
#'                     consumer_secret = "1234",
#'                     access_token = "1234",
#'                     access_secret = "1234")
#'
#' # The BBC World news Twitter feed is scraped and stored in the user's home
#' # folder in the directory "temp" as "bbc_world.rds". If the file does not
#' # already exist a new file is created. If it exists, the existing data is
#' # updated.
#' update_twitter_feed("BBCWorld", "~/temp/bbc_world.rds")
#'
#' # The data set can be loaded with readRDS
#' readRDS("~/temp/bbc_world.rds")
update_twitter_user <- function(user, datafile, go_back = TRUE,
                                n_posts = 100) {
  updated <- FALSE
  feed_data <- NULL
  update_data <- NULL
  if (file.exists(datafile)) {
    feed_data <- readRDS(datafile)
    message(paste0(datafile," found. Updating data ..."))
    # oldest tweet
    oldest <- min(feed_data$id)
    # newest tweet
    newest <- max(feed_data$id)
    # Get newer posts
    message(paste0("Trying to fetch new tweets for ", user, "."))
    update_data <- tryCatch(
      {
        twitteR::userTimeline(user = user,
                              sinceID = newest,
                              n = n_posts,
                              includeRts = TRUE,
                              excludeReplies = FALSE)
      }, warning = function(w) {
        warning(paste0("A warning occured during the first run: ", w))
      }, error = function(e) {
        warning(paste0("An error occured during the first run: ", e, "\n",
                       "A second attempt was made."))
        tryCatch(
          {
            twitteR::userTimeline(user = user,
                                  sinceID = newest,
                                  n = n_posts,
                                  includeRts = TRUE,
                                  excludeReplies = FALSE)
          }, warning = function(w) {
            warning(paste0("A warning occured during the second run: ", w))
          }, error = function(e) {
            stop(paste0("An error occured during the second run:", e))
          }
        )
      }
    )
    #update_data <- do.call("rbind", lapply(update_data, as.data.frame))
    if (length(update_data) == 0) {
      message(paste0("No new tweets found for ", user, "."))
    } else {
      update_data <- twitteR::twListToDF(twList = update_data)
      feed_data <- rbind(feed_data, update_data)
      updated <- TRUE
    }
    update_data <- NULL
    if (go_back) {
      # Get older tweets
      message(paste0("Trying to fetch older tweets for ", user, "."))
      update_data <- tryCatch(
        {
          twitteR::userTimeline(user = user,
                                maxID = oldest,
                                n = n_posts,
                                includeRts = TRUE,
                                excludeReplies = FALSE)
        }, warning = function(w) {
          warning(paste0("A warning occured during the first run: ", w))
        }, error = function(e) {
          warning(paste0("An error occured during the first run: ", e, "\n",
                         "A second attempt was made."))
          tryCatch(
            {
              twitteR::userTimeline(user = user,
                                    maxID = oldest,
                                    n = n_posts,
                                    includeRts = TRUE,
                                    excludeReplies = FALSE)
            }, warning = function(w) {
              warning(paste0("A warning occured during the second run: ", w))
            }, error = function(e) {
              stop(paste0("An error occured during the second run:", e))
            }
          )
        }
      )
      if (length(update_data) <= 1) {
        message(paste0("No older tweets found for ", user, "."))
        update_data <- NULL
      } else {
        update_data <- twitteR::twListToDF(twList = update_data)
        feed_data <- rbind(feed_data, update_data)
        updated <- TRUE
      }
    }
  } else {
    # Creates a new data set
    message(paste0(datafile," not found. Updating data and creating new file ",
                   "..."))
    feed_data <- tryCatch(
      {
        twitteR::userTimeline(user = user,
                              n = n_posts,
                              includeRts = TRUE,
                              excludeReplies = FALSE)
      }, warning = function(w) {
        warning(paste0("A warning occured during the first run: ", w))
      }, error = function(e) {
        warning(paste0("An error occured during the first run: ", e, "\n",
                       "A second attempt was made."))
        tryCatch(
          {
            twitteR::userTimeline(user = user,
                                  n = n_posts,
                                  includeRts = TRUE,
                                  excludeReplies = FALSE)
          }, warning = function(w) {
            warning(paste0("A warning occured during the second run: ", w))
          }, error = function(e) {
            stop(paste0("An error occured during the second run:", e))
          }
        )
      }
    )
    #feed_data <- do.call("rbind", lapply(feed_data, as.data.frame))
    if (length(feed_data) == 0) {
      message(paste0("No tweets found for ", user, "."))
    } else {
      feed_data <- twitteR::twListToDF(twList = feed_data)
      updated <- TRUE
    }
  }
  if (updated) {
    feed_data <- dplyr::arrange(feed_data, dplyr::desc(created))
    feed_data <- dplyr::distinct(feed_data, id, .keep_all = TRUE)
    saveRDS(feed_data, file = datafile)
  }
  return(TRUE)
}


#' Update multiple Twitter users' timelines
#'
#' This function updates multiple Twitter users' timelines with \link{TwitteR}'s
#' userTimeline() function. It makes use of the \link{update_twitter_user}
#' function in this package. For each user in the provided character vector
#' users it looks for a previously stored data.frame (stored in the datadir
#' under the name provided in the named vector "users" with the file extension
#' .rds). If there is one, it tries to scrape newer (and older) tweets than the
#' ones already stored in the data.frame. Otherwise it creats a new data.frame.
#'
#' @param users A named character vector. Names are the data file names,
#'   characters are the Twitter user names that will be updated (see example).
#' @param datadir A directory containing the stored data sets (as "name.rds").
#' @param go_back Go back in time, not only update newer posts, but also get
#'   older ones. Default is TRUE.
#' @param n_posts The number of posts to scrape from each page, defaults to 100.

#' @return TRUE for all pages the update went through.
#'
#' @export
#'
#' @examples
#' # You have to authenticate with Twitter's API first. For more in this visit
#' # TwitteR documentation.
#' setup_twitter_oauth(consumer_key = "1234",
#'                     consumer_secret = "1234",
#'                     access_token = "1234",
#'                     access_secret = "1234")
#'
#' # A character vector is created
#' my_users <- c(cnn = "CNN",
#'               bbc_world = "BBCWorld")
#' # CNN's and BBC's Twitter walls are updated and the results are stored in
#' # the user's home directory in the folder "temp". They are named cnn.rds and
#' # bbc_world.rds. If they do not already exist, both data sets are created.
#' update_twitter_users(users = my_users, datadir = "~/temp")
update_twitter_users <- function(users = NULL, datadir = "./raw-data",
                                 go_back = TRUE, n_posts = 100) {
  # Checking parameters
  finished <- FALSE
  if (is.null(users)) {
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
                          n_posts = n_posts, go_back = go_back)
  return(finished)
}
