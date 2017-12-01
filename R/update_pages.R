#' Update or get posts from a Facebook page
#' 
#' This function updates one Facebook page with \link{Rfacebook}'s getPage. It
#' looks for a previously stored data.frame (readable by readRDS, e.g.,
#' bbcnews.rds). If there is one, it tries to scrape newer (and older) posts
#' than the ones already stored in the data.frame. Otherwise it creats a new
#' data.frame.
#' 
#' @param pagename A page name.
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

#'
#' @return TRUE if it ran through.
#' @export
#'
#' @examples
#' # You have to authenticate with Facebook's API first. For more in this visit
#' # Rfacebooks documentation.
#' fb_oauth <- fbOAuth(app_id="123456789",
#'                     app_secret="1A2B3C4D",
#'                     extended_permissions = TRUE)
#' 
#' # The BBC news page's posts are updated and stored in the user's home
#' # folder in the directory "temp" as "bbc.rds". If the file does not already
#' # exist a new file is created
#' update_page("bbcnews", fb_oauth, "~/temp/bbc.rds")
#' 
#' # The data set can be loaded with readRDS
#' readRDS("~/temp/bbc.rds")
#' 
update_page <- function(pagename, token, datafile, go_back = TRUE, n_posts = 100,
                        feed = FALSE, reactions = TRUE) {
  finished <- FALSE
  if (is.null(token))
  {
    stop("No Facebook Auth token provided.")
  }
  if (file.exists(datafile)) {
    page_data <- readRDS(datafile)
    message(paste0(datafile," found. Updating data ..."))
    # oldest post
    oldest <- as.numeric(lubridate::ymd_hms(min(page_data$created_time)))
    # newest post
    newest <- as.numeric(lubridate::ymd_hms(max(page_data$created_time)))
    # Get newer posts
    message(paste0("Try to fetch newer postings for ", pagename, "."))
    update_data <- tryCatch(
      {
        Rfacebook::getPage(pagename, token = token, n = n_posts,
                           reactions = reactions, feed = feed,
                           since = newest + 1)
      }, warning = function(w) {
        warning(paste0("A warning occured during the first run: ", w))
      }, error = function(e) {
        warning(paste0("An error occured during the first run: ", e, "\n",
                       "A second attempt was made."))
        tryCatch(
          {
            Rfacebook::getPage(pagename, token = token, n = n_posts,
                               reactions = TRUE, feed = TRUE,
                               since = newest + 1)
          }, warning = function(w) {
            warning(paste0("A warning occured during the second run: ", w))
          }, error = function(e) {
            stop(paste0("An error occured during the second run:", e))
          }
        )
      }
    )
    page_data <- rbind(page_data, update_data)
    update_data <- NULL
    if (go_back) {
      # Get older postings
      message(paste0("Trying to fetch older postings for ", pagename, "."))
      update_data <- tryCatch(
        {
          Rfacebook::getPage(pagename, token = token, n = n_posts,
                             reactions = TRUE, feed = TRUE,
                             until = oldest - 1)
        }, warning = function(w) {
          warning(paste0("A warning occured during the first run: ", w))
        }, error = function(e) {
          warning(paste0("An error occured during the first run: ", e, "\n",
                         "A second attempt was made."))
          tryCatch(
            {
              Rfacebook::getPage(pagename, token = token, n = n_posts,
                                 reactions = TRUE, feed = TRUE,
                                 until = oldest - 1)
            }, warning = function(w) {
              warning(paste0("A warning occured during the second run: ", w))
            }, error = function(e) {
              stop(paste0("An error occured during the second run:", e))
            }
          )
        }
      )
      page_data <- rbind(page_data, update_data)
      update_data <- NULL
    }
  } else {
    # Creates a new data set
    message(paste0(datafile," not found. Updating data and creating new file ",
                   "..."))
    page_data <- tryCatch(
      {
        Rfacebook::getPage(pagename, token = token, n = n_posts,
                           reactions = TRUE, feed = TRUE)
      }, warning = function(w) {
        warning(paste0("A warning occured during the first run: ", w))
      }, error = function(e) {
        warning(paste0("An error occured during the first run: ", e, "\n",
                       "A second attempt was made."))
        tryCatch(
          {
            Rfacebook::getPage(pagename, token = token, n = n_posts,
                               reactions = TRUE, feed = TRUE)
          }, warning = function(w) {
            warning(paste0("A warning occured during the second run: ", w))
          }, error = function(e) {
            stop(paste0("An error occured during the second run:", e))
          }
        )
      }
    )
  }
  page_data <- dplyr::distinct(page_data)
  page_data <- dplyr::arrange(page_data, created_time)
  saveRDS(page_data, file = datafile)
  finished <- TRUE
  return(finished)
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

#' @return TRUE for all pages the update went through.
#' 
#' @export
#'
#' @examples
#' # You have to authenticate with Facebook's API first. For more in this visit
#' # Rfacebooks documentation.
#' fb_oauth <- fbOAuth(app_id="123456789",
#'                     app_secret="1A2B3C4D",
#'                     extended_permissions = TRUE)
#' 
#' # A character vector is created
#' my_pages <- c(cnn = "cnn",
#'               bbc = "bbcnews")
#' # The two pages CNN and BBC news are updated and the results are stored in
#' # the user's home directory in the folder "temp". They are named cnn.rds and
#' # bbc.rds. If they do not already exist, both data sets are created.
#' update_pages(pages = my_pages, token = fb_outh, datadir = "~/temp")
update_pages <- function(pages = NULL, token = NULL, datadir = "./data",
                         go_back = TRUE, n_posts = 100, feed = FALSE,
                         reactions = TRUE) {
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
                     token = token, n_posts = n_posts, go_back = go_back)
  return(finished)
}