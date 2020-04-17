# socmedhelpeRs
This is an R package with functions to retrieve updates from Facebook pages
with [Rfacebook](https://github.com/pablobarbera/Rfacebook/Rfacebook) and from
Twitter feeds with [rtweet](https://github.com/mkearney/rtweet).

ATTENTION: Because of Facebook's API changes you now need an app that was
successfully reviewed by Facebook. Because of this, Rfacebook is not maintained
anymore and support for Facebook might break at some point.

This package can be installed from within R using
[devtools](https://github.com/hadley/devtools):


## Install

```R
library(devtools)

# I recommend to install/update these dependencies from GitHub
devtools::install_github("pablobarbera/Rfacebook/Rfacebook")
devtools::install_github("mkearney/rtweet")

# Install the socmedhelpeRs package from GitHub
devtools::install_github("jogrue/socmedhelpeRs")
```



## Usage

```R
# Loading the package
library("socmedhelpeRs")


# Twitter -----------------------------------------------------------------

# You have to authenticate with Twitter's API first. For more on this visit
# rtweet documentation.
rtweet::create_token(app = "R_app",
                     consumer_key = "1234",
                     consumer_secret = "1234",
                     access_token = "1234",
                     access_secret = "1234")

# A named character vector with accounts to scrape is created. Names are then 
# used as filenames for the rds data files. You can use Twitter screen names or
# Twitter user IDs (which is preferrable because names could change).
my_users <- c(cnn = "CNN",
              bbc_world = "742143")

# CNN's and BBC's Twitter walls are updated and the results are stored in
# the user's home directory in the folder "temp". They are named cnn.rds and
# bbc_world.rds. If they do not already exist, both datasets are created.
update_twitter_users(users = my_users, datadir = "~/temp")

# If you run the update function again it will access the now existing files and
# try to get newer Tweets (and older ones if you did not download all of them
# before) for the accounts in my_users.
update_twitter_users(users = my_users, datadir = "~/temp")


# Facebook ----------------------------------------------------------------

### Facebook stopped working if you do not have an app that has been reviewed
### by Facebook.

# You have to authenticate with Facebook's API first. For more on this visit
# Rfacebooks documentation.
fb_oauth <- fbOAuth(app_id="123456789",
                    app_secret="1A2B3C4D",
                    extended_permissions = TRUE)

# A named character vector with accounts to scrape is created. Names are later 
# used as filenames for the rds data files. Again, you can use the Facebook
# handle or the Facebook ID (which is preferrable because the handle could
# change).
my_pages <- c(cnn = "cnn",
              bbc = "228735667216")

# The two pages CNN and BBC News are updated and the results are stored in
# the user's home directory in the folder "temp". They are named cnn.rds and
# bbc.rds. If they do not already exist, both datasets are created.
update_pages(pages = my_pages, token = fb_oauth, datadir = "~/temp")

# If you run the update function again it will access the now existing files and
# try to get newer posts (and older ones if you did not download all of them
# before) for the accounts in my_pages.
update_pages(pages = my_pages, token = fb_oauth, datadir = "~/temp")

# Only the BBC News page is updated based on the dataset stored in bbc.rds.
update_page(pagename = "bbcnews", token = fb_OAuth, datafile = "~/temp/bbc.rds")
```
