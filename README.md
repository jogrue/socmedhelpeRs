# RfacebookHelperFunctions
This is an R package with functions to retrieve updates from Facebook pages
with [Rfacebook](https://github.com/pablobarbera/Rfacebook) and from Twitter
feeds with [TwitteR](https://github.com/geoffjentry/twitteR).

ATTENTION: Facebook does not work anymore for me. Because of Facebook's API
changes you now need a reviewed app.

This package can be installed from within R using
[devtools](https://github.com/hadley/devtools):


## Install
```R
library(devtools)
devtools::install_github("jogrue/RfacebookHelperFunctions")

# I also recommend updating these packages.
devtools::install_github("pablobarbera/Rfacebook/Rfacebook")
devtools::install_github("geoffjentry/twitteR")
devtools::install_github("r-lib/httr")
```



## Usage

```R

# You have to authenticate with Twitter's API first. For more on this visit
# TwitteR documentation.
setup_twitter_oauth(consumer_key = "1234",
                    consumer_secret = "1234",
                    access_token = "1234",
                    access_secret = "1234")

# A character vector is created
my_users <- c(cnn = "CNN",
              bbc_world = "BBCWorld")
              
# CNN's and BBC's Twitter walls are updated and the results are stored in
# the user's home directory in the folder "temp". They are named cnn.rds and
# bbc_world.rds. If they do not already exist, both data sets are created.
update_twitter_users(users = my_users, datadir = "~/temp")

### Facebook stopped working if you do not have an app that has been reviewed
### by Facebook.

# You have to authenticate with Facebook's API first. For more on this visit
# Rfacebooks documentation.
fb_oauth <- fbOAuth(app_id="123456789",
                    app_secret="1A2B3C4D",
                    extended_permissions = TRUE)

# A character vector is created
my_pages <- c(cnn = "cnn",
              bbc = "bbcnews")

# Loading the package
library("RfacebookHelperFunctions")

# The two pages CNN and BBC news are updated and the results are stored in
# the user's home directory in the folder "temp". They are named cnn.rds and
# bbc.rds. If they do not already exist, both data sets are created.
update_pages(pages = my_pages, token = fb_outh, datadir = "~/temp")

# Only the BBC News page is updated in the data set stored in bbc.rds.
update_page(pagename = "bbcnews", token = fb_OAuth, datafile = "~/temp/bbc.rds")
```
