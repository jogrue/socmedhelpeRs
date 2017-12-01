# RfacebookHelperFunctions
Some functions I use get updates from Facebook pages with
[Rfacebook](https://github.com/pablobarbera/Rfacebook).

It is installed from within R using
[devtools](https://github.com/hadley/devtools):


## Install
```R
library(devtools)
devtools::install_github("jogrue/jogRu")

# I also recommend updating these packages.
devtools::install-github("pablobarbera/Rfacebook/Rfacebook")
devtools::install_github("r-lib/httr")
```



## Usage

```R
# You have to authenticate with Facebook's API first. For more in this visit
# Rfacebooks documentation.
fb_oauth <- fbOAuth(app_id="123456789",
                    app_secret="1A2B3C4D",
                    extended_permissions = TRUE)

# A character vector is created
my_pages <- c(cnn = "cnn",
              bbc = "bbcnews")

# The two pages CNN and BBC news are updated and the results are stored in
# the user's home directory in the folder "temp". They are named cnn.rds and
# bbc.rds. If they do not already exist, both data sets are created.
update_pages(pages = my_pages, token = fb_outh, datadir = "~/temp")

# Only the BBC News page is updated in the data set stored in bbc.rds.
update_page(pagename = "bbcnews", token = fb_OAuth, datafile = "~/temp/bbc.rds")
```