library(tidyverse)
library(rvest)
library(withr)

get_archive <- function(date) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    t_date <- lubridate::ymd(date)
    if (t_date < as.Date("2014-11-01")) {
      cli::cli_abort("Data only goes back to 2014-11-01!")
    }
    if (isTRUE(grepl("-", date))) {
      cli::cli_abort("Please enter a date in YYYYMMDD format with no hyphens")
    }
    names <- c(
      "rk", "team", "conf", "rec", "adj_o", "adj_o_rk", "adj_d", "adj_d_rk", "barthag", "proj_rec", "proj_conf_rec",
      "wab", "wab_rk", "cur_rk", "change"
    )
    x <- httr::GET(paste0("https://barttorvik.com/trank-time-machine.php?date=", date)) %>%
      httr::content(as = "text") %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      purrr::pluck(1)
    x <- x %>%
      subset(select = -c(16:ncol(x)))
    colnames(x) <- names
    x <- x %>%
      dplyr::mutate(across(c(1, 5:9, 12:15), as.numeric),
                    date = lubridate::ymd(date), .after = last_col()
      ) %>%
      dplyr::filter(!is.na(adj_o))
    return(x)
  })
}

# set date
Sys.setenv(TZ='EST')

# set date for function
date_to_pull <- gsub('-', '', Sys.Date() - 1)

# pull ratings
archive <- get_archive(date_to_pull)

# set wd
setwd('~/torvik-data')

# write file
write.csv(
  archive,
  paste0('ratings/archive/by_date/ratings_archive_', gsub('-', '', Sys.Date() - 1), '.csv')
)

# push to repo
system('git pull')
system('git add .')
system("git commit -m '[VM PUSH] update archive ratings'")
system('git push')
