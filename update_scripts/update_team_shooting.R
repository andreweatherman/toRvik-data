library(tidyverse)
library(withr)
library(rvest)
library(httr)
library(tidyr)

update_team_shooting <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    names <- c("team", "conf", "dunk_fg", "dunk_share", "dunk_fg_d", "dunk_share_d", "close_fg", "close_share", "close_fg_d", "close_share_d", "far_fg", "far_share", "far_fg_d", "far_share_d", "three_fg", "three_share", "three_fg_d", "three_share_d")
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2010)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2010!")
    } else {
      x <- httr::GET(paste0("https://barttorvik.com/teampbptot.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table() %>%
        purrr::pluck(1) %>%
        subset(select = -c(1, seq(7, 22, 5)))
      colnames(x) <- names
      x <- x[!(x$team == ""), ]
      x <- x %>%
        tidyr::separate(team, into = c("team", "seed"), sep = "(?<=\\D) (?=[0-9])") %>%
        dplyr::mutate(across(c(2, seq(5, 19, 2)), as.numeric), year = year)
      return(x)
    }
  })
}

dat <- update_team_shooting(year=2023)

# set wd
setwd('~/torvik-data')

# save as parquet
arrow::write_parquet(dat, sink = 'team_stats/2023/team_shooting_2023.parquet')

# push to github
system('git pull')
system('git add .')
system("git commit -m '[VM PUSH] update team shooting stats'")
system('git push')
