library(tidyverse)
library(toRvik)
library(withr)

update_season_schedule <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    } else {
      names <- c("date", "type", "neutral", "home", "away", "game_id")
      x <- readr::read_csv(paste0("https://barttorvik.com/", year, "_master_sked.csv"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(c(2, 3, 4, 6, 5, 3, 1))
      colnames(x) <- names
      x <- x %>% dplyr::mutate(
        date = lubridate::mdy(date),
        type = dplyr::case_when(
          type == 0 ~ "nc",
          type == 1 ~ "conf",
          type == 2 ~ "conf_t",
          type == 3 ~ "post",
          TRUE ~ "nond1"
        ),
        neutral = dplyr::case_when(
          neutral == 1 ~ TRUE,
          TRUE ~ FALSE
        )
      )
      return(x)
    }
  })
}

# get data
schedule <- update_season_schedule()

# set wd
setwd('~/torvik-data')

# save
write_csv(schedule, 'season_schedule/season_schedule_2023.csv')

# push to GH
system('git pull')
system('git add .')
system("git commit -m '[VM PUSH] update season schedule'")
system('git push')