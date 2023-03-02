library(tidyverse)
library(janitor)
library(rvest)
library(withr)

update_game_factors <- function(year = current_season()) {
  suppressWarnings({
  withr::local_options(HTTPUserAgent='toRvik Package')
  if (!(is.numeric(year) && nchar(year) == 4 && year >=
    2008)) {
    cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
  } else {
    names <- c(
      "date", "type", "team", "conf", "opp", "loc", "result", "adj_o", "adj_d", "off_ppp", "off_efg", "off_to", "off_or", "off_ftr", "def_ppp",
      "def_efg", "def_to", "def_or", "def_ftr", "game_score", "opp_conf", "season", "tempo", "game_id", "coach", "opp_coach", "avg_marg", "opp_avg_marg"
    )
    x <- readr::read_csv(paste0("https://barttorvik.com/getgamestats.php?year=", year, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>% dplyr::select(-c(22, 30:31))
    colnames(x) <- names
    x <- x %>%
      dplyr::mutate(
        date = lubridate::mdy(date),
        type = dplyr::case_when(
          type == 0 ~ "nc",
          type == 1 ~ "conf",
          type == 2 ~ "conf_t",
          type == 3 ~ "post",
          TRUE ~ "nond1"
        )
      ) %>%
      dplyr::relocate("opp_conf", .after = "opp") %>%
      dplyr::relocate("avg_marg", .after = "result") %>%
      dplyr::relocate("opp_avg_marg", .after = "avg_marg") %>%
      dplyr::arrange(desc(date))
    return(x)
  }
})}

# save to object
factors <- update_game_factors(2023)

# set wd
setwd('~/torvik-data')

# write to .csv
write.csv(
  factors,
  file = 'game_factors/game_factors_2023.csv'
)

# push to github
system('git pull')
system('git add game_factors/game_factors_2023.csv')
system("git commit -m '[VM PUSH] update game factors'")
system('git push')
