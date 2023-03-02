library(toRvik)
library(tidyverse)
library(janitor)
library(rvest)
library(withr)

update_game_box <- function(year = current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    abbrev <- c("fgm", "fga", "tpm", "tpa", "ftm", "fta", "oreb", "dreb", "reb", "ast", "stl", "blk", "to", "pf", "pts")
    names <- c("game_id", "date", "min", "team1", "team2", "pos", "win", "loss")
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number. Data only goes back to 2008!")
    }
    if (dplyr::between(year, 2009, 2014)) {
      x <- jsonlite::fromJSON(paste0("https://barttorvik.com/", year, "_season.json"))
      x <- dplyr::as_tibble(t(sapply(x, `length<-`, max(lengths(x)))))
      x <- x %>%
        dplyr::select(-c(37, 40)) %>%
        dplyr::rename_at(c(1:5, 36:38), ~ paste0(names)) %>%
        dplyr::rename_at(6:20, ~ paste0("team1_", abbrev)) %>%
        dplyr::rename_at(21:35, ~ paste0("team2_", abbrev)) %>%
        dplyr::relocate(game_id, .after = loss) %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          across(c(2, 5:35), as.numeric)
        ) %>%
        dplyr::arrange(desc(date))
      return(x)
    } else {
      x <- jsonlite::fromJSON(paste0("https://barttorvik.com/", year, "_season.json")) %>%
        dplyr::as_tibble()
      x <- x %>%
        dplyr::select(-37) %>%
        dplyr::rename_at(c(1:5, 36:38), ~ paste0(names)) %>%
        dplyr::rename_at(6:20, ~ paste0("team1_", abbrev)) %>%
        dplyr::rename_at(21:35, ~ paste0("team2_", abbrev)) %>%
        dplyr::relocate(game_id, .after = loss) %>%
        dplyr::mutate(
          date = lubridate::mdy(date),
          across(c(2, 5:35), as.numeric)
        ) %>%
        dplyr::arrange(desc(date))
      return(x)
    }
  })
}

box <- update_game_box(2023)

schedule <- bart_season_schedule(2023) |> select(type, game_id)
teams <- bart_teams() |> filter(year == 2023) |> pluck(2)
names(teams) <- bart_teams() |> filter(year == 2023) |> pluck(1)

box <- box |>
  rename(team1_team = team1,
         team2_team = team2) |>
  pivot_longer(
    cols = starts_with("team"),
    # NA drops the first part of the name, .value pivots back wider
    names_to = c(NA,".value"),
    names_sep = "_"
  ) |>
  mutate(did_win = ifelse(win == team, TRUE, FALSE), .after = date) |>
  left_join(schedule, by = 'game_id') |>
  relocate(type, .after = date) |>
  mutate(opp = ifelse(team == win, loss, win), .after = team) |>
  mutate(conf = teams[team], .after = team) |>
  mutate(opp_conf = teams[team], .after = opp) |>
  relocate(min, .after = opp_conf) |>
  relocate(pos, .after = min) |>
  select(-c(win, loss, game_id)) 

# set wd
setwd('~/torvik-data')

# write to .csv
write.csv(
  box,
  file = 'game_box/game_box_2023.csv'
)

# push to github
system('git pull')
system('git add game_box/game_box_2023.csv')
system("git commit -m '[VM PUSH] update game box'")
system('git push')
