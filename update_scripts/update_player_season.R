library(tidyverse)
library(withr)

# define function
update_player_season <- function(year = current_season(), stat = 'all', conf_only = FALSE) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
      2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008")
    }
    if (!(stat %in% c("box", "shooting", "adv", "all"))) {
      cli::cli_abort("Please input a valid stat command ('box', 'shooting', or 'adv')")
    }
    c_only <- as.integer(conf_only)
    if (stat == "box") {
      names <- c(
        "player", "pos", "exp", "hgt", "team", "conf", "g", "mpg", "ppg", "oreb",
        "dreb", "rpg", "apg", "tov", "ast_to", "spg", "bpg", "num", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr:: mutate(tpg=X61/X36)
      y <- x %>% dplyr::mutate(across(c(17, 18, 20, 21, 43, 44), as.numeric))
      y <- y %>%
        dplyr::group_by(X33) %>%
        dplyr::summarize(
          fga = sum(X18, X21, X44, na.rm = TRUE),
          fgm = sum(X17, X20, X43, na.rm = TRUE),
          fg_pct = fgm / fga
        ) %>%
        dplyr::rename("id" = 1)
      x <- x %>%
        dplyr::select(1, 65, 26, 27, 2:4, 55, 64, 58:61, 67, 36, 62, 63, 28, 32, 33)
      colnames(x) <- names
      x <- dplyr::left_join(x, (y %>% dplyr::select(1, 4)), by = "id") %>%
        dplyr::mutate(exp=as.character(exp),
                      num=as.double(num)) %>%
        dplyr::relocate(fg_pct, .before = oreb) %>%
        dplyr::arrange(desc(ppg)) %>%
        dplyr::mutate(inches = as.numeric(gsub('-.*', '', hgt)) * 12 + as.numeric(gsub('.*-', '', hgt)), .after = hgt)
      return(x)
    }
    if (stat == "shooting") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "mpg", "ppg", "usg", "ortg", "efg", "ts",
        "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
        "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
        "mid_m", "mid_a", "mid_pct", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(
          1, 65, 26, 2:4, 55, 64, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
          42, 32, 33
        )
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(p_per = ((40 * ppg) / mpg), .after = ppg,
                      exp=as.character(exp)) %>%
        dplyr::arrange(desc(ppg))
      return(x)
    }
    if (stat == "adv") {
      names <- c(
        "player", "pos", "exp", "team", "conf", "g", "min", "porpag", "dporpag", "ortg", "adj_oe", "drtg", "adj_de",
        "stops", "obpm", "dbpm", "bpm", "oreb", "dreb", "ast", "to", "blk", "stl", "ftr", "pfr",
        "rec", "pick", "year", "id"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 65, 26, 2:5, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 46, 32, 33)
      colnames(x) <- names
      x <- x %>%
        dplyr::mutate(exp=as.character(exp)) %>%
        dplyr::arrange(desc(rec))
      return(x)
    }
    else {
      names <- c("player", "pos", "exp", "num", "hgt", "team", "conf", "g", "min", "mpg", "ppg", "oreb",
                 "dreb", "rpg", "apg", "tov", "ast_to", "spg", "bpg", "usg", "ortg", "efg", "ts",
                 "ftm", "fta", "ft_pct", "two_m", "two_a", "two_pct", "three_m", "three_a",
                 "three_pct", "dunk_m", "dunk_a", "dunk_pct", "rim_m", "rim_a", "rim_pct",
                 "mid_m", "mid_a", "mid_pct", "porpag", "dporpag", "adj_oe", "drtg", "adj_de",
                 "stops", "obpm", "dbpm", "bpm", "oreb_rate", "dreb_rate", "ast", "to", "blk", "stl", "ftr", "pfr",
                 "rec", "pick", "year", "id")
      x <- readr::read_csv(paste0("https://barttorvik.com/getadvstats.php?year=", year, "&conyes=", c_only, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr:: mutate(tpg=X61/X36) %>%
        dplyr::select(1, 65, 26, 28, 27, 2:5, 55, 64, 58:61, 67, 36, 62, 63, 7, 6, 8, 9, 14:22, 43:45, 37, 38, 41, 39, 40,
                      42, 29, 49, 6, 30, 47, 48, 50, 56, 57, 54, 10:13, 23:25, 31, 35, 46, 32, 33)
      colnames(x) <- names
      y <- x %>%
        dplyr::group_by(id) %>%
        dplyr::summarize(fgm=sum(two_m, three_m, na.rm=TRUE),
                  fga=sum(two_a, three_a, na.rm=TRUE),
                  fg_pct=fgm/fga)
      x <- dplyr::left_join(x, y, by='id')
      x <- x %>%
        dplyr::mutate(exp=as.character(exp),
                      num=as.double(num)) %>%
        dplyr::relocate(c(61:63), .after=ts) %>%
        dplyr::mutate(inches = as.numeric(gsub('-.*', '', hgt)) * 12 + as.numeric(gsub('.*-', '', hgt)), .after = hgt)
      return(x)
    }
  })
}

# run function for each stat type
box <- update_player_season(year=2023, stat='box') 
shooting <- update_player_season(year=2023, stat='shooting')
adv <- update_player_season(year=2023, stat='adv')
all <- update_player_season(year = 2023, stat='all')

# list the df and names of files
to_save <- list(box, shooting, adv, all)
names <- c('box','shooting', 'advanced', 'all')

# save as .RDS
map2(
    .x = to_save,
    .y = names,
    .f = function(x, y) {
        write_rds(x, file = paste0('player_season/', y, '_2023.rds'))
    }
)

# push to github
system('git pull')
system('git add .')
system("git commit -m '[VM] update player season stats'")
system('git push')
