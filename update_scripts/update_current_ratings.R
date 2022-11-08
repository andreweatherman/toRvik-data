library(tidyverse)
library(janitor)
library(rvest)
library(withr)

# define function
get_ratings <- function(year) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if (!(is.numeric(year) && nchar(year) == 4 && year >=
          2008)) {
      cli::cli_abort("Enter a valid year as a number (YYYY). Data only goes back to 2008!")
    } else {
      x_names <- c("team", "barthag", "adj_o", "adj_d", "adj_t", "wab")
      y_names <- c(
        "team", "seed", "conf", "nc_elite_sos", "nc_fut_sos", "nc_cur_sos",
        "ov_elite_sos", "ov_fut_sos", "ov_cur_sos"
      )
      x <- readr::read_csv(paste0("https://barttorvik.com/trank.php?year=", year, "&csv=1"), col_names = FALSE, show_col_types = FALSE) %>%
        dplyr::select(1, 4, 2, 3, 27, 35)
      colnames(x) <- x_names
      y <- httr::GET(paste0("https://barttorvik.com/sos.php?year=", year)) %>%
        httr::content(as = "text") %>%
        rvest::read_html() %>%
        rvest::html_table(header = FALSE) %>%
        purrr::pluck(1) %>%
        janitor::row_to_names(row = 2) %>%
        janitor::clean_names() %>%
        select(-1) %>%
        dplyr::mutate_at(3:8, ~ readr::parse_number(.)) %>%
        tidyr::separate(team,
                        into = c("team", "seed"), sep = "(?<=[A-Za-z.]) (?=[0-9])",
                        convert = TRUE
        )
      colnames(y) <- y_names
      x <- dplyr::left_join(x, y, by = "team") %>%
        dplyr::relocate(conf, .before = barthag) %>%
        dplyr::relocate(seed, .after = last_col()) %>%
        dplyr::mutate(year = year) %>%
        dplyr::arrange(desc(barthag)) %>%
        dplyr::mutate(barthag_rk = row_number(), .after = barthag) %>%
        dplyr::arrange(desc(adj_o)) %>%
        dplyr::mutate(adj_o_rk = row_number(), .after = adj_o) %>%
        dplyr::arrange(adj_d) %>%
        dplyr::mutate(adj_d_rk = row_number(), .after = adj_d) %>%
        dplyr::arrange(desc(adj_t)) %>%
        dplyr::mutate(adj_t_rk = row_number(), .after = adj_t) %>%
        arrange(desc(barthag))
      return(x)
    }
  })
}

# save to object
ratings <- get_ratings(2023)

# write to .csv
readr::write_csv(
  ratings,
  file = '../ratings/ratings_2023.csv'
)

# push to github
system('git add ../ratings/ratings_2023.csv')
system("git commit -m '[VM PUSH] update ratings'")
system('git push')
