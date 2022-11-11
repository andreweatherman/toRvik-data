library(tidyverse)
library(withr)
library(curl)

# define function
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
    curl::curl_download(paste0('https://barttorvik.com/timemachine/team_results/', date, '_team_results.json.gz'), 'archive.json')
    names <- c('rank', 'team', 'conf', 'record', 'barthag', 'adj_o', 'adj_o_rk', 'adj_d', 'adj_d_rk',
               'adj_tempo', 'adj_tempo_rk', 'proj_record', 'proj_conf_record',
               'wab', 'wab_rk', 'date')
    x <-  jsonlite::fromJSON('archive.json') %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(dplyr::across(c(1, 5:8, 45, 9:14, 42, 43), as.numeric),
                    dplyr::across(11:14, round, 1),
                    adj_tempo_rk = dplyr::dense_rank(desc(V45)), .after = V45,
                    date = lubridate::ymd(date)) %>%
      tidyr::unite('proj_record', 11:12, sep = '-', remove = TRUE) %>%
      tidyr::unite('proj_conf_record', 12:13, sep = '-', remove = TRUE) %>%
      dplyr::select(c(1:4, 9, 5:8, 43:44, 11:12, 40, 41, 45))
    colnames(x) <- names
    unlink('archive.json')
    return(x) }
  )
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
