library(toRvik)
library(tidyverse)
library(janitor)
library(rvest)
library(withr)
library(lubridate)


update_pregame <- function(year=current_season()) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(!(is.numeric(year) && nchar(year) == 4 && year >=
         2008)){
      cli::cli_abort('Enter a valid year as a number. Data only goes back to 2008!')
    }
    else {
      names <- c('date','conf','line','ttq','type', 'team1','team1_wp','team1_pts','team2','team2_wp','team2_pts','game_id')
      x <- readr::read_csv(paste0('https://barttorvik.com/',year,'_super_sked.csv'), col_names=FALSE, show_col_types = FALSE) %>%
        dplyr::select(2,3,5:7,9,53,14,15,54,20,1)
      colnames(x) <- names
      x <- x %>% dplyr::mutate(type=case_when(type==0~'nc',
                                              type==1~'conf',
                                              type==2~'conf_t',
                                              type==3~'post',
                                              type==99~'nond1'),
                               date=lubridate::mdy(date),
                               across(c(4,7,8,10,11), as.numeric),
                               year=year)
      return(x) }
  })}

# get data
pregame <- update_pregame(2023)

# set wd
setwd('~/torvik-data')

# write to .csv
write.csv(
  pregame,
  file = 'pregame_prob/pregame_2023.csv'
)

# push to github
system('git pull')
system('git add pregame_prob/pregame_2023.csv')
system("git commit -m '[VM PUSH] update pregame probabilities'")
system('git push')
