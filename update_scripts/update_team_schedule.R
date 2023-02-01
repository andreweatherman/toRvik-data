library(tidyverse)
library(toRvik)


# set function
update_team_schedule <- function(year=current_season(), team=NULL) {
  suppressWarnings({
    withr::local_options(HTTPUserAgent='toRvik Package')
    if(!(is.numeric(year) && nchar(year) == 4 && year >=
         2008)){
      cli::cli_abort('Enter a valid year as a number. Data only goes back to 2008!')
    }
    else {
      team <- gsub(" ","+", team)
      team <- gsub("&", "%26", team)
      names <- c('date','type','team','conf','opp','loc','result','adj_o','adj_d','ppp','efg',
                 'to','or','ftr','def_ppp','def_efg','def_to','def_or','def_ftr','game_score',
                 'opp_conf','year','poss','game_id','coach','opp_coach','lead_diff')
      x <- readr::read_csv(paste0('https://barttorvik.com/getgamestats.php?year=', year,'&tvalue=',team, '&csv=1')) %>%
        dplyr::select(-c(22, 28:30))
      colnames(x) <- names
      x <- x %>% dplyr::mutate(type=case_when(type==0~'nc',
                                              type==1~'conf',
                                              type==2~'conf_t',
                                              type==3~'post'),
                               date=lubridate::mdy(date)) %>%
        tidyr::separate(result, into=c('result','score'), sep=',') %>%
        tidyr::separate(score, into=c('win','loss'), sep='-') %>%
        dplyr::mutate(points=case_when(result=='W'~win,
                                       TRUE~loss),
                      opp_points=case_when(result=='W'~loss,
                                           TRUE~win),
                      across(c(8:22, 24:25, 29:31), as.numeric)) %>%
        dplyr::select(-c(8:9)) %>%
        dplyr::relocate(points, .after=result) %>%
        dplyr::relocate(opp_points, .after=points) %>%
        dplyr::relocate(opp_conf, .before=loc) %>%
        dplyr::relocate(lead_diff, .after=opp_points) %>%
        relocate(game_id, .after=last_col()) %>%
        arrange(desc(date))
      return(x) }
  }
  )}

# get teams
teams = bart_teams() |> filter(year == 2023) |> pluck(1)

# set wd
setwd('~/torvik-data')

# loop over teams to get data
data <- map(
    .x = teams,
    .f = function(x) {
        update_team_schedule(2023, team = x) 
    }
)

# create one large df
all <- map_dfr(data, .f = function(l) l)

# save each as .csv
map2(
    .x = 1:length(teams),
    .y = teams,
    .f = function(x, y) {
        y = gsub(' ', '_', y)
        arrow::write_csv_arrow(data[[x]], sink = paste0('team_schedule/2023/', y, '_2023.csv'))
    }
)

arrow::write_csv_arrow(all, sink = 'team_schedule/all_2023.csv')

# push to github
system('git pull')
system('git add .')
system("git commit -m '[VM PUSH] update team schedules'")
system('git push')
