## load libraries
library(tidyverse)
library(nbastatR)


## get all regular season game logs
rs_games <- get_game_logs(seasons=1980:2017, 
                          result_type='team',
                          season_types='Regular Season')

## get all playoffs season game logs
ps_games <- get_game_logs(seasons=1980:2017,
                          result_type='team',
                          season_types='Playoffs')


## define a function that adds point margin column
add_points_margin <- function(games_df) {
  ps_x <- games_df[ , c('idGame', 'nameTeam', 'ptsTeam')]
  ps_y <- games_df[ , c('idGame', 'nameTeam', 'ptsTeam')]
  names(ps_y) <- c('idGame', 'nameOpponent', 'ptsOpponent')
  ps_z <- merge(ps_x, ps_y, by='idGame')
  ps_z <- subset(ps_z, nameTeam != nameOpponent)
  ps_z$ptsMrgn <- ps_z$ptsTeam - ps_z$ptsOpponent
  ps_z <- ps_z[ , c('idGame', 'nameTeam', 'ptsMrgn')]
  games_df <- dplyr::left_join(games_df, ps_z, by=c('idGame', 'nameTeam'))
  return(games_df)
}


## add points margin column in the game logs dataset
rs_games <- add_points_margin(rs_games)
ps_games <- add_points_margin(ps_games)



##
rs_games %>%
  filter(outcomeGame=='W') %>%
  group_by(locationGame) %>%
  summarise(q25 = quantile(ptsMrgn, probs=0.25),
            q50 = quantile(ptsMrgn, probs=0.5),
            q75 = quantile(ptsMrgn, probs=0.75),
            avg = mean(ptsMrgn),
            n = n()) 
  
ps_games %>%
  filter(outcomeGame=='W') %>%
  group_by(locationGame) %>%
  summarise(q25 = quantile(ptsMrgn, probs=0.25),
            q50 = quantile(ptsMrgn, probs=0.5),
            q75 = quantile(ptsMrgn, probs=0.75),
            avg = mean(ptsMrgn),
            n = n()) 
