## load libraries
library(tidyverse)
library(nbastatR)


## get all regular season game logs
rs_games <- get_game_logs(seasons=1980:2017, 
                          result_type='team',
                          season_types='Regular Season')


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


head(rs_games)
get_game_logs()
## add rolling-mean and cummulative-mean metrics
rs_games <- rs_games %>%
  
  ## general rolling and cumulative means
  group_by(slugSeason, slugTeam) %>%
  arrange(date) %>%
  mutate(
    n_gen = 0:(n()-1),
    
    rqP_rollmean5_gen = lag(rollmean(rqP, k=5, fill=NA, align='right'), n=1),
    rqP_rollmean10_gen = lag(rollmean(rqP, k=10, fill=NA, align='right'), n=1),
    rqP_cummean_gen = lag(cummean(rqP), n=1),
    
    rqPA_rollmean5_gen = lag(rollmean(rqPA, k=5, fill=NA, align='right'), n=1),
    rqPA_rollmean10_gen = lag(rollmean(rqPA, k=10, fill=NA, align='right'), n=1),
    rqPA_cummean_gen = lag(cummean(rqPA), n=1),
    
    rqP_rollmean5_gen_siteadj = ifelse(site=='home', rqP_rollmean5_gen + 1, rqP_rollmean5_gen - 1),
    rqP_rollmean10_gen_siteadj = ifelse(site=='home', rqP_rollmean10_gen + 1, rqP_rollmean10_gen - 1),
    rqP_cummean_gen_siteadj = ifelse(site=='home', rqP_cummean_gen + 1, rqP_cummean_gen - 1),
    
    rqPA_rollmean5_gen_siteadj = ifelse(site=='away', rqPA_rollmean5_gen + 1, rqPA_rollmean5_gen - 1),
    rqPA_rollmean10_gen_siteadj = ifelse(site=='away', rqPA_rollmean10_gen + 1, rqPA_rollmean10_gen - 1),
    rqPA_cummean_gen_siteadj = ifelse(site=='away', rqPA_cummean_gen + 1, rqPA_cummean_gen - 1)
  ) %>%
  
  
  ## site-specific rolling and cumulative means
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    n_site = 0:(length(date)-1),
    
    rqP_rollmean5_site = lag(rollmean(rqP, k=5, fill=NA, align='right'), n=1),
    rqP_rollmean10_site = lag(rollmean(rqP, k=10, fill=NA, align='right'), n=1),
    rqP_cummean_site = lag(cummean(rqP), n=1),
    
    rqPA_rollmean5_site = lag(rollmean(rqPA, k=5, fill=NA, align='right'), n=1),
    rqPA_rollmean10_site = lag(rollmean(rqPA, k=10, fill=NA, align='right'), n=1),
    rqPA_cummean_site = lag(cummean(rqPA), n=1)
  ) %>%
  
  
  ## conference-specific rolling and cumulative means
  group_by(season, team, o_cnf) %>%
  arrange(date) %>%
  mutate(
    n_cnf = 0:(length(date)-1),
    
    rqP_rollmean5_cnf = lag(rollmean(rqP, k=5, fill=NA, align='right'), n=1),
    rqP_rollmean10_cnf = lag(rollmean(rqP, k=10, fill=NA, align='right'), n=1),
    rqP_cummean_cnf = lag(cummean(rqP), n=1),
    
    rqPA_rollmean5_cnf = lag(rollmean(rqPA, k=5, fill=NA, align='right'), n=1),
    rqPA_rollmean10_cnf = lag(rollmean(rqPA, k=10, fill=NA, align='right'), n=1),
    rqPA_cummean_cnf = lag(cummean(rqPA), n=1),
    
    rqP_rollmean5_cnf_siteadj = ifelse(site=='home', rqP_rollmean5_cnf + 1, rqP_rollmean5_cnf - 1),
    rqP_rollmean10_cnf_siteadj = ifelse(site=='home', rqP_rollmean10_cnf + 1, rqP_rollmean10_cnf - 1),
    rqP_cummean_cnf_siteadj = ifelse(site=='home', rqP_cummean_cnf + 1, rqP_cummean_cnf - 1),
    
    rqPA_rollmean5_cnf_siteadj = ifelse(site=='away', rqPA_rollmean5_cnf + 1, rqPA_rollmean5_cnf - 1),
    rqPA_rollmean10_cnf_siteadj = ifelse(site=='away', rqPA_rollmean10_cnf + 1, rqPA_rollmean10_cnf - 1),
    rqPA_cummean_cnf_siteadj = ifelse(site=='away', rqPA_cummean_cnf + 1, rqPA_cummean_cnf - 1)    
  )  

# simple moving average
# 


