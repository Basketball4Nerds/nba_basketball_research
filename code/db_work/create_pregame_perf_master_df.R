## clear environment
rm(list = ls())

## set database credentials
source('../../credentials/aws_db_credentials.R')

## load libraries
library(RPostgreSQL)
library(tidyverse)
library(nbastatR)
library(lubridate)
library(zoo)
library(glue)

## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
#rm(db_pass) 

## list available tables
dbListTables(con)

## read data
team_gamelogs_master_df <- dbReadTable(conn=con, name='team_gamelogs_master_df')


## set vector of post-game metrics
post_game_metrics <- c('fgm', 'fga', 'fgp', 'fg2m', 'fg2a', 'fg2p', 'fg3m', 'fg3a', 'fg3p', 
                       'ftm', 'fta', 'ftp', 'oreb', 'dreb', 'reb', 'ast', 'stl', 'blk', 'tov', 'pf', 'pts')


## set vector of post-game metrics that need to be normalized per 240 minutes
post_game_metrics_for_240_min_nrm <- 
  post_game_metrics %>%
  setdiff(x=., y=c('fgp', 'fg2p', 'fg3p', 'ftp')) %>%
  sprintf("%s_alwd", .) %>%
  c(post_game_metrics_for_240_min_nrm, .)
post_game_metrics_for_240_min_nrm


## define regex pattern for post-game performance metrics
post_game_metrics_regex <- glue("^{post_game_metrics}(_alwd)?$") %>% str_c(., collapse="|")


## create df with only pre-game performance metrics
pregame_perf_df <- team_gamelogs_master_df %>%

  ## look at game logs from 2006 only
  filter(game_date >= '2006-09-01') %>%
  
  ## normalize performance for per 240 minutes
  mutate_at(
    vars(post_game_metrics_for_240_min_nrm),
    funs(. / min * 240)
  ) %>%

  ## general rolling and cumulative means
  group_by(season, team_abbr) %>%
  arrange(season, team_abbr, game_date) %>%
  
  mutate(
    
    ## number of games played
    n_gen = 0:(n()-1),
    
    ## number of wins
    w_gen = lag(cumsum(game_outcome=='W'), n=1, default=0),
    
    ## number of losses
    l_gen = lag(cumsum(game_outcome=='L'), n=1, default=0),

    ## win percentage    
    wp_gen = round(w_gen / n_gen, 3),
    wp_gen = ifelse(is.nan(wp_gen), NA, wp_gen)
    
  ) %>% 
  
  ## general 5-game rolling average
  mutate_at(
    vars(matches(post_game_metrics_regex)),
    funs(rollmean5_gen = lag(rollmean(., k=5, fill=NA, align='right'), n=1))
  ) %>%
  
  ## general 10-game rolling average
  mutate_at(
    vars(matches(post_game_metrics_regex)),
    funs(rollmean10_gen = lag(rollmean(., k=10, fill=NA, align='right'), n=1))
  ) %>%

  ## general cumulative average  
  mutate_at(
    vars(matches(post_game_metrics_regex)),
    funs(cummean_gen = lag(cummean(.), n=1))
  ) %>% 
  
  ## site-specific rolling and cumulative means
  group_by(season, team_abbr, game_location) %>%
  arrange(season, team_abbr, game_location, game_date) %>%
  
  ## site-specific number of games played (pre-game)  
  mutate(
    
    ## number of games played (site-specific)
    n_site = 0:(n()-1),

    ## number of wins (site-specific)
    w_site = lag(cumsum(game_outcome=='W'), n=1, default=0),
    
    ## number of losses (site-specific)
    l_site = lag(cumsum(game_outcome=='L'), n=1, default=0),
    
    ## win percentage (site-specific)
    wp_site = round(w_site / n_site, 3),
    wp_site = ifelse(is.nan(wp_site), NA, wp_site)
      
  ) %>%
  
  ## site-specific 5-game rolling average
  mutate_at(
    vars(matches(post_game_metrics_regex)),
    funs(rollmean5_site = lag(rollmean(., k=5, fill=NA, align='right'), n=1))
  ) %>%  
  
  ## site-specific 10-game rolling average
  mutate_at(
    vars(matches(post_game_metrics_regex)),
    funs(rollmean10_site = lag(rollmean(., k=10, fill=NA, align='right'), n=1))
  ) %>% 
  
  ## site-specific cumulative average  
  mutate_at(
    vars(matches(post_game_metrics_regex)),
    funs(cummean_site = lag(cummean(.), n=1))
  ) %>% 
  
  ## drop post-game performance columns
  select(
    -one_of(c('matchup', 'min')),
    -matches(post_game_metrics_regex)
  ) %>% 
  
  ## ungroup
  ungroup()


## make partial copy of pregame performance to perform join
pregame_perf_partial_copy_df <- pregame_perf_df %>%
  
  ## extract pre-game performance and leave out the post-game performance metrics
  select(game_id, game_date, season, team_abbr, opponent_abbr, 
         n_gen, w_gen, l_gen, wp_gen, n_site, w_site, l_site, wp_site, 
         matches('rollmean|cummean')) %>%

  ## rename rolling and cumulative mean performances
  rename_at(
    vars(matches('rollmean|cummean')), funs(sprintf('o_%s', .))
  ) %>%
  rename_at(
    vars(c('n_gen', 'w_gen', 'l_gen', 'wp_gen', 'n_site', 'w_site', 'l_site', 'wp_site')),
    funs(sprintf('o_%s', .))
  ) %>%
  rename(
    team_abbr=opponent_abbr,
    opponent_abbr=team_abbr    
  )


## create pre-game performance master dataset (that includes opponent's pre-game performance metrics)
pregame_perf_master_df <- 
  
  ## add raw opponent performance
  left_join(pregame_perf_df, 
            pregame_perf_partial_copy_df, 
            by=c('game_id', 'game_date', 'season', 'team_abbr', 'opponent_abbr')) 


## remove partial copy 
rm(pregame_perf_partial_copy_df)


## dimension of new df
dim(pregame_perf_master_df)
names(pregame_perf_master_df)


## remove existing
# dbExecute(conn=con, statement="DROP TABLE pregame_perf_master_df")


## write to database
dbWriteTable(conn=con, name='pregame_perf_master_df', value=pregame_perf_master_df)


## list tables in database
dbListTables(conn=con)


## disconnect from datatabase
dbDisconnect(conn=con)
