## clear environment
rm(list=ls())


## load libraries
library(knitr)
library(dplyr)
library(zoo)
library(BBmisc)
library(Metrics)
library(ggplot2)
source('../../../helper/general/fill_in_opp_cols.R')


## load regular season games
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')
games$team_type <- ifelse(games$line < 0, 'favorite', 
                          ifelse(games$line > 0, 'underdog', NA))


## select relevant cols
base_cols <- c('season', 'date', 'site', 'playoffs', 'team', 'o_team', 'cnf', 'o_cnf', 'outcome', 'rqP', 'rqPA')
games <- games[ , base_cols]

## number of observations by season
tapply(games$outcome, games$season, length)

## subset only regular season data
rs_games <- subset(games, playoffs==0)

## remove 1998-1999 games (lockout season with not enough data for rolling averages)
rs_games <- subset(games, season != 1998)


## add rolling-mean and cummulative-mean metrics
rs_games <- rs_games %>%
  
  ## general rolling and cumulative means
  group_by(season, team) %>%
  arrange(date) %>%
  mutate(
    n_gen = 0:(length(date)-1),
    
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


## as data frame
rs_games <- as.data.frame(rs_games)


## fill in opposite team cols
new_cols <- setdiff(colnames(rs_games), base_cols)
rs_games <- fill_in_opp_cols(rs_games, cols=new_cols)


## filter by complete cases
# table(complete.cases(rs_games))
# rs_games <- rs_games[complete.cases(rs_games), ]



