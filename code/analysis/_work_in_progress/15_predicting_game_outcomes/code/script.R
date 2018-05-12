## clear environment
remove(list = ls())


## loading libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(dplyr)
library(reshape2)
library(data.table)
library(BBmisc)
library(knitr)
library(zoo)
library(nbastatR)
#detach('package:plyr', unload=TRUE)
source('../../../helper/general/fill_in_opp_cols.R')
source('../../../helper/general/calc_acc_fr_cnf_mtx.R')


## load data
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)


## light preprocessing
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')


## select columns of interest
base_cols <- c('season', 'date', 'site', 'playoffs', 'team', 'o_team', 
               'cnf', 'o_cnf', 'won', 'outcome', 'line', 'rqP', 'rqPA')
games <- games[ , base_cols]


## subset for regular games
rs_games <- subset(games, playoffs==0)

## remove 1998-99 season (lockout season with not enough observations)
rs_games <- subset(rs_games, season != 1998)

## add cumulative perf
rs_games <- rs_games %>% 
  
  ## general rolling and cumulative averages
  group_by(season, team) %>%
  arrange(date) %>%
  mutate(
    rqP_rollmean5_gen = lag(rollmean(rqP, k=5, fill=NA, align='right'), n=1),
    rqP_rollmean10_gen = lag(rollmean(rqP, k=10, fill=NA, align='right'), n=1),
    rqP_cummean_gen = lag(cummean(rqP), n=1),
    rqPA_rollmean5_gen = lag(rollmean(rqPA, k=5, fill=NA, align='right'), n=1),
    rqPA_rollmean10_gen = lag(rollmean(rqPA, k=10, fill=NA, align='right'), n=1),
    rqPA_cummean_gen = lag(cummean(rqPA), n=1)
  ) %>%
  
  ## site-specific rolling and cumulative averages
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    rqP_rollmean5_site = lag(rollmean(rqP, k=5, fill=NA, align='right'), n=1),
    rqP_rollmean10_site = lag(rollmean(rqP, k=10, fill=NA, align='right'), n=1),
    rqP_cummean_site = lag(cummean(rqP), n=1), 
    rqPA_rollmean5_site = lag(rollmean(rqPA, k=5, fill=NA, align='right'), n=1),
    rqPA_rollmean10_site = lag(rollmean(rqPA, k=10, fill=NA, align='right'), n=1),
    rqPA_cummean_site = lag(cummean(rqPA), n=1)
  ) %>%

  ## conference-specific rolling and cumulative averages
  group_by(season, team, o_cnf) %>%
  arrange(date) %>%
  mutate(
    rqP_rollmean5_cnf = lag(rollmean(rqP, k=5, fill=NA, align='right'), n=1),
    rqP_rollmean10_cnf = lag(rollmean(rqP, k=10, fill=NA, align='right'), n=1),
    rqP_cummean_cnf = lag(cummean(rqP), n=1), 
    rqPA_rollmean5_cnf = lag(rollmean(rqPA, k=5, fill=NA, align='right'), n=1),
    rqPA_rollmean10_cnf = lag(rollmean(rqPA, k=10, fill=NA, align='right'), n=1),
    rqPA_cummean_cnf = lag(cummean(rqPA), n=1)
  ) 


## fill in opponent cols
new_cols <- setdiff(names(rs_games), base_cols)
rs_games <- fill_in_opp_cols(rs_games, cols=new_cols)


