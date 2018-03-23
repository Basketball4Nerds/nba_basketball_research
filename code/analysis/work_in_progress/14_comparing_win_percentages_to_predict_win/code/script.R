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
library(nbastatR)
#detach('package:plyr', unload=TRUE)
source('../../../helper/fill_in_opp_cols.R')


## load data
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)


## light preprocessing
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')


## select columns of interest
base_cols <- c('season', 'date', 'site', 'playoffs', 'team', 'o_team', 
               'cnf', 'o_cnf', 'won', 'outcome')
games <- games[ , base_cols]

## add cumulative perf
games <- games %>% 
  
  ## add general cumulative win/loss perf
  group_by(season, team) %>%
  arrange(date) %>%
  mutate(
    w_gen = cumsum(won),
    l_gen = cumsum(!won),
    w_gen = c(0, w_gen)[-length(w_gen)],
    l_gen = c(0, l_gen)[-length(l_gen)],
    n_gen = w_gen + l_gen,
    wpc_gen = w_gen / n_gen
  ) %>%

  ## add site-specific cumulative win/loss perf  
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    w_site = cumsum(won),
    l_site = cumsum(!won),
    w_site = c(0, w_site)[-length(w_site)],
    l_site = c(0, l_site)[-length(l_site)],
    n_site = w_site + l_site,
    wpc_site = w_site / n_site
  ) %>%
  
  ## add oppoent-conference-specific cumulative win/loss perf
  group_by(season, team, o_cnf) %>%
  arrange(date) %>%
  mutate(
    w_cnf = cumsum(won),
    l_cnf = cumsum(!won),
    w_cnf = c(0, w_cnf)[-length(w_cnf)],
    l_cnf = c(0, l_cnf)[-length(l_cnf)],
    n_cnf = w_cnf + l_cnf,
    wpc_cnf = w_cnf / n_cnf
  ) 

##
new_cols <- c('n_gen', 'wpc_gen', 'n_site', 'wpc_site', 'n_cnf', 'wpc_cnf')
games <- games[ , c(base_cols, new_cols)]

## 
games <- fill_in_opp_cols(games, cols=new_cols)

head(games)

