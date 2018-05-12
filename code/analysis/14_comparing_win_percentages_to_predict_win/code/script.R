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
source('../../../helper/general/fill_in_opp_cols.R')
source('../../../helper/general/calc_acc_fr_cnf_mtx.R')


## load data
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)


## light preprocessing
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')


## select columns of interest
base_cols <- c('season', 'date', 'site', 'playoffs', 'team', 'o_team', 
               'cnf', 'o_cnf', 'won', 'outcome', 'line')
games <- games[ , base_cols]

## add cumulative perf
games <- games %>% 
  
  ## add general cumulative win/loss perf
  group_by(season, team) %>%
  arrange(date) %>%
  mutate(
    w_gen = lag(cumsum(won), n=1, default=0),
    l_gen = lag(cumsum(!won), n=1, default=0),
    n_gen = w_gen + l_gen,
    wpc_gen = w_gen / n_gen
  ) %>%

  ## add site-specific cumulative win/loss perf  
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    w_site = lag(cumsum(won), n=1, default=0),
    l_site = lag(cumsum(!won), n=1, default=0),
    n_site = w_site + l_site,
    wpc_site = w_site / n_site
  ) %>%
  
  ## add oppoent-conference-specific cumulative win/loss perf
  group_by(season, team, o_cnf) %>%
  arrange(date) %>%
  mutate(
    w_cnf = lag(cumsum(won), n=1, default=0),
    l_cnf = lag(cumsum(!won), n=1, default=0),
    n_cnf = w_cnf + l_cnf,
    wpc_cnf = w_cnf / n_cnf
  ) 


## select relevant fields
new_cols <- c('n_gen', 'wpc_gen', 'n_site', 'wpc_site', 'n_cnf', 'wpc_cnf')
games <- games[ , c(base_cols, new_cols)]


## fill in opponent cols
games <- fill_in_opp_cols(games, cols=new_cols)


## establish baseline measure
games0 <- subset(games, n_gen >= 10 & o_n_gen >= 10)
pred_base <- ifelse(games0$line < 0, 'win', 'loss')
cmtx_base <- table(games0$outcome, pred_base)
calc_acc_fr_cnf_mtx(cmtx_base) * 100
length(pred_base)

pred_base2 <- ifelse(games0$line < 0 & games0$site=='home', 'win',
                     ifelse(games0$line > 0 & games0$site=='away', 'loss', NA))
cmtx_base2 <- table(games0$outcome, pred_base2)
calc_acc_fr_cnf_mtx(cmtx_base2) * 100
length(pred_base2)


## prediction based on general win percentage
games1 <- games[ , c(base_cols, 'n_gen', 'o_n_gen', 'wpc_gen', 'o_wpc_gen')]
games1 <- subset(games1, n_gen >= 10 & o_n_gen >= 10)

pred_wpc_gen <- games1$wpc_gen > games1$o_wpc_gen
cmtx_wpc_gen <- table(games1$outcome, pred_wpc_gen)
calc_acc_fr_cnf_mtx(cmtx_wpc_gen) * 100
length(pred_wpc_gen)

pred_wpc_gen2 <- ifelse(games1$wpc_gen > games1$o_wpc_gen & games1$site=='home', TRUE,
                               ifelse(games1$wpc_gen < games1$o_wpc_gen & games1$site=='away', FALSE, NA))
cmtx_wpc_gen2 <- table(games1$outcome, pred_wpc_gen2)
calc_acc_fr_cnf_mtx(cmtx_wpc_gen2) * 100
table(!is.na(pred_wpc_gen2))



## prediction based on site-specific win percentage
games2 <- games[ , c(base_cols, 'n_site', 'o_n_site', 'wpc_site', 'o_wpc_site')]
games2 <- subset(games2, n_site >= 10 & o_n_site >= 10)

pred_wpc_site  <- games2$wpc_site > games2$o_wpc_site
cmtx_wpc_site <- table(games2$outcome, pred_wpc_site)
calc_acc_fr_cnf_mtx(cmtx_wpc_site) * 100
length(pred_wpc_site)

pred_wpc_site2 <- ifelse(games2$wpc_site > games2$o_wpc_site & games2$site=='home', TRUE,
                        ifelse(games2$wpc_site < games2$o_wpc_site & games2$site=='away', FALSE, NA))
cmtx_wpc_site2 <- table(games2$outcome, pred_wpc_site2)
calc_acc_fr_cnf_mtx(cmtx_wpc_site2) * 100
table(!is.na(pred_wpc_site2))



## prediction based on conference-specific win percentage
games3 <- games[ , c(base_cols, 'n_cnf', 'o_n_cnf', 'wpc_cnf', 'o_wpc_cnf')]
games3 <- subset(games3, n_cnf >= 10 & o_n_cnf >= 10)

pred_wpc_cnf <- games3$wpc_cnf > games3$o_wpc_cnf
cmtx_wpc_cnf <- table(games3$outcome, pred_wpc_cnf)
calc_acc_fr_cnf_mtx(cmtx_wpc_cnf) * 100
length(pred_wpc_cnf)

pred_wpc_cnf2 <- ifelse(games3$wpc_cnf > games3$o_wpc_cnf & games3$site=='home', TRUE,
                         ifelse(games3$wpc_cnf < games3$o_wpc_cnf & games3$site=='away', FALSE, NA))
cmtx_wpc_cnf2 <- table(games3$outcome, pred_wpc_cnf2)
calc_acc_fr_cnf_mtx(cmtx_wpc_cnf2) * 100
table(!is.na(pred_wpc_cnf2))



