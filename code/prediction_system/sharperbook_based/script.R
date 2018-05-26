
## clear environment
rm(list = ls())


## set database credentials
source('../../../credentials/aws_db_credentials.R')


## load libraries
library(RPostgreSQL)
library(tidyverse)
library(lubridate)
library(zoo)
library(glue)
library(rpart)
library(rpart.plot)
library(randomForest)


## import custom functions
source('../../helper/calc_pred_acc.R')


## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
#rm(db_pass) 

## list available tables
dbListTables(con)

## read data
spreads <- dbReadTable(conn=con, name='spreads')
team_gamelogs_master_df <- dbReadTable(conn=con, name='team_gamelogs_master_df')
#team_gamelogs_df <- dbReadTable(conn=con, name='team_gamelogs')


team_gamelogs_master_df <- team_gamelogs_master_df %>%
  filter(season >= '1979-80')

x <- team_gamelogs_master_df %>%
  filter(team_abbr=='PHX')
tail(x)
head(x)



head(p_spreads)

table(team_gamelogs_master_df$team_abbr)



p_spreads <- spreads %>%
  select(season, game_date, team_abbr, opponent_abbr, pinnacle_line, pinnacle_payout) %>%
  rename(
    line = pinnacle_line, 
    payout = pinnacle_payout
  ) %>%
  mutate(game_date = as.Date(game_date))

names(team_gamelogs_master_df)
calc_moneyline_payout_profit(moneyline_odds = p_spreads$payout[1])

head(p_spreads)

names(p_spreads)
str(team_gamelogs_master_df)
str(p_spreads)
class(team_gamelogs_master_df$game_date)

x <- left_join(p_spreads, team_gamelogs_master_df, 
               by = c('game_date', 'team_abbr', 'opponent_abbr', 'season')) %>%
  select(season, game_date, team_abbr, opponent_abbr, line, payout, ptsmrgn)
head(x)

'BOS' %in% sort(unique(team_gamelogs_master_df$team_abbr))

sum(is.na(x$ptsmrgn))
head(p_spreads)
team_gamelogs_df %>%
  filter(game_date=='NOV 01, 2006') 

head(x, 50)
x %>% 
  filter(is.na(ptsmrgn))

head(p_spreads)
dim(p_spreads)
