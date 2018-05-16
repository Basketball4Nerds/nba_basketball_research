## set database credentials
source('../../credentials/aws_db_credentials.R')


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
source('../helper/calc_pred_acc.R')


## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
#rm(db_pass) 

## list available tables
dbListTables(con)

## get pregame performance data
spreads <- dbReadTable(conn=con, name='spreads')
gamelogs <- dbReadTable(conn=con, name='team_gamelogs')

p_spreads <- spreads %>%
  select(season, game_date, team_abbr, opponent_abbr, pinnacle_line, pinnacle_payout) %>%
  rename(
    line = pinnacle_line, 
    payout = pinnacle_payout
  )

names(gamelogs)
calc_moneyline_payout_profit(moneyline_odds = p_spreads$payout[1])

head(p_spreads)
