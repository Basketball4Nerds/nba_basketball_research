## set up working directory
rm(list = ls())

## load libraries
library(shiny)
library(ggplot2)
library(corrplot)
library(TTR)
library(plyr)
library(BBmisc)
library(rpart)
library(rpart.plot)
library(randomForest)
library(data.table)
library(DBI)
library(RMySQL)
library(RCurl)
library(rjson)
library(dplyr)
library(rvest)
library(car)  # for leveneTest()
library(lsr)  # for cohensD()
library(gridExtra)  # for expand.grid()
library(reshape2)  # for dcast()

## load functions and classes
source('./code/r/data_grab_funcs.R')
source('./code/r/combine_dataset_files.R')
source('./code/r/helper_funcs.R')
source('./code/r/preprocess_funcs.R')
source('./code/r/rnk_grp_funcs.R')
source('./code/r/mov_cnt_sum_avg_funcs.R')
source('./code/r/functions.R')
source('./code/r/classes.R')

## load keys & mappers
source('./credentials/keys.R')
TeamCityConfDf <- read.csv('./data/teams_cities_conferences.csv', stringsAsFactors=TRUE)
TEAMS <- as.character(TeamCityConfDf$Team)
CITY_ABBR <- as.character(TeamCityConfDf$CityAbbr)

## collect datasets to process and store into db
games <- concatenate_dataset_files(dir_path='./data/raw_in_queue/games')
spreads <- concatenate_dataset_files(dir_path='./data/raw_in_queue/spreads')
totals <- concatenate_dataset_files(dir_path='./data/raw_in_queue/totals')
moneylines <- concatenate_dataset_files(dir_path='./data/raw_in_queue/moneylines')

## parse and process raw odds dfs
spreads_parsed <- create


## connect to MySQL db
mydb = dbConnect(MySQL(), 
                 user=DB_USER, 
                 password=DB_PASS, 
                 dbname=DB_NAME,
                 host='localhost')
dbListTables(mydb)

## store them into db



move_files_to_another_dir(from_dir='./test1', to_dir='./test2')





## load latest data

## load data already collected
games2 <- read.csv('./data/games.csv', stringsAsFactors=FALSE)

## proper data types for date
games$date <- as.Date(games$date)

## see range of dates by dataset
base::range(games$date)

## get latest season data
last_season <- 2016
games_apnd <- get_raw_games_data_via_api(season=last_season)
games <- subset(games, season != last_season)
games <- rbind.data.frame(games, games_apnd)
remove(games_apnd)



#### download latest odds data
download_latest_raw_odds_data('spreads')
download_latest_raw_odds_data('totals')
download_latest_raw_odds_data('moneylines')



#### save data feed dfs into csv
write.csv(games, './data/games.csv', row.names=FALSE)





## add latest data to the dfs
games <- add_latest_data(games, 
                         func='get_raw_games_data_via_api',
                         non_date_args=list())

# moneylines <- read.csv('./data/raw/moneylines.csv', stringsAsFactors=FALSE)
spreads <- read.csv('./data/raw/spreads.csv', stringsAsFactors=FALSE)
totals <- read.csv('./data/raw/totals.csv', stringsAsFactors=FALSE)

# moneylines$date <- as.Date(moneylines$date)
# spreads$date <- as.Date(spreads$date)
# totals$date <- as.Date(totals$date)

# write.csv(spreads, './data/spreads.csv', row.names=FALSE)
# write.csv(totals, './data/totals.csv', row.names=FALSE)
