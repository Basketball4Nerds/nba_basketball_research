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

## collect datasets to parse and store into db
games <- concatenate_dataset_files(dir_path='./data/raw_in_queue/games')
spreads <- concatenate_dataset_files(dir_path='./data/raw_in_queue/spreads')
totals <- concatenate_dataset_files(dir_path='./data/raw_in_queue/totals')
moneylines <- concatenate_dataset_files(dir_path='./data/raw_in_queue/moneylines')

## parse and process raw odds dfs
spreads_parsed <- create_parsed_odds_df(spreads, type='spreads')
totals_parsed <- create_parsed_odds_df(totals, type='totals')
moneylines_parsed <- create_parsed_odds_df(moneylines, type='moneylines')

## connect to MySQL db
mydb <- dbConnect(MySQL(), 
                  user=DB_USER, 
                  password=DB_PASS, 
                  dbname=DB_NAME,
                  host=DB_HOST)
dbListTables(mydb)

#rs <- dbSendQuery(mydb, 'SELECT * FROM games LIMIT 5;')
rs <- dbSendQuery(mydb, 'DELETE FROM games LIMIT 5;')
#data <- fetch(rs, n=-1)

## store them into db
dbWriteTable(mydb, value=games, name="games", append=TRUE, row.names=FALSE)
# dbWriteTable(mydb, value=spreads_parsed, name="spreads", append=TRUE, row.names=FALSE)
# dbWriteTable(mydb, value=totals_parsed, name="totals", append=TRUE, row.names=FALSE)
# dbWriteTable(mydb, value=moneylines_parsed, name="moneylines", append=TRUE, row.names=FALSE)

## close connection to db
all_cons <- dbListConnections(MySQL())
for(con in all_cons) dbDisconnect(con)


## move raw dataset files into different directories (to mark successful data upload to db)
move_files_to_another_dir(from_dir='./data/raw_in_queue/games', to_dir='./data/raw_stored_in_db/games')
move_files_to_another_dir(from_dir='./data/raw_in_queue/spreads', to_dir='./data/raw_stored_in_db/spreads')
move_files_to_another_dir(from_dir='./data/raw_in_queue/totals', to_dir='./data/raw_stored_in_db/totals')
move_files_to_another_dir(from_dir='./data/raw_in_queue/moneylines', to_dir='./data/raw_stored_in_db/moneylines')

## load complete data from db
games2 <- read.csv('./data/games.csv', stringsAsFactors=FALSE)

## proper data types for date
games$date <- as.Date(games$date)
spreads$date <- as.Date(spreads$date)
totals$date <- as.Date(totals$date)
moneylines$date <- as.Date(moneylines$date)

## see range of dates by dataset
base::range(games$date)
base::range(spreads$date)
base::range(totals$date)
base::range(moneylines$date)









