## set up working directory
rm(list = ls())


## load libraries
library(shiny)
library(ggplot2)
library(corrplot)
library(TTR)
library(plyr)
library(BBmisc)
library(rpart.plot)
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
library(mRMRe)  # for redundant feature reduction
library(fmsb)  # for VIF calculation
library(rpart)  # for decision tree
library(caret)  
library(e1071)  # for svm
library(kernlab)  # for svm
library(class)  # for knn
library(randomForest)
library(gbm)  # gbm modeling
library(nnet)  # for neural network
library(neuralnet)  # for neural network


## load functions and classes
source('./code/r/analysis_funcs.R')
source('./code/r/classes.R')
source('./code/r/combine_dataset_files.R')
source('./code/r/data_grab_funcs.R')
source('./code/r/functions.R')
source('./code/r/helper_funcs.R')
source('./code/r/mov_cnt_sum_avg_funcs.R')
source('./code/r/pred_funcs.R')
source('./code/r/preprocess_funcs.R')
source('./code/r/rnk_grp_funcs.R')
source('./code/r/supervised_summarizer.R')
source('./code/r/rm_high_vif_cols.R')

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
games$lead_changes <- NULL  # error in 2015-2016 data from source
spreads_parsed <- create_parsed_odds_df(spreads, type='spreads')
totals_parsed <- create_parsed_odds_df(totals, type='totals')
moneylines_parsed <- create_parsed_odds_df(moneylines, type='moneylines')


## add game IDs (gid) to all dfs
games <- add_gid(games)
spreads_parsed <- add_gid(spreads_parsed)
totals_parsed <- add_gid(totals_parsed)
moneylines_parsed <- add_gid(moneylines_parsed)


## connect to MySQL db
mydb <- dbConnect(MySQL(), 
                  user=DB_USER, 
                  password=DB_PASS, 
                  dbname=DB_NAME,
                  host=DB_HOST)


## get list of tables in db
dbListTables(mydb)


## store them into db
dbWriteTable(mydb, value=games, name="games", append=TRUE, row.names=FALSE)
dbWriteTable(mydb, value=spreads_parsed, name="spreads", append=TRUE, row.names=FALSE)
dbWriteTable(mydb, value=totals_parsed, name="totals", append=TRUE, row.names=FALSE)
dbWriteTable(mydb, value=moneylines_parsed, name="moneylines", append=TRUE, row.names=FALSE)


## move raw dataset files into different directories (to mark successful data upload to db)
move_files_to_another_dir(from_dir='./data/raw_in_queue/games', to_dir='./data/raw_stored_in_db/games')
move_files_to_another_dir(from_dir='./data/raw_in_queue/spreads', to_dir='./data/raw_stored_in_db/spreads')
move_files_to_another_dir(from_dir='./data/raw_in_queue/totals', to_dir='./data/raw_stored_in_db/totals')
move_files_to_another_dir(from_dir='./data/raw_in_queue/moneylines', to_dir='./data/raw_stored_in_db/moneylines')


## remove old df variables from environment 
remove('games', 'spreads', 'totals', 'moneylines',
       'spreads_parsed', 'totals_parsed', 'moneylines_parsed')

## load complete data from db
games <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM games;'), n=-1))
spreads <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM spreads;'), n=-1))
totals <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM totals;'), n=-1))
moneylines <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM moneylines;'), n=-1))


## close connection to db
all_cons <- dbListConnections(MySQL())
for(con in all_cons) dbDisconnect(con)


## proper data types for date and season
games$date <- as.Date(games$date)
spreads$date <- as.Date(spreads$date)
totals$date <- as.Date(totals$date)
moneylines$date <- as.Date(moneylines$date)

games$season <- as.integer(games$season)
spreads$season <- as.integer(spreads$season)
totals$season <- as.integer(totals$season)
moneylines$season <- as.integer(moneylines$season)


## see range of dates by dataset
base::range(games$date)
base::range(spreads$date)
base::range(totals$date)
base::range(moneylines$date)


## remove id columns
games$id <- spreads$id <- totals$id <- moneylines$id <- NULL


