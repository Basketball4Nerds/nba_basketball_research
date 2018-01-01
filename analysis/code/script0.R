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
library(klaR)  # for naive bayes
library(e1071)  # for svm, naive bayes
library(kernlab)  # for svm
library(class)  # for knn
library(randomForest)
library(gbm)  # gbm modeling
library(nnet)  # for neural network
#library(neuralnet)  # for neural network
library(glmnet)  # for lasso and ridge regression

## load functions and classes
## * future plan: these functions will be wrapped in a package to avoid overcrowding the global env
source('./code/_analysis_scripts/analysis_funcs.R')
source('./code/_analysis_scripts/functions.R')
source('./code/_analysis_scripts/pred_funcs.R')
source('./code/_analysis_scripts/pred_funcs2.R')
source('./code/_analysis_scripts/supervised_summarizer_funcs.R')
source('./code/_analysis_scripts/feature_selection_funcs.R')

source('./code/_preprocess_scripts/mov_cnt_sum_avg_funcs.R')
source('./code/_preprocess_scripts/preprocess_funcs.R')
source('./code/_preprocess_scripts/rnk_grp_funcs.R')

source('./code/_other_scripts/classes.R')
source('./code/_other_scripts/helper_funcs.R')


## load team-city mappers
TeamCityConfDf <- read.csv('./data/teams_cities_conferences.csv', stringsAsFactors=TRUE)
TEAMS <- as.character(TeamCityConfDf$Team)
CITY_ABBR <- as.character(TeamCityConfDf$CityAbbr)



# ## load DB login keys
# source('../credentials/keys.R')
# 
# 
# ## connect to db again
# mydb <- dbConnect(MySQL(), 
#                   user=DB_USER, 
#                   password=DB_PASS, 
#                   dbname=DB_NAME,
#                   host=DB_HOST)
# 
# 
# ## load complete data from db
# games <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM games;'), n=-1))
# spreads <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM spreads;'), n=-1))
# totals <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM totals;'), n=-1))
# moneylines <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM moneylines;'), n=-1))
#
#
# ## close database connections
# close_all_db_cons()


## getting data for analysis;
## * future plan: once DB construction is complete, data will be pulled from DB, not local CSV files
games <- read.csv('./data/raw/games.csv', stringsAsFactors=FALSE)
spreads <- read.csv('./data/raw/spreads.csv', stringsAsFactors=FALSE)
totals <- read.csv('./data/raw/totals.csv', stringsAsFactors=FALSE)
moneylines <- read.csv('./data/raw/moneylines.csv', stringsAsFactors=FALSE)


## there shouldn't, but in case there are duplicate rows
games <- unique(games)
spreads <- unique(spreads)
totals <- unique(totals)
moneylines <- unique(moneylines)


## data quality ensurance
ddply(games, 'season', nrow)
ddply(spreads, 'season', nrow)
ddply(totals, 'season', nrow)
ddply(moneylines, 'season', nrow)


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
