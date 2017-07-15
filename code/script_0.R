##### set up working directory
rm(list = ls())



#### load libraries
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
library(RSQLite)
library(RCurl)
library(rjson)
library(dplyr)
library(rvest)
library(car)  # for leveneTest()
library(lsr)  # for cohensD()
library(gridExtra)  # for expand.grid()
library(reshape2)  # for dcast()



#### load functions and classes
source('./code/data_grab_funcs.R')
source('./code/helper_funcs.R')
source('./code/preprocess_funcs.R')
source('./code/rnk_grp_funcs.R')
source('./code/mov_cnt_sum_avg_funcs.R')
source('./code/functions.R')
source('./code/classes.R')


#### load keys & mappers
TeamCityConfDf <- read.csv('./data/teams_cities_conferences.csv', stringsAsFactors=TRUE)
TEAMS <- as.character(TeamCityConfDf$Team)
CITY_ABBR <- as.character(TeamCityConfDf$CityAbbr)



#### 
## check the latest data downloaded 

## download new data 


#### load data

## load data already collected
games <- read.csv('./data/games.csv', stringsAsFactors=FALSE)
# spreads <- read.csv('./data/spreads.csv', stringsAsFactors=FALSE)
# totals <- read.csv('./data/totals.csv', stringsAsFactors=FALSE)

## proper data types for date
games$date <- as.Date(games$date)
# spreads$date <- as.Date(spreads$date)
# totals$date <- as.Date(totals$date)

## see range of dates by dataset
range(games$date)
# range(spreads$date)
# range(totals$date)

## add latest data to the dfs
games <- addLatestData(games, 
                       func='get_raw_games_data_via_api',
                       non_date_args=list())
# spreads <- addLatestData(spreads,
#                          func='scrapeDataFrSportsbookReviewByDate',
#                          non_date_args=list(relTeams=TEAMS, type='point-spread'))
# totals <- addLatestData(totals,
#                         func='scrapeDataFrSportsbookReviewByDate',
#                         non_date_args=list(relTeams=TEAMS, type='total-points'))



#### save data feed dfs into csv
write.csv(games, './data/games.csv', row.names=FALSE)
# write.csv(spreads, './data/spreads.csv', row.names=FALSE)
# write.csv(totals, './data/totals.csv', row.names=FALSE)




#### create odds df (contains odds-makers' projections)
# odds <- createOddsDf(spreads=spreads, totals=totals)


