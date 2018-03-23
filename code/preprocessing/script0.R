## set up working directory
rm(list = ls())


## load libraries
library(plyr)


## load team-city mappers
TeamCityConfDf <- read.csv('../../data/teams_cities_conferences.csv', stringsAsFactors=TRUE)
TEAMS <- as.character(TeamCityConfDf$Team)
CITY_ABBR <- as.character(TeamCityConfDf$CityAbbr)


## getting data for analysis;
games <- read.csv('../../data/raw/raw_joined/games.csv', stringsAsFactors=FALSE)
spreads <- read.csv('../../data/raw/raw_joined/spreads.csv', stringsAsFactors=FALSE)
totals <- read.csv('../../data/raw/raw_joined/totals.csv', stringsAsFactors=FALSE)
moneylines <- read.csv('../../data/raw/raw_joined/moneylines.csv', stringsAsFactors=FALSE)


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
