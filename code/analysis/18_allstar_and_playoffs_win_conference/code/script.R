## clear environment
rm(list=ls())


## load libraries
library(knitr)
library(dplyr)
library(zoo)
library(BBmisc)
library(Metrics)
library(ggplot2)
library(gridExtra)
library(ggthemes) 
library(nbastatR)


## data sources
# https://www.basketball-reference.com/playoffs/
# https://www.basketball-reference.com/allstar/
# https://en.wikipedia.org/wiki/Eastern_Conference_(NBA)
# https://en.wikipedia.org/wiki/Western_Conference_(NBA)


## load data

# all star games data
allstar <- read.csv("../../../../data/allstar_team_scores.csv", na.strings=c('', ' '))

# playoff winner data
playoffs <- read.csv("../../../../data/playoffs_win_conferences.csv", na.strings=c('', ' '))


## light preprocessing

allstar <- 
  allstar %>%
  mutate(
    
    # add winning conference as column    
    AllStarWinConf =
      case_when(
        EastConfScore > WestConfScore ~ 'East',
        EastConfScore < WestConfScore ~ 'West'
      ),
    
    # add absolute value of win pts margin as column    
    PtsMargin = abs(EastConfScore - WestConfScore),

    # add season column    
    Season = paste0(Year - 1, '-', substr(Year, 3, 4)),
    
    # remove year column
    Year = NULL
  )

playoffs <-
  playoffs %>%
  mutate(
    
    # add season column
    Season = paste0(Year - 1, '-', substr(Year, 3, 4)),
    
    # remove year column
    Year = NULL
  )




## join two dfs 
data <- 
  left_join(x=allstar, y=playoffs, by='Season') %>%
  select(Season, AllStarWinConf, PlayoffsWinConf)


## confusion matrix
cnf_mtx <- table(data$AllStarWinConf, data$PlayoffsWinConf)
cnf_mtx

## accuracy
sum(diag(cnf_mtx)) / sum(cnf_mtx)


## html table (raw)
kable(data, 'html')
kable(cnf_mtx, 'html')


##
data2 <- 
  left_join(x=allstar, y=playoffs, by='Season') %>%
  filter(abs(PtsMargin) >= 10) %>%
  select(Season, AllStarWinConf, PlayoffsWinConf)


##
cnf_mtx2 <- table(data2$AllStarWinConf, data2$PlayoffsWinConf)


## accuracy
sum(diag(cnf_mtx2)) / sum(cnf_mtx2)


## html table (raw)
kable(data, 'html')






#rs_games <- get_game_logs(seasons=1980:2017, result_types='team', season_types='Regular Season')
