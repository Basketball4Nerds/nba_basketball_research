## set database credentials
source('../../credentials/aws_db_credentials.R')

## load libraries
library(RPostgreSQL)
library(tidyverse)
library(glue)
library(nbastatR)


## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
rm(db_pass) 

## list available tables
dbListTables(con)


## check for tables
dbExistsTable(con, 'team_gamelogs')


## 
t <- get_game_logs(seasons=2007, result_types='team')

drop_cols <- c('yearSeason', 'slugTeamWinner', 'slugTeamLoser', 'hasVideo', 'urlTeamSeasonLogo')

f <- t %>%
  select(-one_of(drop_cols)) %>% 
  setNames(gsub("Team|slug", "", names(.)))

View(head(f))











## read lines data from flat files
moneylines <- read.csv('../../data/raw/raw_joined/moneylines.csv', stringsAsFactors=FALSE)





## modify season column
moneylines <- moneylines %>% 
  mutate(
    season = glue("{season}-{substr(season + 1, 3, 4)}")
  )


## write to flat files
moneylines <- write.csv(moneylines, '../../data/raw/raw_joined/moneylines.csv', row.names=FALSE)
spreads <- write.csv(spreads, '../../data/raw/raw_joined/spreads.csv', row.names=FALSE)
totals <- write.csv(totals, '../../data/raw/raw_joined/totals.csv', row.names=FALSE)

## drop tables in database
dbExecute(con, "DROP TABLE moneylines")

## write to database
dbWriteTable(con, 'moneylines', moneylines)


## disconnect from database
dbDisconnect(con)


