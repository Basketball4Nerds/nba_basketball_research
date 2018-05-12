## set database credentials
source('../../credentials/aws_db_credentials.R')

## load libraries
library(RPostgreSQL)
library(tidyverse)
library(glue)


## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
rm(db_pass) 

## list available tables
dbListTables(con)


## read lines data from flat files
moneylines <- read.csv('../../data/raw/raw_joined/moneylines.csv', stringsAsFactors=FALSE)
spreads <- read.csv('../../data/raw/raw_joined/spreads.csv', stringsAsFactors=FALSE)
totals <- read.csv('../../data/raw/raw_joined/totals.csv', stringsAsFactors=FALSE)

## remove unnecessary columns
moneylines$id <- moneylines$gid <- NULL
spreads$id <- spreads$gid <- NULL
totals$id <- totals$gid <- NULL


## modify season column
moneylines <- moneylines %>% 
  mutate(
    season = glue("{season}-{substr(season + 1, 3, 4)}")
  )
spreads <- spreads %>%
  mutate(
    season = glue("{season}-{substr(season + 1, 3, 4)}")
  )
totals <- totals %>%
  mutate(
    season = glue("{season}-{substr(season + 1, 3, 4)}")
  )


## write to flat files
moneylines <- write.csv(moneylines, '../../data/raw/raw_joined/moneylines.csv', row.names=FALSE)
spreads <- write.csv(spreads, '../../data/raw/raw_joined/spreads.csv', row.names=FALSE)
totals <- write.csv(totals, '../../data/raw/raw_joined/totals.csv', row.names=FALSE)

## drop tables in database
dbExecute(con, "DROP TABLE moneylines")
dbExecute(con, "DROP TABLE spreads")
dbExecute(con, "DROP TABLE totals")

## write to database
dbWriteTable(con, 'moneylines', moneylines)
dbWriteTable(con, 'spreads', spreads)
dbWriteTable(con, 'totals', totals)


## disconnect from database
dbDisconnect(con)


