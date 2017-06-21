#### note: this script is INCOMPLETE

#### note: this script is to be executed every night at midnight



#### import libraries
library(RCurl)
library(rjson)
library(dplyr)
library(DBI)
library(RSQLite)
library(rvest)



#### updating games data
date <- '2015-12-11'
tableName <- 'games'

## obtain and process new games data for the current season
gmsRawDf <- getRawGamesDataViaApi(date=date)
gmsRawDf$date <- as.character(gmsRawDf$date)



## drop games data for the current season from the database
dbCon <- dbConnect(SQLite(), dbname='./data/db.sqlite')
query <- sprintf('DELETE FROM %s WHERE season=%s', tableName, season)
dbSendQuery(conn=dbCon, statement=query)
dbDisconnect(dbCon)

## populate database with new data
gmsRawDf$date <- as.character(gmsRawDf$date)
my_db <- src_sqlite("./data/db.sqlite", create=FALSE)  # create src (establish connection)
db_insert_into(con=my_db$con, table=tableName, values=gmsRawDf) # insert into table
dbDisconnect(my_db$con)




#### updating odds data

