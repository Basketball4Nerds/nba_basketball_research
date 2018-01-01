#### note: this script is INCOMPLETE

#### note: this script is to be executed every night at midnight



source('./code/combine_dataset_files.R')
source('./code/data_grab_funcs.R')





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








##### 




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


## close connection to db
close_all_db_cons()


## move raw dataset files into different directories (to mark successful data upload to db)
move_files_to_another_dir(from_dir='./data/raw_in_queue/games', to_dir='./data/raw_stored_in_db/games')
move_files_to_another_dir(from_dir='./data/raw_in_queue/spreads', to_dir='./data/raw_stored_in_db/spreads')
move_files_to_another_dir(from_dir='./data/raw_in_queue/totals', to_dir='./data/raw_stored_in_db/totals')
move_files_to_another_dir(from_dir='./data/raw_in_queue/moneylines', to_dir='./data/raw_stored_in_db/moneylines')


## remove old df variables from environment 
remove('games', 'spreads', 'totals', 'moneylines',
       'spreads_parsed', 'totals_parsed', 'moneylines_parsed')


