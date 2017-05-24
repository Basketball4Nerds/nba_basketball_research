##### set up working directory
rm(list = ls())



#### load libraries
library(shiny)
#library(ggplot2)
#library(corrplot)
library(TTR)
library(plyr)
library(BBmisc)
#library(rpart)
#library(rpart.plot)
#library(randomForest)
#library(data.table)
#library(DBI)
#library(RSQLite)
library(RCurl)
library(rjson)



#### run shiny server (everything inside is performed each time a user visits the app)
shinyServer(function(input, output) {
  
#   ## load data from database 
#   db <- dbConnect(SQLite(), dbname='./data/db.sqlite')
#   gmsRawDf <- dbReadTable(db, 'games')
#   odds <- dbReadTable(db, 'odds')
#   dbDisconnect(db)
  
  ## load functions
  source('./functions.R', local=TRUE)
  #source('./code/functions.R', local=TRUE)
  
  ## determine the current season
  season <- getSeasonFrDate(date=Sys.Date())

  ## load data via API
  gmsRawDf <- getRawGamesDataViaApi(season=season)

  
  ## load source code
  source('./reactives.R', local=TRUE)
  source('./uiCtrls.R', local=TRUE)
  source('./uiOutputs.R', local=TRUE)
})