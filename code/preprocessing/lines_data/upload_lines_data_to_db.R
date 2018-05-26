## set database credentials
source('../../../credentials/aws_db_credentials.R')

## load libraries
library(RPostgreSQL)
library(tidyverse)
library(glue)

## load custom functions
source('../../helper/combine_dataset_files.R')


## combine raw daily odds datasets
combine_dataset_files(dir_path = '../../../data/raw/moneylines/', 
                      export_file_path = '../../../data/raw_joined/moneylines.csv')
combine_dataset_files(dir_path = '../../../data/raw/spreads/',
                      export_file_path = '../../../data/raw_joined/spreads.csv')
combine_dataset_files(dir_path = '../../../data/raw/totals/',
                      export_file_path = '../../../data/raw_joined/totals.csv')


## read lines data from flat files
moneylines <- read.csv('../../../data/raw_joined/moneylines.csv', stringsAsFactors=FALSE)
spreads <- read.csv('../../../data/raw_joined/spreads.csv', stringsAsFactors=FALSE)
totals <- read.csv('../../../data/raw_joined/totals.csv', stringsAsFactors=FALSE)


## remove unnecessary columns
# moneylines$id <- moneylines$gid <- NULL
# spreads$id <- spreads$gid <- NULL
# totals$id <- totals$gid <- NULL


## import team name-to-ID lookup table
team_name_to_id_lookup_df <- read.csv('team_name_to_id_lookup.csv', stringsAsFactors=FALSE)


## define variable of dfs to preprocess
df_names <- c('moneylines', 'spreads', 'totals')

## define non-NBA team names to remove
rm_teams <- c('Baltimore', 'Heat (FL)', 'Louisville', 'Montepaschi Siena', 'Pittsburgh', 'Real Madrid')

## for each df
for (df_name in df_names) {

  df <- 
    
    ## get df by its variable name
    get(df_name) %>%
    
    ## remove bad data (rows for football teams)
    filter(
      !(team %in% rm_teams) & !(o_team %in% rm_teams)    
    ) %>%
    
    mutate(
      
      ## modify season column (YYYY-YY format)    
      season = glue("{season}-{substr(season + 1, 3, 4)}"),
      
      ## fill in missing team and opponent info for one game on 2010-12-22    
      team = case_when(
        date=='2010-12-22' & team=='' & o_team=='Wizards' ~ 'Bulls',
        TRUE ~ team
      ),
      o_team = case_when(
        date=='2010-12-22' & team=='Wizards' & o_team=='' ~ 'Bulls',
        TRUE ~ o_team
      )
      
    ) %>%
    
    ## add team id
    left_join(y=team_name_to_id_lookup_df, by=c('team'='team_name')) %>%
    
    ## add opponent team id
    left_join(y=team_name_to_id_lookup_df, by=c('o_team'='team_name')) %>%
    
    ## rename columns
    rename(
      game_date = date,
      team_id = team_id.x, 
      opponent_id = team_id.y
    ) %>%
    
    ## select columns
    select(season, game_date, team_id, opponent_id, starts_with("X")) %>%
    as.data.frame()  
  
  ## export to environment
  assign(x=df_name, value=df)
}





## data quality assurance
head(moneylines)
head(spreads)
head(totals)


## write to flat files
# write.csv(moneylines, '../../data/raw_joined/moneylines.csv', row.names=FALSE)
# write.csv(spreads, '../../data/raw_joined/spreads.csv', row.names=FALSE)
# write.csv(totals, '../../data/raw_joined/totals.csv', row.names=FALSE)



## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
# rm(db_pass) 

## list available tables
dbListTables(con)


## drop tables in database
dbExecute(con, "DROP TABLE moneylines")
dbExecute(con, "DROP TABLE spreads")
dbExecute(con, "DROP TABLE totals")


## write to database
dbWriteTable(con, 'moneylines', moneylines)
dbWriteTable(con, 'spreads', spreads)
dbWriteTable(con, 'totals', totals)


## read from database
# moneylines <- dbReadTable(con, 'moneylines')
# spreads <- dbReadTable(con, 'spreads')
# totals <- dbReadTable(con, 'totals')


## disconnect from database
dbDisconnect(con)

