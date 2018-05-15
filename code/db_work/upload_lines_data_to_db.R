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
# rm(db_pass) 

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


## check data quality
sort(table(moneylines$team))
sort(table(spreads$team))
sort(table(totals$team))


## fill in missing team info for one game on 2010-12-22
moneylines$team[moneylines$date=='2010-12-22' & moneylines$team=='' & moneylines$o_team=='Wizards'] <- 'Bulls'
moneylines$o_team[moneylines$date=='2010-12-22' & moneylines$team=='Wizards' & moneylines$o_team==''] <- 'Bulls'

spreads$team[spreads$date=='2010-12-22' & spreads$team=='' & spreads$o_team=='Wizards'] <- 'Bulls'
spreads$o_team[spreads$date=='2010-12-22' & spreads$team=='Wizards' & spreads$o_team==''] <- 'Bulls'

totals$team[totals$date=='2010-12-22' & totals$team=='' & totals$o_team=='Wizards'] <- 'Bulls'
totals$o_team[totals$date=='2010-12-22' & totals$team=='Wizards' & totals$o_team==''] <- 'Bulls'


## remove bad data (rows for football teams)
rm_teams <- c('Baltimore', 'Heat (FL)', 'Louisville', 'Montepaschi Siena', 'Pittsburgh', 'Real Madrid')
moneylines <- moneylines %>% filter(!(team %in% rm_teams) & !(o_team %in% rm_teams))
spreads <- spreads %>% filter(!(team %in% rm_teams) & !(o_team %in% rm_teams))
totals <- totals %>% filter(!(team %in% rm_teams) & !(o_team %in% rm_teams))


## read team abbreviation df
team_abbr_df <- read.csv('../../data/team_city_conference.csv', stringsAsFactors=FALSE)
team_abbr_df

## replace team names with their abbreviated equivalents
moneylines$team <- team_abbr_df$team_abbr[match(moneylines$team, team_abbr_df$team)]
moneylines$o_team <- team_abbr_df$team_abbr[match(moneylines$o_team, team_abbr_df$team)]

spreads$team <- team_abbr_df$team_abbr[match(spreads$team, team_abbr_df$team)]
spreads$o_team <- team_abbr_df$team_abbr[match(spreads$o_team, team_abbr_df$team)]

totals$team <- team_abbr_df$team_abbr[match(totals$team, team_abbr_df$team)]
totals$o_team <- team_abbr_df$team_abbr[match(totals$o_team, team_abbr_df$team)]


## team name replacement quality check
table(moneylines$team)
table(moneylines$o_team)

table(spreads$team)
table(spreads$o_team)

table(totals$team)
table(totals$o_team)


## rename column names
moneylines <- moneylines %>%
  rename(
    team_abbr = team, 
    opponent_abbr = o_team,
    game_date = date
  )
spreads <- spreads %>%
  rename(
    team_abbr = team, 
    opponent_abbr = o_team, 
    game_date = date
  )
totals <- totals %>%
  rename(
    team_abbr = team, 
    opponent_abbr = o_team, 
    game_date = date
  )


## write to flat files
write.csv(moneylines, '../../data/raw/raw_joined/moneylines.csv', row.names=FALSE)
write.csv(spreads, '../../data/raw/raw_joined/spreads.csv', row.names=FALSE)
write.csv(totals, '../../data/raw/raw_joined/totals.csv', row.names=FALSE)


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

