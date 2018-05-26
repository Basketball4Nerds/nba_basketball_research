
## clear environment
remove(list = ls())

## set database credentials
source('../../../credentials/aws_db_credentials.R')

## load libraries
library(RPostgreSQL)
library(tidyverse)
library(nbastatR)
library(lubridate)
library(zoo)
library(glue)

## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
#rm(db_pass) 

## list available tables
dbListTables(con)


## read tables into dfs
team_gamelogs_df <- dbReadTable(con, 'team_gamelogs')


## save original column names
orig_cols <- colnames(team_gamelogs_df)


## create team gamelogs master
team_gamelogs_df <- team_gamelogs_df %>%
  
  ## drop after-game win/loss records
  select(-one_of(c('win', 'loss', 'w_pct'))) %>%
  
  mutate(
    
    ## replace -1 or 0 with NA
    fga = ifelse(fga==-1, NA, fga),
    fg3m = ifelse(fg3m==-1, NA, fg3m),
    fg3a = ifelse(fg3a==-1, NA, fg3a),
    oreb = ifelse(oreb==-1, NA, oreb),
    dreb = ifelse(dreb==-1, NA, dreb),
    reb = ifelse(reb==0, NA, reb),
    ast = ifelse(ast==-1, NA, ast),
    stl = ifelse(stl==-1, NA, stl),
    blk = ifelse(blk==-1, NA, blk),
    tov = ifelse(tov==-1, NA, tov),
    
    ## format date     
    game_date = as.Date(game_date, format='%b %d, %Y'),
    
    ## get season slug
    season = case_when(
      month(game_date) %in% c(10, 11, 12) ~ year(game_date),
      month(game_date) %in% c(1, 2, 3, 4, 5, 6) ~ year(game_date) - 1
    ),
    season = glue("{season}-{substr(season+1, 3, 4)}"),
    
    ## get team abbreviation from matchup (whichever team that's listed first)
    team_abbr = gsub(' @.*| vs.*', '', matchup),
    
    ## get opponent abbreviation from matchup (whichever team that's listed second)
    opponent_abbr = gsub('.*@ |.*vs\\. ', '', matchup),
    
    ## get game location info from 
    game_location = case_when(
      grepl('vs\\.', matchup) ~ 'H',
      grepl('@', matchup) ~ 'A'
    ),
    
    ## get 2-point shots made, attempted, and percentage
    fg2m = fgm - fg3m,
    fg2a = fga - fg3a,
    fg2_pct = round(fg2m / fg2a, 3)
    
  ) %>%
  
  ## sort by team and game date
  arrange(team_abbr, game_date) %>%
  
  ## rename columns
  rename(
    game_outcome = wl,
    fgp = fg_pct,    
    fg2p = fg2_pct,    
    fg3p = fg3_pct,   
    ftp = ft_pct
  ) 


## set vector of post-game metrics
post_game_metrics <- c('fgm', 'fga', 'fgp', 'fg2m', 'fg2a', 'fg2p', 'fg3m', 'fg3a', 'fg3p', 
                       'ftm', 'fta', 'ftp', 'oreb', 'dreb', 'reb', 'ast', 'stl', 'blk', 'tov', 'pf', 'pts')


## make partial copy of team gamelogs to perform join
team_gamelogs_partial_copy_df <- team_gamelogs_df %>%
  select(-one_of('matchup', 'game_outcome', 'min', 'game_location')) %>%
  rename_at(
    vars(post_game_metrics), 
    funs(sprintf('%s_alwd', .))
  ) %>%
  rename(
    team_abbr = opponent_abbr,
    opponent_abbr = team_abbr,
    opponent_id = team_id
  )


## create team gamelog master dataset (that includes opponent's performance metrics)
team_gamelogs_master_df <- 
  
  ## add raw opponent performance
  left_join(team_gamelogs_df, 
            team_gamelogs_partial_copy_df, 
            by=c('game_id', 'game_date', 'season', 'team_abbr', 'opponent_abbr')) %>%
  
  mutate(
    
    ## add point margin column
    ptsmrgn = pts - pts_alwd
  ) %>%
  
  ## select relevant columns 
  select(
    game_id, team_id, opponent_id, season, game_date, game_location, game_outcome, ptsmrgn, min,
    post_game_metrics,
    glue("{post_game_metrics}_alwd")
  )

## remove team gamelogs partial copy df
rm(team_gamelogs_partial_copy_df)


## quick review of gamelogs master df
head(team_gamelogs_master_df)
tail(team_gamelogs_master_df)


## remove table from database
dbExecute(conn=con, statement='DROP TABLE team_gamelogs_master_df')


## write to database
dbWriteTable(conn=con, name='team_gamelogs_master_df', value=team_gamelogs_master_df)


## list tables in database
dbListTables(conn=con)


## disconnect from datatabase
dbDisconnect(conn=con)
