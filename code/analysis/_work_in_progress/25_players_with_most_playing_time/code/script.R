## set database credentials
source('credentials.R')

## load libraries
library(RPostgreSQL)
library(tidyverse)

## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
rm(db_pass) 

## list available tables
dbListTables(con)

## check for tables
dbExistsTable(con, 'player_gamelogs')
dbExistsTable(con, 'player_header')

## read table into data frame
player_game_logs_df <- dbReadTable(con, 'player_gamelogs')
player_header_df <- dbReadTable(con, 'player_header')

## disconnect from database
dbDisconnect(con)

x <- player_game_logs_df %>%
  group_by(season_id, player_id) %>%
  summarize(
    sum_minutes = sum(min),
    mean_minutes = mean(min),
    n_games = n()
  ) %>%
  arrange(season_id, -mean_minutes) %>%
  top_n(wt=sum_minutes, n=10) %>%
  left_join(player_header_df, by='player_id') %>%
  select(season_id, player_id, display_name, sum_minutes, mean_minutes, n_games)


## find most over-played players
sort(table(x$display_name))


## save the top two
lebron_james <- x %>% filter(display_name=='LeBron James')
kevin_durant <- x %>% filter(display_name=='Kevin Durant')

## plot
ggplot() + 
  geom_point(data=x,
             aes(x=season_id, y=sum_minutes)) + 
  geom_point(data=lebron_james, 
             aes(x=season_id, y=sum_minutes, color='red')) + 
  geom_point(data=kevin_durant, 
             aes(x=season_id, y=sum_minutes, color='blue')) +   
  scale_colour_manual(name = 'Highlighted Players', 
                      values = c('red'='red', 'blue'='blue'),
                      breaks = c('red', 'blue'),
                      labels = c('LeBron James', 'Kevin Durant')) + 
  theme_light() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  xlab('season') + 
  ylab("player's total minutes") + 
  ggtitle("Top 10 Player Minutes per Season")

head(player_game_logs_df)
  


