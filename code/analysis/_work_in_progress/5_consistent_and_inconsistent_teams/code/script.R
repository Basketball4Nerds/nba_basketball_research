
## load libraries
library(nbastatR)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(psych)
library(BBmisc)
library(data.table)  # for setDT()
library(knitr)
library(kableExtra)
library(glue)


## load data
rs_team_gamelogs_2017 <- get_game_logs(seasons=2018, 
                               result_type='team',
                               season_types='Regular Season')
rs_player_gamelogs_2017 <- get_game_logs(seasons=2018, 
                                 result_type='player',
                                 season_types='Regular Season')


## aggregate team points mean and sd
team_agg <- rs_team_gamelogs_2017 %>% 
  
  ## normalize for per-240-minutes
  mutate(
    ptsTeam = ptsTeam / minutesTeam * 240
  ) %>%
  
  ## group by team
  group_by(slugTeam) %>%
  
  ## get mean and sd
  summarize(
    pts_mean = round(mean(ptsTeam), 2),
    pts_sd = round(sd(ptsTeam), 2)
  ) %>%
    
  ## as df
  as.data.frame() %>%

  ## sort by column
  arrange(pts_sd)


## view aggregation
View(team_agg)
tail(team_agg)

## export to html
kable(team_agg, 'html')


## pts variation by game outcome
team_agg2 <- rs_team_gamelogs_2017 %>% 

  ## normalize for per-240-minutes
  mutate(
    ptsTeam = ptsTeam / minutesTeam * 240
  ) %>%
  
  ## group by team and outcome
  group_by(slugTeam, outcomeGame) %>%
  
  ## get mean and sd
  summarize(
    pts_mean = round(mean(ptsTeam), 2),
    pts_sd = round(sd(ptsTeam), 2)
  ) %>%

  ## as data table (needed for the next step)
  setDT() %>%
  
  ##   
  dcast(slugTeam ~ outcomeGame, value.var=c('pts_mean', 'pts_sd')) %>%
  
  ## create difference columns
  mutate(
    pts_mean_diff = pts_mean_W - pts_mean_L,
    pts_sd_abs_diff = abs(pts_sd_W - pts_sd_L)
  ) %>%

  ## sort by column
  arrange(pts_mean_diff)


## view aggregation
View(team_agg2)


## export to html
kable(team_agg2, 'html')


## pts variation by game location
team_agg3 <- rs_team_gamelogs_2017 %>% 
  
  ## normalize for per-240-minutes
  mutate(
    ptsTeam = ptsTeam / minutesTeam * 240
  ) %>%
  
  ## group by team and site
  group_by(slugTeam, locationGame) %>%
  
  ## get mean and sd
  summarize(
    pts_mean = round(mean(ptsTeam), 2),
    pts_sd = round(sd(ptsTeam), 2)
  ) %>%
  
  ## as data table (needed for the next step)
  setDT() %>%
  
  ##   
  dcast(slugTeam ~ locationGame, value.var=c('pts_mean', 'pts_sd')) %>%
  
  ## create difference columns
  mutate(
    pts_mean_diff = pts_mean_H - pts_mean_A,
    pts_sd_abs_diff = abs(pts_sd_H - pts_sd_A)
  ) %>%
  
  ## sort by column
  arrange(pts_mean_diff)


## view aggregation
team_agg3



## pts variation by game outcome and location
team_agg4 <- rs_team_gamelogs_2017 %>% 
  
  ## normalize for per-240-minutes
  mutate(
    ptsTeam = ptsTeam / minutesTeam * 240
  ) %>%
  
  ## group by team and site and outcome
  group_by(slugTeam, locationGame, outcomeGame) %>%
  
  ## get mean and sd
  summarize(
    pts_mean = round(mean(ptsTeam), 2),
    pts_sd = round(sd(ptsTeam), 2)
  ) %>%
  
  ## as data table (needed for the next step)
  setDT() %>%
  
  ##   
  dcast(slugTeam ~ locationGame + outcomeGame, value.var=c('pts_mean', 'pts_sd')) %>%
  

## view aggregation
team_agg4





#### plot
pts_range <- range(rs_team_gamelogs_2017$ptsTeam)


SAC2017 <- rs_team_gamelogs_2017 %>%
  filter(slugTeam=='SAC') %>%
  mutate(var = glue("{outcomeGame} @ {locationGame}"))
GSW2017 <- rs_team_gamelogs_2017 %>%
  filter(slugTeam=='GSW') %>%
  mutate(var = glue("{outcomeGame} @ {locationGame}"))

ggplot(GSW2017, aes(x=dateGame, y=ptsTeam)) + 
  geom_point(aes(color=var)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_y_continuous(limits=pts_range) + 
  ylab('48-minutes adjusted points') + 
  ggtitle("GSW 2017-18 Regular Season 48-Minutes-Adjusted Points")

ggplot(SAC2017, aes(x=dateGame, y=ptsTeam)) + 
  geom_point(aes(color=var)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  scale_y_continuous(limits=pts_range) + 
  ylab('48-minutes adjusted points') + 
  ggtitle("SAC 2017-18 Regular Season 48-Minutes-Adjusted Points")
