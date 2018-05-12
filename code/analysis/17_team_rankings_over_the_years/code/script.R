## clear environment
rm(list = ls())


## load libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(dplyr)
library(BBmisc)
library(knitr)
library(dplyr)
library(nbastatR)


## load data
rs_games <- get_game_logs(seasons=1980:2017, result_types='team', season_types='Regular Season')


## this function takes a vector of ranks and shrinks the gap between any two ranks if there is any
remove_rank_gap <- function(rank_vals) {
  current_rank_unique_vals <- unique(sort(rank_vals))
  new_rank_unique_vals <- 1:length(current_rank_unique_vals)
  new_rank_vals <- plyr::mapvalues(rank_vals, from=current_rank_unique_vals, to=new_rank_unique_vals)
  return(new_rank_vals)
}


## aggregate data
agg <- rs_games %>% 
  group_by(slugSeason, slugTeam) %>%
  summarize(
    wpc = sum(outcomeGame=='W') / length(outcomeGame)
  ) %>%
  rename(
    season=slugSeason, 
    team=slugTeam
  ) %>%
  mutate(
    rank = remove_rank_gap(rank(-wpc, ties.method='min'))
  ) %>%
  data.frame()


##
agg_sub <- subset(agg, team %in% c('CHI', 'LAL', 'BOS', 'CLE'))


## plot win percentages over time
ggplot() + 
  geom_line(aes(x=season, y=wpc, group=team), data=agg, color='grey', alpha=0.7) + 
  geom_line(aes(x=season, y=wpc, group=team, color=team), data=agg_sub) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Regular Season Win Percentage Over Time") + 
  ylab('win percentage') + 
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)])  


## plot win percentage ranks over time
ggplot() + 
  geom_line(aes(x=season, y=rank, group=team), data=agg, color='grey', alpha=0.7) + 
  geom_line(aes(x=season, y=rank, group=team, color=team), data=agg_sub) + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_y_continuous(trans = "reverse", breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  ggtitle("Regular Season Win Percentage Ranks Over Time") + 
  ylab('win percentage rank') +
  scale_x_discrete(breaks = unique(agg$season)[c(TRUE, FALSE)])  




## get team ranks df
team_ranks <- get_teams_seasons_rankings(seasons=1980:2017)

team_ranks <-  
  team_ranks %>%
  rename(
    team = nameTeam,
    season = slugSeason
  )

ggplot() + 
  geom_line(aes(x=season, y=effRank, group=team), data=team_ranks, color='grey', alpha=0.7) +
  scale_y_continuous(trans = "reverse", breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  ggtitle("Regular Season Ranks Over Time") + 
  ylab('team rank') 