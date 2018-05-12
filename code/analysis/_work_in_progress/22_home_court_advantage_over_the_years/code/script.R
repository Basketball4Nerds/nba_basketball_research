library(nbastatR)
library(tidyverse)
library(ggplot2)


## get all regular season game logs
rs_games <- get_game_logs(seasons=1980:2017, 
                          result_type='team',
                          season_types='Regular Season')

## get all playoffs season game logs
ps_games <- get_game_logs(seasons=1980:2017,
                          result_type='team',
                          season_types='Playoffs')

# write.csv(rs_games, 'rs_games.csv', row.names=FALSE)
# write.csv(ps_games, 'ps_games.csv', row.names=FALSE)


x <- rs_games %>%
  filter(!is.na(outcomeGame)) %>%
  group_by(slugSeason, locationGame) %>%
  summarize(mean_pts = mean(ptsTeam)) %>%
  spread(key=locationGame, value=mean_pts) %>%
  set_names(c('slugSeason', 'mean_pts_A', 'mean_pts_H')) %>%
  mutate(
    mean_pts_diff = mean_pts_H - mean_pts_A
  )

y <- rs_games %>%
  filter(!is.na(outcomeGame)) %>%
  group_by(slugSeason) %>%
  summarize(mean_pts = mean(ptsTeam))

z <- x %>%
  left_join(y, by=c('slugSeason')) %>%
  mutate(
    mean_pts_diff_nrm = mean_pts_diff / mean_pts * 100
  )

  

ggplot(z) + 
  geom_point(aes(x=slugSeason, y=mean_pts_diff_nrm)) + 
  geom_line(aes(x=slugSeason, y=mean_pts_diff_nrm, group=1)) 


ggplot(x) + 
  geom_point(aes(x=slugSeason, y=mean_pts_mrgn, color=locationGame))


