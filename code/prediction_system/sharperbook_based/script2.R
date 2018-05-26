## load libraries
library(tidyverse)
library(nbastatR)
library(ggplot2)


## get all playoffs season game logs
ps_games <- get_game_logs(seasons=1980:2017,
                          result_type='team',
                          season_types='Playoffs')


## define a function that adds point margin column
add_points_margin <- function(games_df) {
  ps_x <- games_df[ , c('idGame', 'nameTeam', 'ptsTeam')]
  ps_y <- games_df[ , c('idGame', 'nameTeam', 'ptsTeam')]
  names(ps_y) <- c('idGame', 'nameOpponent', 'ptsOpponent')
  ps_z <- merge(ps_x, ps_y, by='idGame')
  ps_z <- subset(ps_z, nameTeam != nameOpponent)
  ps_z$ptsMrgn <- ps_z$ptsTeam - ps_z$ptsOpponent
  ps_z <- ps_z[ , c('idGame', 'nameTeam', 'ptsMrgn')]
  games_df <- dplyr::left_join(games_df, ps_z, by=c('idGame', 'nameTeam'))
  return(games_df)
}


## add points margin column in the gamelogs dataset
ps_games <- add_points_margin(ps_games)


## create summarized version of playoffs gamelogs
ps_games_df <- ps_games %>%
  
  ## create seed aggregation df  
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  arrange(slugSeason, slugTeam, slugOpponent, dateGame) %>%
  slice(1) %>%
  mutate(
    isHigherSeed = case_when(
      locationGame=='H' ~ TRUE,
      locationGame=='A' ~ FALSE
    )
  ) %>%
  select(slugSeason, slugTeam, slugOpponent, isHigherSeed) %>%
  
  ## join with original playoffs gamelogs dataset
  left_join(ps_games, by=c('slugSeason', 'slugTeam', 'slugOpponent')) %>%
  
  ## select relevant columns
  select(slugSeason, dateGame, locationGame, slugTeam, slugOpponent, outcomeGame, ptsMrgn, isHigherSeed) %>%
  
  ## add game number 
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  arrange(slugSeason, slugTeam, slugOpponent, dateGame) %>%
  mutate(gameNumber = 1:n()) %>%
  
  as.data.frame()


x <- ps_games_df %>% 
  filter(isHigherSeed & gameNumber==1) %>%
  as.data.frame()
head(x)

x %>% ggplot() + 
  geom_histogram(aes(x=ptsMrgn), binwidth=1) + 
  ggtitle("Home Team Favorites: Distribution of Point Margins") + 
  geom_vline(xintercept=5, color='red')

spread = -5
payout = 100

## 



## 
if (spread < 0) {
  positive_outcome_prob = 1 - ecdf(x$ptsMrgn)(abs(spread))
} else {
  positive_outcome_prob = ecdf(x$ptsMrgn)(abs(spread))    
}
negative_outcome_prob = 1 - positive_outcome_prob

positive_outcome_prob
negative_outcome_prob

calc_expected_return(payout=-110, positive_outcome_prob=positive_outcome_prob)
