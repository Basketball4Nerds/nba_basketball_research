## load libraries
library(tidyverse)
library(nbastatR)


## get all regular season game logs
rs_games <- get_game_logs(seasons=1980:2017, 
                          result_type='team',
                          season_types='Regular Season')

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


## add points margin column in the game logs dataset
rs_games <- add_points_margin(rs_games)
ps_games <- add_points_margin(ps_games)




library(nbastatR)
library(tidyverse)
rs_games_2018 <- get_game_logs(seasons=2018, result_types='team', season_types='Regular Season')
ps_games_2018 <- get_game_logs(seasons=2018, result_types='team', season_types='Playoffs')

select_vars <- c('dateGame', 'slugTeam', 'slugOpponent', 'locationGame', 'ptsMrgn', 'outcomeGame')

rs_z_2018 <- rs_games_2018 %>% 
  add_points_margin() %>%
  select(select_vars) 

ps_z_2018 <- ps_games_2018 %>% 
  add_points_margin() %>%
  select(select_vars)


team = 'TOR'
opponent = 'CLE'
# 
team = 'BOS'
opponent = 'PHI'
# 
team = 'HOU'
opponent = 'UTA'
# 
team = 'GSW'
opponent = 'NOP'


sort(unique(rs_games$slugTeam))



rs_z_2018 %>% filter(slugTeam==team & slugOpponent==opponent)
ps_z_2018 %>% filter(slugTeam==team & slugOpponent==opponent)

?ecdf
hist(abs(ps_z$ptsMrgn))
median(abs(ps_z$ptsMrgn))
ecdf(abs(ps_z$ptsMrgn))(10.5)
## Do teams tend to lose the third game after the first two wins?

# All rounds are best-of-seven series. Series are played in a 2–2–1–1–1 format, 
# meaning the team with home-court advantage hosts games 1, 2, 5, and 7, while 
# their opponent hosts games 3, 4, and 6, with games 5–7 being played if needed. 
# This format has been used since 2014, after NBA team owners unanimously voted 
# to change from a 2–3–2 format on October 23, 2013.[2]

# 2-3-2 format since 1985: 
# http://blog.minitab.com/blog/the-statistics-game/the-2-3-2-format-vs-the-2-2-1-1-1-in-the-nba-finals
ps_games <- get_game_logs(seasons=1985:2017, result_types='team', season_types='Playoffs')
ps_games <- subset(ps_games, slugSeason != '2017-18')

## using regular season to predict playoff first matchup game
tt <- rs_games %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  summarise(
    wins = sum(outcomeGame=='W'),
    home_wins = sum(locationGame=='H' & outcomeGame=='W'),
    away_wins = sum(locationGame=='A' & outcomeGame=='W'),
    losses = sum(outcomeGame=='L'),
    home_losses = sum(locationGame=='H' & outcomeGame=='L'),
    away_losses = sum(locationGame=='A' & outcomeGame=='L')
  )
tt






## set previous game locations
previous_game_locations <- c('H', 'H', 'A', 'A')

## previous game outcomes
previous_game_outcomes <- c('W', 'W', 'W', 'L')

## number of previous games
n_prev_games <- length(previous_game_outcomes)

## index of game we are trying to predict
x <- n_prev_games + 1

## set next game location
next_game_location <- 'H'






#### NON-ORDER-SPECIFIC CONFIGURATION

zzz <- ps_games %>% 
  select(idGame, dateGame, locationGame, slugSeason, slugTeam, slugOpponent, outcomeGame, ptsMrgn) %>%
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  arrange(slugSeason, slugTeam, slugOpponent, dateGame) %>%
  filter(
    x <= n() &  # https://stackoverflow.com/questions/43110349/dplyr-filter-by-group-size
    locationGame[x] == 'H'
  ) %>%
  filter(
    all(sort(outcomeGame[1:n_prev_games]) == sort(previous_game_outcomes))
  ) %>%
  slice(x) %>%
  group_by(outcomeGame) %>%
  summarise(q25 = quantile(ptsMrgn, probs=0.25),
            q50 = quantile(ptsMrgn, probs=0.5),
            q75 = quantile(ptsMrgn, probs=0.75),
            avg = mean(ptsMrgn),
            n = n()) %>%
  as.data.frame()
zzz




####### ORDER-SPECIFIC site & game outcome configuration

www <- ps_games %>% 
  group_by(slugSeason, slugTeam, slugOpponent) %>%
  arrange(slugSeason, slugTeam, slugOpponent, dateGame) %>%
  filter(
    all(locationGame[1:n_prev_games] == previous_game_locations) &
    all(outcomeGame[1:n_prev_games] == c(previous_game_outcomes)) & 
    locationGame[x] == next_game_location
  ) %>%
  slice(x) %>%
  group_by(outcomeGame) %>% 
  summarise(q25 = quantile(ptsMrgn, probs=0.25),
            q50 = quantile(ptsMrgn, probs=0.5),
            q75 = quantile(ptsMrgn, probs=0.75),
            avg = mean(ptsMrgn),
            n = n()) %>%
  as.data.frame()
www


