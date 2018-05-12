#### load data
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$team_type <- ifelse(games$line < 0, 'favorite',
                          ifelse(games$line > 0, 'underdog', NA))


#### load libraries
library(ggplot2)



#### analysis
mean(games$fl)
tapply(games$fl, games$site, mean)
a <- tapply(games$fl, list(games$site, games$outcome), mean)
b <- tapply(games$fl, list(games$site, games$team_type), mean)
c <- tapply(games$fl, list(games$site, games$team_type, games$outcome), mean)

t(as.data.frame(a))
t(as.data.frame(b))
t(as.data.frame(c))


#### close games
close_games <- subset(games, abs(pMrgn) <= 3)

mean(close_games$fl)
tapply(close_games$fl, close_games$site, mean)
x <- tapply(close_games$fl, list(close_games$site, close_games$outcome), mean)
y <- tapply(close_games$fl, list(close_games$site, close_games$team_type), mean)
z <- tapply(close_games$fl, list(close_games$site, close_games$team_type, close_games$outcome), mean)

t(as.data.frame(x))
t(as.data.frame(y))
t(as.data.frame(z))


table(games$team_type, games$site)


## What is the average number of fouls that a team receives during game?
# Does this number differ for home team vs. away team?
# 
# - Do home underdogs get called less fouls?
# - Do away favorites receive more fouls?
# 
# - Do home underdogs (who eventually go on to lose the game) get called even less fouls?
# - Do away favorites (who'd eventually go on to win the game) get called even more fouls?
#
# - maybe during close games, refs call more fouls against away favorites
# {
#   - games that ended in small difference
#   - games that had lots of lead changes
# }

                     - 
                     