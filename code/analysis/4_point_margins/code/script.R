#### load regular season games
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')
games$team_type <- ifelse(games$line < 0, 'favorite', 
                          ifelse(games$line > 0, 'underdog', NA))

#### loading libraries
library(ggplot2)
library(gridExtra)
library(psych)



#### distribution of point margins for all games

## raw
ggplot(games) + 
  geom_histogram(aes(x=pMrgn), binwidth=1) +
  ggtitle("Distribution of Game Point Margins") 

## absolute value
qntl <- quantile(abs(games$pMrgn), probs=c(.25, .5, .75, .9, 1))
ggplot(games) + 
  geom_histogram(aes(x=abs(pMrgn)), binwidth=1) + 
  geom_vline(xintercept=qntl[1], color='orange') +   
  geom_vline(xintercept=qntl[2], color='yellow') +   
  geom_vline(xintercept=qntl[3], color='green') + 
  geom_vline(xintercept=qntl[4], color='blue', linetype='dotted') + 
  ggtitle('Distribution of Game Point Margins (Absolute Values)')




#### subset datasets
home <- subset(games, site=='home')
away <- subset(games, site=='away')

a <- ggplot(home) + 
  geom_histogram(aes(x=pMrgn), binwidth=1) +
  ggtitle('Home Games') + 
  scale_x_continuous(limits=c(-65, 65))

b <- ggplot(away) + 
  geom_histogram(aes(x=pMrgn), binwidth=1) + 
  ggtitle('Away Games') + 
  scale_x_continuous(limits=c(-65, 65))

grid.arrange(a, b, top='Distribution of Point Margins by Game Site')



#### 
home_favorite <- subset(games, 
                        site=='home' & team_type=='favorite')

ggplot(home_favorite) + 
  geom_histogram(aes(x=pMrgn), binwidth=1) + 
  ggtitle("Home Team Favorites: Distribution of Point Margins") + 
  geom_vline(xintercept=0, color='red')
ecdf(home_favorite$pMrgn)(0)

ggplot(home_favorite) + 
  geom_histogram(aes(x=pMrgn), binwidth=1) + 
  ggtitle("Home Team Favorites: Distribution of Point Margins") + 
  geom_vline(xintercept=5, color='red')
ecdf(home_favorite$pMrgn)(5)



#### winning at home vs. away
home_wins <- subset(games, site=='home' & outcome=='won')
away_wins <- subset(games, site=='away' & outcome=='won')

quantile(abs(home_wins$pMrgn), probs=c(.25, .5, .75, .9, 1))
quantile(abs(away_wins$pMrgn), probs=c(.25, .5, .75, .9, 1))

ecdf(home_wins$pMrgn)(10)

# #########
# 
# library(nbastatR)
# library(tidyverse)
# library(ggplot2)
# 
# 
# ## get all regular season game logs
# rs_games <- get_game_logs(seasons=1980:2017,
#                           result_type='team',
#                           season_types='Regular Season')
# 
# ## get all playoffs season game logs
# ps_games <- get_game_logs(seasons=1980:2017,
#                           result_type='team',
#                           season_types='Playoffs')
# 
# 
# ## point margins for regular seasons
# rs_x <- rs_games[ , c('idGame', 'nameTeam', 'ptsTeam')]
# rs_y <- rs_games[ , c('idGame', 'nameTeam', 'ptsTeam')]
# names(rs_y) <- c('idGame', 'nameOpponent', 'ptsOpponent')
# rs_z <- merge(rs_x, rs_y, by='idGame')
# rs_z <- subset(rs_z, nameTeam != nameOpponent)
# rs_z$ptsMrgn <- rs_z$ptsTeam - rs_z$ptsOpponent
# 
# 
# ## point margins for playoffs seasons
# ps_x <- ps_games[ , c('idGame', 'nameTeam', 'ptsTeam')]
# ps_y <- ps_games[ , c('idGame', 'nameTeam', 'ptsTeam')]
# names(ps_y) <- c('idGame', 'nameOpponent', 'ptsOpponent')
# ps_z <- merge(ps_x, ps_y, by='idGame')
# ps_z <- subset(ps_z, nameTeam != nameOpponent)
# ps_z$ptsMrgn <- ps_z$ptsTeam - ps_z$ptsOpponent
# 
# 
# ## calculate quantiles
# rs_qntl <- quantile(abs(rs_z$ptsMrgn), probs=c(0.25, 0.5, 0.75, 0.9, 1))
# ps_qntl <- quantile(abs(ps_z$ptsMrgn), probs=c(0.25, 0.5, 0.75, 0.9, 1))
# rs_qntl
# ps_qntl
# 
# 
# ## regular season point margins
# rs_z_plt <- ggplot(rs_z) +
#   geom_histogram(aes(x=abs(ptsMrgn)), binwidth=1) +
#   geom_vline(xintercept=rs_qntl[1], color='orange') +
#   geom_vline(xintercept=rs_qntl[2], color='yellow') +
#   geom_vline(xintercept=rs_qntl[3], color='green') +
#   geom_vline(xintercept=rs_qntl[4], color='blue', linetype='dotted') +
#   ggtitle('Distribution of Regular Season Games Point Margins')
# rs_z_plt
# 
# 
# ## playoffs season point margins
# ps_z_plt <- ggplot(ps_z) +
#   geom_histogram(aes(x=abs(ptsMrgn)), binwidth=1) +
#   geom_vline(xintercept=ps_qntl[1], color='orange') +
#   geom_vline(xintercept=ps_qntl[2], color='yellow') +
#   geom_vline(xintercept=ps_qntl[3], color='green') +
#   geom_vline(xintercept=ps_qntl[4], color='blue', linetype='dotted') +
#   ggtitle('Distribution of Playoffs Season Games Point Margins')
# ps_z_plt




