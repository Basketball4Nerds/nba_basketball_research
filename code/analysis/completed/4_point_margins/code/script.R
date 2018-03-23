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
