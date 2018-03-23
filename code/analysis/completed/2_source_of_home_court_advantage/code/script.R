#### load regular season games
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')



#### load libraries
library(ggplot2)
library(gridExtra)


#### game outcomes by site
table(games$outcome, games$site)



#### mean scored points by site
tapply(games$p, games$site, mean)

# a <- ggplot(games) + 
#   geom_boxplot(aes(x=site, y=p, fill=site)) + 
#   ylab('scored points')
# b <- ggplot(games, aes(x=p)) + 
#   geom_density(aes(group=site, color=site, fill=site), alpha=0.3) + 
#   xlab('scored points')
# grid.arrange(a, b, ncol=2, 
#              top="Distribution of Scored Points by Game Site")

b <- ggplot(games, aes(x=p)) +
  geom_density(aes(group=site, color=site, fill=site), alpha=0.3) +
  xlab('scored points') + 
  ggtitle("Distribution of Scored Points by Game Site")
b


#### mean scored points by site (adjusted for game outcome)
tapply(games$p, list(games$site, games$outcome), mean)

won_games <- subset(games, won)
lost_games <- subset(games, !won)

ggplot(won_games) + 
  geom_boxplot(aes(x=site, y=p, fill=site)) 
ggplot(won_games, aes(x=p)) + 
  geom_density(aes(group=site, color=site, fill=site), alpha=0.3)

ggplot(lost_games) + 
  geom_boxplot(aes(x=site, y=p, fill=site)) 
ggplot(lost_games, aes(x=p)) + 
  geom_density(aes(group=site, color=site, fill=site), alpha=0.3)



#### adjusting the role of site 
model <- lm(p ~ outcome + site, data=games)
summary(model)



#### 
table(is.na(games$rst))
unique(games$rst)
hist(games$rst)
table(games$rst)

tapply(games$p, list(games$site, games$outcome, games$rst), mean)
