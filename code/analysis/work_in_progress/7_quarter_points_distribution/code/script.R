#### load regular season games
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')



#### load libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(factoextra)  # for fviz_nbclust()
library(BBmisc)


#### 

## filter data with no overtime
games <- subset(games, p==rqP)

## select columns
games <- games[ , c('season', 'date', 'site', 'team', 'outcome', 'won', 'pMrgn',
                    'rqP', 'pQ1', 'pQ2', 'pQ3', 'pQ4', 'rqPA', 'pQ1A', 'pQ2A', 'pQ3A', 'pQ4A')]


## create additional columns
games$pQ1_share <- round(games$pQ1 / games$rqP, 3)
games$pQ2_share <- round(games$pQ2 / games$rqP, 3)
games$pQ3_share <- round(games$pQ3 / games$rqP, 3)
games$pQ4_share <- round(games$pQ4 / games$rqP, 3)

games$pQ1A_share <- round(games$pQ1A / games$rqPA, 3)
games$pQ2A_share <- round(games$pQ2A / games$rqPA, 3)
games$pQ3A_share <- round(games$pQ3A / games$rqPA, 3)
games$pQ4A_share <- round(games$pQ4A / games$rqPA, 3)



#### quarter points distribution profiling: 
#### determining optimal number of k for k-means clustering;
#### based on https://uc-r.github.io/kmeans_clustering

set.seed(123)
qp_share_df <- games[ , c('pQ1_share', 'pQ2_share', 'pQ3_share', 'pQ4_share')]
qp_df <- games[ , c('pQ1', 'pQ2', 'pQ3', 'pQ4')]

k_values <- 1:15
wss_values <- c()
for (k in k_values) {
  wss <- kmeans(qp_share_df, k, nstart = 10 )$tot.withinss
  wss_values <- c(wss_values, wss)
  print(k)  
}

plot(k_values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



#### 
set.seed(123)
km15 <- kmeans(qp_share_df, centers=12)
km15
km15_df <- as.data.frame.matrix(t(table(games$outcome, km15$cluster)))
km15_df$wpc <- round(km15_df$won / (km15_df$lost + km15_df$won), 3)
km15_df$cluster <- rownames(km15_df)

sortByCol(km15_df, col='wpc', asc=FALSE)

km15


km15_df <- as.data.frame.matrix(t(table(games$outcome, km15$cluster)))
km15_df$wpc <- round(km15_df$won / (km15_df$lost + km15_df$won), 3)
km15_df$cluster <- rownames(km15_df)


games2015 <- subset(games, season==2015)
warriors2015 <- subset(games2015, team=='Warriors')
cavaliers2015 <- subset(games2015, team=='Cavaliers')

ggplot(warriors2015) + 
  geom_boxplot(aes(x=team, y=pQ1_share))
ggplot(warriors2015) + 
  geom_boxplot(aes(x=team, y=pQ2_share))
ggplot(warriors2015) + 
  geom_boxplot(aes(x=team, y=pQ4_share))


ggplot(cavaliers2015) + 
  geom_boxplot(aes(x=team, y=pQ1_share))
ggplot(cavaliers2015) + 
  geom_boxplot(aes(x=team, y=pQ2_share))
ggplot(cavaliers2015) + 
  geom_boxplot(aes(x=team, y=pQ3_share))
ggplot(cavaliers2015) + 
  geom_boxplot(aes(x=team, y=pQ4_share))

head()
close_games <- subset(games, abs(pMrgn) <= 3)
close_games_won <- subset(close_games, won)
close_games_lost <- subset(close_games, !won)

ggplot(close_games_won) + 
  geom_boxplot(aes(x=outcome, y=pQ4_share))
km15


subset(games, season==2015 & team=='Warriors' & abs(pMrgn) <=5)
