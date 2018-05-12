#### load regular season games
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')


#### load libraries
library(ggplot2)
library(dplyr)  
library(knitr)  # for kable()
library(kableExtra)  # for kable()


#### relationship between offensive rebounds and game outcome
tapply(games$oRb, games$outcome, mean, na.rm=TRUE)
tapply(games$oRb, list(games$season, games$outcome), mean, na.rm=TRUE)

## testing statistical significance with independent t-test
t.test(oRb ~ outcome, data=games)



#### relationship between offensive rebounds and game outcome
tapply(games$dRb, games$outcome, mean, na.rm=TRUE)

## testing statistical significance with independent t-test
t.test(dRb ~ outcome, data=games)



#### relationship between number of points and offensive rebounds
cor(games$oRb, games$p, use='complete.obs')
cor(games$dRb, games$p, use='complete.obs')

cor.test(games$oRb, games$p)
cor.test(games$dRb, games$p)


#### relationship between FGP and offensive rebounds
cor(games$oRb, games$FGP, use='complete.obs')

ggplot(games, aes(x=FGP, y=oRb)) + 
  geom_point(alpha=0.1) + 
  geom_smooth(method='lm') + 
  ggtitle('FGP vs. Offensive Rebounds')



#### visualizing
avg_o_rbnds_by_ssn_df <- tapply(games$oRb, list(games$season, games$outcome), mean, na.rm=TRUE)
kable(avg_o_rbnds_by_ssn_df, "html") %>%
  kable_styling() %>%
  scroll_box(width = "100%", height = "100%")


x <- tapply(games$oRb, games$outcome, mean, na.rm=TRUE)
x <- tapply(games$oRb, list(games$season, games$outcome), mean, na.rm=TRUE)
