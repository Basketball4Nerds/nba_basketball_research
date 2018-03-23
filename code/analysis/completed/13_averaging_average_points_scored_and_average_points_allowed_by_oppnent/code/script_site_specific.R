##
remove(list=ls())

## load regular season games
games <- read.csv("../../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')
games$team_type <- ifelse(games$line < 0, 'favorite', 
                          ifelse(games$line > 0, 'underdog', NA))


## load libraries
library(knitr)
library(dplyr)
library(zoo)
library(BBmisc)
library(Metrics)
library(ggplot2)
source('../../../helper/fill_in_opp_cols.R')



## select relevant cols
base_cols <- c('season', 'date', 'site', 'playoffs', 'team', 'o_team', 'outcome', 'rqP', 'rqPA')
games <- games[ , base_cols]


## add site-specific rolling-mean and cummulative-mean metrics
games <- games %>%
  group_by(season, team, site) %>%
  arrange(date) %>%
  mutate(
    rqP_rollmean5_site = rollmean(rqP, k=5, fill=NA, align='right'),
    rqP_rollmean10_site = rollmean(rqP, k=10, fill=NA, align='right'),
    rqP_cummean_site = cummean(rqP),
    rqPA_rollmean5_site = rollmean(rqPA, k=5, fill=NA, align='right'),
    rqPA_rollmean10_site = rollmean(rqPA, k=10, fill=NA, align='right'),
    rqPA_cummean_site = cummean(rqPA)
  ) %>%
  mutate(
    rqP_rollmean5_site = c(NA, rqP_rollmean5_site)[-length(rqP_rollmean5_site)],
    rqP_rollmean10_site = c(NA, rqP_rollmean10_site)[-length(rqP_rollmean10_site)],
    rqP_cummean_site = c(NA, rqP_cummean_site)[-length(rqP_cummean_site)],
    rqPA_rollmean5_site = c(NA, rqPA_rollmean5_site)[-length(rqPA_rollmean5_site)],
    rqPA_rollmean10_site = c(NA, rqPA_rollmean10_site)[-length(rqPA_rollmean10_site)],
    rqPA_cummean_site = c(NA, rqPA_cummean_site)[-length(rqPA_cummean_site)]
  )
games <- as.data.frame(games)


## fill in opposite team cols
new_cols <- setdiff(colnames(games), base_cols)
games <- fill_in_opp_cols(games, cols=new_cols)


## filter by complete cases
games <- games[!is.na(games$rqP_rollmean5_site) & !is.na(games$o_rqPA_rollmean5_site), ]
#games <- games[complete.cases(games), ]


## add averaged rqP projections
games$rqP_rollmean5_site_proj <- (games$rqP_rollmean5_site + games$o_rqPA_rollmean5_site) / 2
games$rqP_rollmean10_site_proj <- (games$rqP_rollmean10_site + games$o_rqPA_rollmean10_site) / 2
games$rqP_cummean_site_proj <- (games$rqP_cummean_site + games$o_rqPA_cummean_site) / 2


## correlation between prediction var and predictor vars
cor(games$rqP, games$rqP_rollmean5_site, use='pairwise.complete.obs')
cor(games$rqP, games$rqP_rollmean10_site, use='pairwise.complete.obs')
cor(games$rqP, games$rqP_cummean_site, use='pairwise.complete.obs')

cor(games$rqP, games$o_rqPA_rollmean5_site, use='pairwise.complete.obs')
cor(games$rqP, games$o_rqPA_rollmean10_site, use='pairwise.complete.obs')
cor(games$rqP, games$o_rqPA_cummean_site, use='pairwise.complete.obs')

cor(games$rqP, games$rqP_rollmean5_site_proj, use='pairwise.complete.obs')
cor(games$rqP, games$rqP_rollmean10_site_proj, use='pairwise.complete.obs')
cor(games$rqP, games$rqP_cummean_site_proj, use='pairwise.complete.obs')


## mean absolute error
mean(abs(games$rqP - games$rqP_rollmean5_site), na.rm=TRUE)
mean(abs(games$rqP - games$rqP_rollmean10_site), na.rm=TRUE)
mean(abs(games$rqP - games$rqP_cummean_site), na.rm=TRUE)

mean(abs(games$rqP - games$o_rqPA_rollmean5_site), na.rm=TRUE)
mean(abs(games$rqP - games$o_rqPA_rollmean10_site), na.rm=TRUE)
mean(abs(games$rqP - games$o_rqPA_cummean_site), na.rm=TRUE)

mean(abs(games$rqP - games$rqP_rollmean5_site_proj), na.rm=TRUE)
mean(abs(games$rqP - games$rqP_rollmean10_site_proj), na.rm=TRUE)
mean(abs(games$rqP - games$rqP_cummean_site_proj), na.rm=TRUE)



## create a new games df (with projection's high's and low's)
games_proj_df <-
  data.frame(
    rqP_rollmean5_site_proj_low = pmin(games$rqP_rollmean5_site, games$o_rqPA_rollmean5_site),
    rqP_rollmean5_site_proj_high = pmax(games$rqP_rollmean5_site, games$o_rqPA_rollmean5_site),
    
    rqP_rollmean10_site_proj_low = pmin(games$rqP_rollmean10_site, games$o_rqPA_rollmean10_site),
    rqP_rollmean10_site_proj_high = pmax(games$rqP_rollmean10_site, games$o_rqPA_rollmean10_site),
    
    rqP_cummean_site_proj_low = pmin(games$rqP_cummean_site, games$o_rqPA_cummean_site),
    rqP_cummean_site_proj_high = pmax(games$rqP_cummean_site, games$o_rqPA_cummean_site)
  )
games_proj_df <- cbind(games[ , c(base_cols)], games_proj_df)



## see how often the actual regular-quarter points fall in the rolling-mean and cummulative-mean projection range
x <- ifelse(games_proj_df$rqP < games_proj_df$rqP_rollmean5_site_proj_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$rqP_rollmean5_site_proj_high, 'over projected range', 'within range'))
table(x)

y <- ifelse(games_proj_df$rqP < games_proj_df$rqP_rollmean10_site_proj_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$rqP_rollmean10_site_proj_high, 'over projected range', 'within range'))
table(y)

z <- ifelse(games_proj_df$rqP < games_proj_df$rqP_cummean_site_proj_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$rqP_cummean_site_proj_high, 'over projected range', 'within range'))
table(z)


## 
cavs2015 <- subset(games_proj_df, season==2015 & team=='Cavaliers')


ggplot(cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=rqP_rollmean5_site_proj_low), color='blue') + 
  geom_line(aes(x=date, y=rqP_rollmean5_site_proj_low, group=1), color='blue') + 
  geom_point(aes(x=date, y=rqP_rollmean5_site_proj_high), color='red') + 
  geom_line(aes(x=date, y=rqP_rollmean5_site_proj_high, group=1), color='red') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')
