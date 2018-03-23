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


## add rolling-mean and cummulative-mean metrics
games <- games %>%
  group_by(season, team) %>%
  arrange(date) %>%
  mutate(
    rqP_rollmean5_gen = rollmean(rqP, k=5, fill=NA, align='right'),
    rqP_rollmean10_gen = rollmean(rqP, k=10, fill=NA, align='right'),
    rqP_cummean_gen = cummean(rqP),
    rqPA_rollmean5_gen = rollmean(rqPA, k=5, fill=NA, align='right'),
    rqPA_rollmean10_gen = rollmean(rqPA, k=10, fill=NA, align='right'),
    rqPA_cummean_gen = cummean(rqPA)
  ) %>%
  mutate(
    rqP_rollmean5_gen = c(NA, rqP_rollmean5_gen)[-length(rqP_rollmean5_gen)],
    rqP_rollmean10_gen = c(NA, rqP_rollmean10_gen)[-length(rqP_rollmean10_gen)],
    rqP_cummean_gen = c(NA, rqP_cummean_gen)[-length(rqP_cummean_gen)],
    rqPA_rollmean5_gen = c(NA, rqPA_rollmean5_gen)[-length(rqPA_rollmean5_gen)],
    rqPA_rollmean10_gen = c(NA, rqPA_rollmean10_gen)[-length(rqPA_rollmean10_gen)],
    rqPA_cummean_gen = c(NA, rqPA_cummean_gen)[-length(rqPA_cummean_gen)]
  )
games <- as.data.frame(games)


## fill in opposite team cols
new_cols <- setdiff(colnames(games), base_cols)
games <- fill_in_opp_cols(games, cols=new_cols)


## filter by complete cases
games <- games[complete.cases(games), ]


## add averaged rqP projections
games$rqP_rollmean5_gen_proj <- (games$rqP_rollmean5_gen + games$o_rqPA_rollmean5_gen) / 2
games$rqP_rollmean10_gen_proj <- (games$rqP_rollmean10_gen + games$o_rqPA_rollmean10_gen) / 2
games$rqP_cummean_gen_proj <- (games$rqP_cummean_gen + games$o_rqPA_cummean_gen) / 2


## correlation between prediction var and predictor vars
cor(games$rqP, games$rqP_rollmean5_gen)
cor(games$rqP, games$rqP_rollmean10_gen)
cor(games$rqP, games$rqP_cummean_gen)

cor(games$rqP, games$o_rqPA_rollmean5_gen)
cor(games$rqP, games$o_rqPA_rollmean10_gen)
cor(games$rqP, games$o_rqPA_cummean_gen)

cor(games$rqP, games$rqP_rollmean5_gen_proj)
cor(games$rqP, games$rqP_rollmean10_gen_proj)
cor(games$rqP, games$rqP_cummean_gen_proj)


## mean absolute error
mae(games$rqP, games$rqP_rollmean5_gen)
mae(games$rqP, games$rqP_rollmean10_gen)
mae(games$rqP, games$rqP_cummean_gen)

mae(games$rqP, games$o_rqPA_rollmean5_gen)
mae(games$rqP, games$o_rqPA_rollmean10_gen)
mae(games$rqP, games$o_rqPA_cummean_gen)

mae(games$rqP, games$rqP_rollmean5_gen_proj)
mae(games$rqP, games$rqP_rollmean10_gen_proj)
mae(games$rqP, games$rqP_cummean_gen_proj)


## create a new games df (with projection's high's and low's)
games_proj_df <-
  data.frame(
    rqP_rollmean5_gen_proj_low = pmin(games$rqP_rollmean5_gen, games$o_rqPA_rollmean5_gen),
    rqP_rollmean5_gen_proj_high = pmax(games$rqP_rollmean5_gen, games$o_rqPA_rollmean5_gen),
    
    rqP_rollmean10_gen_proj_low = pmin(games$rqP_rollmean10_gen, games$o_rqPA_rollmean10_gen),
    rqP_rollmean10_gen_proj_high = pmax(games$rqP_rollmean10_gen, games$o_rqPA_rollmean10_gen),
    
    rqP_cummean_gen_proj_low = pmin(games$rqP_cummean_gen, games$o_rqPA_cummean_gen),
    rqP_cummean_gen_proj_high = pmax(games$rqP_cummean_gen, games$o_rqPA_cummean_gen)
  )
games_proj_df <- cbind(games[ , c(base_cols)], games_proj_df)



## see how often the actual regular-quarter points fall in the rolling-mean and cummulative-mean projection range
x <- ifelse(games_proj_df$rqP < games_proj_df$rqP_rollmean5_gen_proj_low, 'under projected range',
       ifelse(games_proj_df$rqP > games_proj_df$rqP_rollmean5_gen_proj_high, 'over projected range', 'within range'))
table(x)

y <- ifelse(games_proj_df$rqP < games_proj_df$rqP_rollmean10_gen_proj_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$rqP_rollmean10_gen_proj_high, 'over projected range', 'within range'))
table(y)

z <- ifelse(games_proj_df$rqP < games_proj_df$rqP_cummean_gen_proj_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$rqP_cummean_gen_proj_high, 'over projected range', 'within range'))
table(z)


## 
cavs2015 <- subset(games_proj_df, season==2015 & team=='Cavaliers')


ggplot(cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=rqP_rollmean5_gen_proj_low), color='blue') + 
  geom_line(aes(x=date, y=rqP_rollmean5_gen_proj_low, group=1), color='blue') + 
  geom_point(aes(x=date, y=rqP_rollmean5_gen_proj_high), color='red') + 
  geom_line(aes(x=date, y=rqP_rollmean5_gen_proj_high, group=1), color='red') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')
