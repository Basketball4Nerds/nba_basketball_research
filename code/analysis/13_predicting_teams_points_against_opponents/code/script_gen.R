## create indices for data points with minimum n_gen
ind_min_n5_gen <- rs_games$n_gen >= 5 & rs_games$o_n_gen >= 5
ind_min_n10_gen <- rs_games$n_gen >= 10 & rs_games$o_n_gen >= 10


## calculate averaged rqP projections
rs_games$pred_rqP_rollmean5_gen <- (rs_games$rqP_rollmean5_gen + rs_games$o_rqPA_rollmean5_gen) / 2
rs_games$pred_rqP_rollmean10_gen <- (rs_games$rqP_rollmean10_gen + rs_games$o_rqPA_rollmean10_gen) / 2
rs_games$pred_rqP_cummean_gen <- (rs_games$rqP_cummean_gen + rs_games$o_rqPA_cummean_gen) / 2

rs_games$o_pred_rqP_rollmean5_gen <- (rs_games$o_rqP_rollmean5_gen + rs_games$rqPA_rollmean5_gen) / 2
rs_games$o_pred_rqP_rollmean10_gen <- (rs_games$o_rqP_rollmean10_gen + rs_games$rqPA_rollmean10_gen) / 2
rs_games$o_pred_rqP_cummean_gen <- (rs_games$o_rqP_cummean_gen + rs_games$rqPA_cummean_gen) / 2


## correlation between prediction var and predictor vars
round(cor(rs_games$rqP, rs_games$rqP_rollmean5_gen, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$rqP_rollmean10_gen, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_gen], rs_games$rqP_cummean_gen[ind_min_n5_gen], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_gen], rs_games$rqP_cummean_gen[ind_min_n10_gen], use='pairwise.complete.obs'), 3)

round(cor(rs_games$rqP, rs_games$o_rqPA_rollmean5_gen, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$o_rqPA_rollmean10_gen, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_gen], rs_games$o_rqPA_cummean_gen[ind_min_n5_gen], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_gen], rs_games$o_rqPA_cummean_gen[ind_min_n10_gen], use='pairwise.complete.obs'), 3)

round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean5_gen, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean10_gen, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_gen], rs_games$pred_rqP_cummean_gen[ind_min_n5_gen], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_gen], rs_games$pred_rqP_cummean_gen[ind_min_n10_gen], use='pairwise.complete.obs'), 3)


## mean absolute error
round(mean(abs(rs_games$rqP - rs_games$rqP_rollmean5_gen), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$rqP_rollmean10_gen), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_gen] - rs_games$rqP_cummean_gen[ind_min_n5_gen]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_gen] - rs_games$rqP_cummean_gen[ind_min_n10_gen]), na.rm=TRUE), 3)

round(mean(abs(rs_games$rqP - rs_games$o_rqPA_rollmean5_gen), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$o_rqPA_rollmean10_gen), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_gen] - rs_games$o_rqPA_cummean_gen[ind_min_n5_gen]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_gen] - rs_games$o_rqPA_cummean_gen[ind_min_n10_gen]), na.rm=TRUE), 3)

round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean5_gen), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean10_gen), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_gen] - rs_games$pred_rqP_cummean_gen[ind_min_n5_gen]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_gen] - rs_games$pred_rqP_cummean_gen[ind_min_n10_gen]), na.rm=TRUE), 3)








## create a new games df (with projection's high's and low's)
games_proj_df <-
  data.frame(
    pred_rqP_rollmean5_gen_low = pmin(rs_games$rqP_rollmean5_gen, rs_games$o_rqPA_rollmean5_gen),
    pred_rqP_rollmean5_gen_high = pmax(rs_games$rqP_rollmean5_gen, rs_games$o_rqPA_rollmean5_gen),
    
    pred_rqP_rollmean10_gen_low = pmin(rs_games$rqP_rollmean10_gen, rs_games$o_rqPA_rollmean10_gen),
    pred_rqP_rollmean10_gen_high = pmax(rs_games$rqP_rollmean10_gen, rs_games$o_rqPA_rollmean10_gen),
    
    pred_rqP_cummean_gen_low = pmin(rs_games$rqP_cummean_gen, rs_games$o_rqPA_cummean_gen),
    pred_rqP_cummean_gen_high = pmax(rs_games$rqP_cummean_gen, rs_games$o_rqPA_cummean_gen)
  )
games_proj_df <- cbind(rs_games, games_proj_df)



## see how often the actual regular-quarter points fall in the rolling-mean and cummulative-mean projection range
x <- ifelse(games_proj_df$rqP < games_proj_df$pred_rqP_rollmean5_gen_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$pred_rqP_rollmean5_gen_high, 'over projected range', 'within range'))
table(x)

y <- ifelse(games_proj_df$rqP < games_proj_df$pred_rqP_rollmean10_gen_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$pred_rqP_rollmean10_gen_high, 'over projected range', 'within range'))
table(y)

z <- ifelse(games_proj_df$rqP < games_proj_df$pred_rqP_cummean_gen_low, 'under projected range',
            ifelse(games_proj_df$rqP > games_proj_df$pred_rqP_cummean_gen_high, 'over projected range', 'within range'))
table(z)


## 
rs_cavs2015 <- subset(games_proj_df, season==2015 & team=='Cavaliers')
rs_cavs2015 <- rs_cavs2015[complete.cases(rs_cavs2015), ]


ggplot(rs_cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=rqP_rollmean5_gen), color='green') + 
  geom_point(aes(x=date, y=o_rqPA_rollmean5_gen), color='orange') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')

ggplot(rs_cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=rqP_rollmean10_gen), color='green') + 
  geom_point(aes(x=date, y=o_rqPA_rollmean10_gen), color='orange') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')

ggplot(rs_cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=rqP_cummean_gen), color='green') + 
  geom_point(aes(x=date, y=o_rqPA_cummean_gen), color='orange') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')



ggplot(rs_cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=pred_rqP_rollmean5_gen_low), color='blue') + 
  geom_line(aes(x=date, y=pred_rqP_rollmean5_gen_low, group=1), color='blue') + 
  geom_point(aes(x=date, y=pred_rqP_rollmean5_gen_high), color='red') + 
  geom_line(aes(x=date, y=pred_rqP_rollmean5_gen_high, group=1), color='red') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')

ggplot(rs_cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=pred_rqP_rollmean10_gen_low), color='blue') + 
  geom_line(aes(x=date, y=pred_rqP_rollmean10_gen_low, group=1), color='blue') + 
  geom_point(aes(x=date, y=pred_rqP_rollmean10_gen_high), color='red') + 
  geom_line(aes(x=date, y=pred_rqP_rollmean10_gen_high, group=1), color='red') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')

ggplot(rs_cavs2015) + 
  geom_point(aes(x=date, y=rqP)) + 
  geom_point(aes(x=date, y=pred_rqP_cummean_gen_low), color='blue') + 
  geom_line(aes(x=date, y=pred_rqP_cummean_gen_low, group=1), color='blue') + 
  geom_point(aes(x=date, y=pred_rqP_cummean_gen_high), color='red') + 
  geom_line(aes(x=date, y=pred_rqP_cummean_gen_high, group=1), color='red') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  ggtitle("Cavaliers 2015-2016 Reg. Season: Regular-Quarter Points Scored by Date") + 
  ylab('regular-quarter points')

