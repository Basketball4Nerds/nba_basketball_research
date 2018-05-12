## In this track, we will pick the best metric from each category. Then 
## we will employ voting methods to predict win.
##
## Note that we need not optimize the win prediction accuracy.
## If we do, we would be inclined to bet only on the favorites.
## Our overall goal is NOT to optimize the win pred accuracy 
## but to optimize the wager earning, which is
## done through expected value calculations.
## Depending on the payout (which affects the EV), we
## may bet on underdogs though it has less-than-50-percent chance
## of winning the game.


## track 1 predictors forked over from script5_track1.R;
## these predictors include cumperf predictors that are 
## considered best each category; 
##
## "rqPA_cumperf_oeffQntlRnk" was considered a better predictor than
## "rqPA_cumperf_site" but the latter was chosen over the former
## for the easy of preprocess computing and to (slightly) increase 
## number of predictions
trk1_predictors <- c('wpc_site', 
                     'oeff_cumperf_site', 'oeffA_cumperf_site', 
                     'FGP_cumperf_site', 'FGPA_cumperf_site', 
                     'rqP_cumperf_site', 'rqPA_cumperf_site', 'j5', 
                     'line', 'home', 'mtchmrgn')


## remove highly correlated variables via VIF calc
trk1_trimmed_predictors <- vif_func(in_frame=train[ , trk1_predictors], 
                                    thresh=5, trace=TRUE)
trk1_trimmed_predictors


## create win probability dfs
trk1_wprob_df <- create_wprob_df(train_complete, predictors=trk1_predictors)
trk1_wprob_df2 <- create_wprob_df(train_complete, predictors=trk1_trimmed_predictors)


## join with the original moneylines data
trk1_wprob_df <- left_join(moneylines, trk1_wprob_df, by=c('season', 'date', 'team', 'o_team'))
trk1_wprob_df2 <- left_join(moneylines, trk1_wprob_df2, by=c('season', 'date', 'team', 'o_team'))


## filter for complete cases
trk1_wprob_df <- trk1_wprob_df[complete.cases(trk1_wprob_df), ]
trk1_wprob_df2 <- trk1_wprob_df2[complete.cases(trk1_wprob_df2), ]


## add expected value col (favorable to bet if EV > 0)
trk1_wprob_df$expval <- calc_exp_val(wgr_amt=100, 
                                     moneyline_odds=trk1_wprob_df$bookmaker_ml, 
                                     w_prob=trk1_wprob_df$wprob)
trk1_wprob_df2$expval <- calc_exp_val(wgr_amt=100, 
                                      moneyline_odds=trk1_wprob_df2$bookmaker_ml, 
                                      w_prob=trk1_wprob_df2$wprob)


## place bet if EV is above certain threshold
min_expval_thres <- 20
trk1_wprob_df$pred <- ifelse(trk1_wprob_df$expval > min_expval_thres, TRUE, FALSE)
trk1_wprob_df2$pred <- ifelse(trk1_wprob_df2$expval > min_expval_thres, TRUE, FALSE)


## add wager amount
trk1_wprob_df <- add_betamt_col(trk1_wprob_df)
trk1_wprob_df2 <- add_betamt_col(trk1_wprob_df2)


## add earning column
trk1_wprob_df <- add_earning_col(trk1_wprob_df, pred_col='pred', 
                                 bet_amt_col='bet_amt', payout_col='bookmaker_ml')
trk1_wprob_df2 <- add_earning_col(trk1_wprob_df2, pred_col='pred', 
                                  bet_amt_col='bet_amt', payout_col='bookmaker_ml')


## quick view of wprob_dfs
head(trk1_wprob_df)
head(trk1_wprob_df2)
table(trk1_wprob_df$bet_amt, trk1_wprob_df2$bet_amt)


## calculate total bet amount and total earning by season
sum(trk1_wprob_df$bet_amt)
sum(trk1_wprob_df$earning)

sum(trk1_wprob_df2$bet_amt)
sum(trk1_wprob_df2$earning)

