## In this track, we will pick the best K metrics regardless of category. 
## Then we will employ voting methods to predict win.


## There are two ways picking the top performing predictor metrics.
##
## 1. Via the use of create_win_pred_acc_df(), 
## which produces overall prediction accuracy per predictor
## 
## 2. Via the use of get_pred_perf_rnk_plcment_lst(), 
## which produces a list of vectors whose index corresponds to a
## predictor's win prediction performance rank for each year
##
## Both method should render identical or similar result,
## returning the same predictors as highest-performing regardless
## of method we call.


## create wpc df using the entire collection of predictors 
## to evaludate and pick the best
wpa_df <- create_win_pred_acc_df(train, 
                                 metric_cols=predictors)
wpa_df


## create pred_perf_rnk_plcment_lst using the entire collection of predictors
## to evaludate and pick the best
a <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=predictors, 
                                  rank_method='pred_acc', min_n=5)
a
rev(sort(table(c(a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], a[[7]], a[[8]], a[[9]], a[[10]]))))


## pick the top predictors that aren't of the same category
trk2_predictors <- c('line', 'wpc_site', 'j5', 'oeff_cumperf_site', 'oeffA_cumperf_site')


## create win prob df
trk2_wprob_df <- create_wprob_df(train_complete, predictors=trk2_predictors)


## join with the original moneylines data
trk2_wprob_df <- left_join(moneylines, trk2_wprob_df, by=c('season', 'date', 'team', 'o_team'))


## filter for complete cases
trk2_wprob_df <- trk2_wprob_df[complete.cases(trk2_wprob_df), ]


## add expected value col (favorable to bet if EV > 0)
trk2_wprob_df$expval <- calc_exp_val(wgr_amt=100, 
                                     moneyline_odds=trk2_wprob_df$bookmaker_ml, 
                                     w_prob=trk2_wprob_df$wprob)


## place bet if EV is above certain threshold
min_expval_thres <- 20
trk2_wprob_df$pred <- ifelse(trk2_wprob_df$expval > min_expval_thres, TRUE, FALSE)


## add wager amount
trk2_wprob_df <- add_betamt_col(trk2_wprob_df)


## add earning column
trk2_wprob_df <- add_earning_col(trk2_wprob_df, pred_col='pred', 
                                 bet_amt_col='bet_amt', payout_col='bookmaker_ml')


## quick view of wprob_dfs
head(trk2_wprob_df, 50)


## calculate total bet amount and total earning by season
sum(trk2_wprob_df$bet_amt)
sum(trk2_wprob_df$earning)

