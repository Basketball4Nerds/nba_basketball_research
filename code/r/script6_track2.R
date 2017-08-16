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

## 
head(trk2_wprob_df)


