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

head(trk1_wprob_df)
head(trk1_wprob_df2)


## calculate earning 