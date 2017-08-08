## In this code track, we will throw in all predictor variables into the 
## high-VIF removal function (without handling any variables first)
## and remove collinear variables. We will then use cross validation to compare
## different model peroformances and pick the best one.


## remove highly correlated variables via VIF calc
trk2_predictor_vars <- vif_func(in_frame=train_complete[ , predictor_vars], 
                                  thresh=5, trace=TRUE)



## examine predictive power of the selected vars
a_trk2 <- get_pred_perf_rnk_plcmnt_lst(train_complete, 
                                  predictor_vars=trk2_predictor_vars, 
                                  rank_method='pred_acc', min_n=5)
lapply(a_trk2, table)


## picking top-performing predictors from top 5 rank populations
rev(sort(table(c(a_trk2[[1]], a_trk2[[2]], a_trk2[[3]], a_trk2[[4]], a_trk2[[5]]))))


## hand-picking the top performing variables (see above)
trk2_predictor_vars_2 <- c('wpc_cnf', 'j15', 'FGPA_cumperf_site', 
                           'FGP_cumperf_site', 'FGPA_cumperf_oeffQntlRnk')





