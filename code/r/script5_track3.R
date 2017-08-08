## In this code track, we will pick the top K best performing predictor variables
## that have demonstrated the highest predictive power over the years. We will then
## apply the VIF-filter method remove collinear variables. We will then employ a voting 
## method to make predictions.


## examine predictive power of the selected vars
a_trk2 <- get_pred_perf_rnk_plcmnt_lst(train_complete, 
                                       predictor_vars=trk2_predictor_vars, 
                                       rank_method='pred_acc', min_n=5)
lapply(a_trk2, table)



## remove highly correlated variables via VIF calc
trk2_predictor_vars <- vif_func(in_frame=train_complete[ , predictor_vars], 
                                thresh=5, trace=TRUE)





