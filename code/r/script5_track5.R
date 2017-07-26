## in this code track, throw in all predictor variables and rank the first N.

## to pick the best win perc metric
a <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=predictor_vars, 
                                  rank_method='pred_acc', min_n=5)
lapply(a, table)
