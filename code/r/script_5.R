predictive_df <- create_predictive_df(master)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))



## 
a <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=wpc_cols, min_n=5)
table(a[[1]])
b <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=oeff_cumperf_cols, min_n=5)
table(b[[1]])
c <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=oeffA_cumperf_cols, min_n=5)
table(c[[1]])
d <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=FGP_cumperf_cols, min_n=5)
table(d[[1]])
e <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=FGPA_cumperf_cols, min_n=5)
table(e[[1]])
f <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=rqP_cumperf_cols, min_n=5)
table(f[[1]])
g <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=rqPA_cumperf_cols, min_n=5)
table(g[[1]])
# h <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=pos_cumperf_cols, min_n=5)
# table(h[[1]])
# i <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=posA_cumperf_cols, min_n=5)
# table(i[[2]])







