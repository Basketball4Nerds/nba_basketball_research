
## subset
master_df <- subset(master, season==2012)






## grab all cumperf cols
o_cumperf_cols <- sort(names(master_df)[grepl('^o_.*cumperf_', names(master_df))])
cumperf_cols <- gsub('^o_', '', o_cumperf_cols)



x <- create_wpc_win_pred_acc_df(master_df, min_diff=c(0.1, 0.15, 0.2), min_n=c(5, 10))










