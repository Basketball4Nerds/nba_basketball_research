## get various cumperf predictor cols by metric type
oeff_cumperf_cols <- names(predictive_df)[grepl("^oeff_cumperf_", names(predictive_df), perl = TRUE)]
oeffA_cumperf_cols <- names(predictive_df)[grepl("^oeffA_cumperf_", names(predictive_df), perl = TRUE)]
FGP_cumperf_cols <- names(predictive_df)[grepl("^FGP_cumperf_", names(predictive_df), perl = TRUE)]
FGPA_cumperf_cols <- names(predictive_df)[grepl("^FGPA_cumperf_", names(predictive_df), perl = TRUE)]
rqP_cumperf_cols <- names(predictive_df)[grepl("^rqP_cumperf_", names(predictive_df), perl = TRUE)]
rqPA_cumperf_cols <- names(predictive_df)[grepl("^rqPA_cumperf_", names(predictive_df), perl = TRUE)]
pos_cumperf_cols <- names(predictive_df)[grepl("^pos_cumperf_", names(predictive_df), perl = TRUE)]
posA_cumperf_cols <- names(predictive_df)[grepl("^posA_cumperf_", names(predictive_df), perl = TRUE)]


# ## get various predictor cols by vary-by type
# gen_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)gen', names(predictive_df))]
# site_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)site', names(predictive_df))]
# cnf_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)cnf', names(predictive_df))]
# oeffQntlRnk_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)oeffQntlRnk', names(predictive_df))]
# oeffaQntlRnk_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)oeffaQntlRnk', names(predictive_df))]


## create win pred accuracy df with wpc cols
quantile(predictive_df$wpc_gen, na.rm=TRUE)
wpc_wpa_df <- create_win_pred_acc_df(predictive_df,
                                     metric_cols=wpc_cols,
                                     min_diff=c(0.1, 0.15, 0.2),
                                     min_n=c(5, 10))
plot_wpa(wpc_wpa_df)


## create win pred acc df with oeff cumperf cols
quantile(predictive_df$oeff_cumperf_gen, na.rm=TRUE)
oeff_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                      metric_cols=oeff_cumperf_cols, 
                                      min_diff=c(1.5, 3, 4.5), 
                                      min_n=c(5, 10))
plot_wpa(oeff_wpa_df)


## create win pred acc df with oeffA cumperf cols
quantile(predictive_df$oeffA_cumperf_gen, na.rm=TRUE)
oeffA_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                       metric_cols=oeffA_cumperf_cols, 
                                       min_diff=c(1.5, 3, 4.5), 
                                       min_n=c(5, 10))
plot_wpa(oeffA_wpa_df)


## create win pred acc df with FGP cumperf cols
quantile(predictive_df$FGP_cumperf_gen, na.rm=TRUE)
FGP_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                     metric_cols=FGP_cumperf_cols, 
                                     min_diff=c(0.015, 0.02, 0.025), 
                                     min_n=c(5, 10))
plot_wpa(FGP_wpa_df)


## create win pred acc df with FGPA cumperf cols
quantile(predictive_df$FGPA_cumperf_gen, na.rm=TRUE)
FGPA_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                      metric_cols=FGPA_cumperf_cols, 
                                      min_diff=c(0.015, 0.02, 0.025), 
                                      min_n=c(5, 10))
plot_wpa(FGPA_wpa_df)


## create win pred acc df with rqP cumperf cols
quantile(predictive_df$rqP_cumperf_gen, na.rm=TRUE)
rqP_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                     metric_cols=rqP_cumperf_cols, 
                                     min_diff=c(2.5, 5, 7.5), 
                                     min_n=c(5, 10))
plot_wpa(rqP_wpa_df)

## create win pred acc df with rqPA cumperf cols
quantile(predictive_df$rqPA_cumperf_gen, na.rm=TRUE)
rqPA_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                      metric_cols=rqPA_cumperf_cols, 
                                      min_diff=c(2.5, 5, 7.5), 
                                      min_n=c(5, 10))
plot_wpa(rqPA_wpa_df)


## create win pred acc df with pos cumperf cols
quantile(predictive_df$pos_cumperf_gen, na.rm=TRUE)
pos_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                     metric_cols=pos_cumperf_cols, 
                                     min_diff=c(1.5, 2, 2.5), 
                                     min_n=c(5, 10))
plot_wpa(pos_wpa_df)  
# this plot suggests that I exclude pos metric from possible set of predictors


## create win pred acc df with posA cumperf cols
quantile(predictive_df$posA_cumperf_gen, na.rm=TRUE)
posA_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                      metric_cols=posA_cumperf_cols, 
                                      min_diff=c(1.5, 2, 2.5), 
                                      min_n=c(5, 10))
plot_wpa(posA_wpa_df)  
# this plot suggests that I exclude pos metric from possible set of predictors


## create win pred acc df with J cols
quantile(predictive_df$j10, na.rm=TRUE)
j_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                   metric_cols=j_cols, 
                                   min_diff=c(20, 40, 60))
plot_wpa(j_wpa_df)


## create win pred acc df with mtchmrgn col
quantile(predictive_df$mtchmrgn, na.rm=TRUE)
mtchmrgn_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                          metric_cols='mtchmrgn', 
                                          min_diff=c(1, 2, 3))
plot_wpa(mtchmrgn_wpa_df)


## create win pred acc df with line col
quantile(predictive_df$line, na.rm=TRUE)
line_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                      metric_cols='line', 
                                      min_diff=c(2, 4, 6, 8, 10))
plot_wpa(line_wpa_df)


## create win pred acc df with rst cols
quantile(predictive_df$rst, na.rm=TRUE)
rst_wpa_df <- create_win_pred_acc_df(predictive_df, 
                                     metric_cols='rst', 
                                     min_diff=c(1, 2, 3))
plot_wpa(rst_wpa_df)


## create win pred acc df with site
site_wpa_df <- create_win_pred_acc_df(predictive_df, metric_cols='site')
site_wpa_df


## acc vs. number of predictions
# ggplot(wpc_wpa_df, aes(x=n_pred, y=acc, color=metric, group=metric)) + 
#   geom_point() +
#   geom_line() + 
#   facet_grid(. ~ min_n)


# #### create prediction performance dfs of metric combinations
#
# ## create a list of metric combination
# metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
# smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(predictive_df, metric_cmb_lst)
# sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)







