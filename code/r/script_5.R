
## create win pred accuracy df with wpc cols
quantile(master_df$wpc_gen - master_df$o_wpc_gen, na.rm=TRUE)
wpc_cols <- names(master_df)[grepl('^wpc_', names(master_df))]
# wpc_wpa_df <- create_win_pred_acc_df(master_df, 
#                                      metric_cols=wpc_cols, 
#                                      min_diff=c(0.1, 0.15, 0.2), 
#                                      min_n=c(5, 10))
wpc_wpa_df <- create_win_pred_acc_df(master_df,
                                     metric_cols=wpc_cols,
                                     min_diff=seq(0, 0.3, 0.05),
                                     min_n=c(5, 10))
plot_wpa(wpc_wpa_df)

## create win pred acc df with oeff cumperf cols
quantile(master_df$oeff_cumperf_gen - master_df$o_oeff_cumperf_gen, na.rm=TRUE)
oeff_cumperf_cols <- names(master_df)[grepl("^oeff_cumperf_", names(master_df), perl = TRUE)]
oeff_wpa_df <- create_win_pred_acc_df(master_df, 
                                      metric_cols=oeff_cumperf_cols, 
                                      min_diff=c(1.5, 3, 4.5), 
                                      min_n=c(5, 10))

## create win pred acc df with oeffA cumperf cols
quantile(master_df$oeffA_cumperf_gen - master_df$o_oeffA_cumperf_gen, na.rm=TRUE)
oeffA_cumperf_cols <- names(master_df)[grepl("^oeffA_cumperf_", names(master_df), perl = TRUE)]
oeffA_wpa_df <- create_win_pred_acc_df(master_df, 
                                       metric_cols=oeffA_cumperf_cols, 
                                       min_diff=c(1.5, 3, 4.5), 
                                       min_n=c(5, 10))

## create win pred acc df with FGP cumperf cols
quantile(master_df$FGP_cumperf_gen - master_df$o_FGP_cumperf_gen, na.rm=TRUE)
FGP_cumperf_cols <- names(master_df)[grepl("^FGP_cumperf_", names(master_df), perl = TRUE)]
FGP_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols=FGP_cumperf_cols, 
                                     min_diff=c(0.015, 0.02, 0.025), 
                                     min_n=c(5, 10))

## create win pred acc df with FGPA cumperf cols
quantile(master_df$FGPA_cumperf_gen - master_df$o_FGPA_cumperf_gen, na.rm=TRUE)
FGPA_cumperf_cols <- names(master_df)[grepl("^FGPA_cumperf_", names(master_df), perl = TRUE)]
FGPA_wpa_df <- create_win_pred_acc_df(master_df, 
                                      metric_cols=FGPA_cumperf_cols, 
                                      min_diff=c(0.015, 0.02, 0.025), 
                                      min_n=c(5, 10))

## create win pred acc df with rqP cumperf cols
quantile(master_df$rqP_cumperf_gen - master_df$o_rqP_cumperf_gen, na.rm=TRUE)
rqP_cumperf_cols <- names(master_df)[grepl("^rqP_cumperf_", names(master_df), perl = TRUE)]
rqP_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols=rqP_cumperf_cols, 
                                     min_diff=c(2.5, 5, 7.5), 
                                     min_n=c(5, 10))

## create win pred acc df with rqPA cumperf cols
quantile(master_df$rqPA_cumperf_gen - master_df$o_rqPA_cumperf_gen, na.rm=TRUE)
rqPA_cumperf_cols <- names(master_df)[grepl("^rqPA_cumperf_", names(master_df), perl = TRUE)]
rqPA_wpa_df <- create_win_pred_acc_df(master_df, 
                                      metric_cols=rqPA_cumperf_cols, 
                                      min_diff=c(2.5, 5, 7.5), 
                                      min_n=c(5, 10))

## create win pred acc df with pos cumperf cols
quantile(master_df$pos_cumperf_gen - master_df$o_pos_cumperf_gen, na.rm=TRUE)
pos_cumperf_cols <- names(master_df)[grepl("^pos_cumperf_", names(master_df), perl = TRUE)]
pos_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols=pos_cumperf_cols, 
                                     min_diff=c(1.5, 2, 2.5), 
                                     min_n=c(5, 10))

## create win pred acc df with posA cumperf cols
quantile(master_df$posA_cumperf_gen - master_df$o_posA_cumperf_gen, na.rm=TRUE)
posA_cumperf_cols <- names(master_df)[grepl("^posA_cumperf_", names(master_df), perl = TRUE)]
posA_wpa_df <- create_win_pred_acc_df(master_df, 
                                      metric_cols=posA_cumperf_cols, 
                                      min_diff=c(1.5, 2, 2.5), 
                                      min_n=c(5, 10))

## create win pred acc df with J cols
quantile(master_df$j10 - master_df$o_j10, na.rm=TRUE)
j_cols <- names(master_df)[grepl("^j", names(master_df), perl = TRUE)]
j_wpa_df <- create_win_pred_acc_df(master_df, 
                                   metric_cols=j_cols, 
                                   min_diff=c(20, 40, 60))

## create win pred acc df with mtchmrgn col
quantile(master_df$mtchmrgn, na.rm=TRUE)
mtchmrgn_wpa_df <- create_win_pred_acc_df(master_df, 
                                          metric_cols='mtchmrgn', 
                                          min_diff=c(1, 2, 3))

## create win pred acc df with line col
quantile(master_df$line, na.rm=TRUE)
line_wpa_df <- create_win_pred_acc_df(master_df, 
                                      metric_cols='line', 
                                      min_diff=c(2, 4, 6, 8, 10))

## create win pred acc df with rst cols
quantile(master_df$rst - master_df$o_rst, na.rm=TRUE)
rst_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols='rst', 
                                     min_diff=c(1, 2, 3))

## create win pred acc df with site
site_wpa_df <- create_win_pred_acc_df(master_df, metric_cols='site')



ggplot(wpc_wpa_df, aes(x=n_pred, y=acc, color=metric, group=metric)) + 
  geom_point() +
  geom_line() + 
  facet_grid(. ~ min_n)
plot_wpa(wpc_wpa_df)


# #### create prediction performance dfs of metric combinations
#
# ## create a list of metric combination
# metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
# smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(master_df, metric_cmb_lst)
# sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)



