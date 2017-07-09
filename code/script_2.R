
## subset
master_df <- subset(master, season==2012)

## create win pred accuracy df with wpc cols
quantile(master_df$wpc_gen - master_df$o_wpc_gen, na.rm=TRUE)
wpc_cols <- names(master_df)[grepl('^wpc_', names(master_df))]
wpc_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols=wpc_cols, 
                                     min_diff=c(0.1, 0.15, 0.2), 
                                     min_n=c(5, 10))

wpc_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols=wpc_cols, 
                                     min_diff=seq(0, 0.3, 0.05), 
                                     min_n=c(5, 10))

ggplot(wpc_wpa_df, aes(x=n_pred, y=acc, color=metric, group=metric)) + 
  geom_point() +
  geom_line() + 
  facet_grid(. ~ min_n)


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

## create win pred acc df with rst cols
quantile(master_df$rst - master_df$o_rst, na.rm=TRUE)
rst_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols='rst', 
                                     min_diff=c(1, 2, 3))






## create win pred acc df with line col
smpl_params_df <- expand.grid(metric=c('site', 'line', 'mtch_mrgn', 'rst'), by=NA, n_min=0, min_diff=NA)
smpl_pred_acc_df <- createWinPredAccDf(master_df, smpl_params_df, rm.irr.cols=TRUE)
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)

#### examine win prediction accuracy with simple metrics comparison (of j and wPc)
#### with differing minimum n-game thresholds
smpl_params_df2 <- expand.grid(metric=c('j', 'wPc'), by=NA, n_min=c(5, 10, 20, 30), min_diff=NA)
smpl_pred_acc_df2 <- createWinPredAccDf(master_df, smpl_params_df2, rm.irr.cols=TRUE)
smpl_pred_acc_df2
sortByCol(smpl_pred_acc_df2, col='acc', asc=FALSE)



# #### prediction accuracy by tiered metrics
#
# ## by tiered line
# range(abs(master$line))
# trd_line_params_df <- expand.grid(metric='line', by=NA, n_min=0, min_diff=seq(0, 22, 1))
# trd_line_pred_acc_df <- createWinPredAccDf(master_df, trd_line_params_df, rm.irr.cols=TRUE)
# sortByCol(trd_line_pred_acc_df, col='acc', asc=FALSE)
#
#
# ## by tiered match margins
# table(abs(master$mtch_mrgn))
# mtchmrgn_params_df <- expand.grid(metric='mtch_mrgn', by=NA, n_min=0, min_diff=seq(1, 7, by=1))
# trd_mtchmrgn_pred_acc_df <- createWinPredAccDf(master_df, mtchmrgn_params_df, rm.irr.cols=TRUE)
# sortByCol(trd_mtchmrgn_pred_acc_df, col='acc', asc=FALSE)
#
#
# ## by tiered rest margin
# table(abs(master$rst - master$o_rst))
# trd_rst_params_df <- expand.grid(metric='rst', by=NA, n_min=0, min_diff=seq(1, 9, by=1))
# trd_rst_pred_acc_df <- createWinPredAccDf(master_df, trd_rst_params_df, rm.irr.cols=TRUE)
# sortByCol(trd_rst_pred_acc_df, col='acc', asc=FALSE)
#
#
# ## by tiered J
# range(abs(master$j - master$o_j))
# trd_j_params_df <- expand.grid(metric='j', by=NA, n_min=5,
#                          min_diff=c(25, 50, 75, 100, 125, 150, 200, 250, 300, 350, 400))
# trd_j_pred_acc_df <- createWinPredAccDf(master_df, trd_j_params_df, rm.irr.cols=TRUE)
# trd_j_pred_acc_df
#
#


seq(0.05, 0.5, 0.05)
# ## by tiered simple win percentage
# range(abs(master$wPc - master$o_wPc), na.rm=TRUE)
# trd_smpl_wPc_params_df <- expand.grid(metric='wPc', by=NA, n_min=5,
#                          min_diff=seq(0.05, 0.5, 0.05))
# trd_smpl_wPc_pred_acc_df <- createWinPredAccDf(master_df, trd_smpl_wPc_params_df, rm.irr.cols=TRUE)
# trd_smpl_wPc_pred_acc_df
#
#
# ## by tiered variable-specific win percentagea
# trd_varsp_wPc_params_df <- expand.grid(metric='wPc', by=c('site', 'cnf', 'OG', 'DG'), n_min=5,
#                          min_diff=seq(0.05, 0.5, 0.05))
# trd_varsp_wPc_pred_acc_df <- createWinPredAccDf(master_df, trd_varsp_wPc_params_df, rm.irr.cols=TRUE)
# trd_varsp_wPc_pred_acc_df
#
#
#
# #### create prediction performance dfs of metric combinations
#
# ## create a list of metric combination
# metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
# smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(master_df, metric_cmb_lst)
# sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)







