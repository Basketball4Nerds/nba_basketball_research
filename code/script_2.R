
## subset
master_df <- subset(master, season==2012)

## get wpc cols




wpc_cols <- sort(names(master_df)[grepl('^wpc_', names(master_df))])
o_wpc_cols <- sort(names(master_df)[grepl('^o_wpc_', names(master_df))])

gm_cnt_cols <- names(master_df)[grepl('^n_', names(master_df))]
o_gm_cnt_cols <- names(master_df)[grepl('^o_n_', names(master_df))]

o_cumperf_cols <- names(master_df)[grepl('^o_.*cumperf_', names(master_df))]
cumperf_cols <- gsub('^o_', '', o_cumperf_cols)



pred_win_by_wpc <- function(master_df) {
  
  
  
  for (i in 1:length(wpc_cols)) {
    wpc_col <- wpc_cols[i]
    o_wpc_col <- o_wpc_cols[i]
    gm_cnt_col <- gm_cnt_cols[i]
    
    pred <- pred_win_higher_val(master_df[[wpc_col]], master_df[[o_wpc_col]])
    
    cnf_mtx <- table(master_df$won, pred)
    n_pred <- sum(cnf_mtx)
    acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
  
  }
  
}


#### examine win prediction accuracy with simple metrics comparison
#### (of site, line, match margin, and rest)
smpl_params_df <- expand.grid(metric=c('site', 'line', 'mtch_mrgn', 'rst'), by=NA, n_min=0, min_diff=NA)
smpl_pred_acc_df <- createWinPredAccDf(master_df, smpl_params_df, rm.irr.cols=TRUE)
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)




#### examine win prediction accuracy with simple metrics comparison (of j and wPc)
#### with differing minimum n-game thresholds
smpl_params_df2 <- expand.grid(metric=c('j', 'wPc'), by=NA, n_min=c(5, 10, 20, 30), min_diff=NA)
smpl_pred_acc_df2 <- createWinPredAccDf(master_df, smpl_params_df2, rm.irr.cols=TRUE)
smpl_pred_acc_df2
sortByCol(smpl_pred_acc_df2, col='acc', asc=FALSE)




#### examine win prediction accuracy with variable-specific metrics comparison (of wPc)
varsp_params_df <- expand.grid(metric = 'wPc', 
                               by = list('site', 
                                         'cnf', 
                                         'OG', 
                                         'DG', 
                                         c('site', 'cnf')), 
                               n_min = c(5, 10), 
                               min_diff = NA)
varsp_params_df



varsp_pred_acc_df <- createWinPredAccDf(master_df, varsp_params_df, rm.irr.cols=TRUE)
sortByCol(varsp_pred_acc_df, col='acc', asc=FALSE)




#### prediction accuracy by tiered metrics

## by tiered line
range(abs(master$line))
trd_line_params_df <- expand.grid(metric='line', by=NA, n_min=0, min_diff=seq(0, 22, 1))
trd_line_pred_acc_df <- createWinPredAccDf(master_df, trd_line_params_df, rm.irr.cols=TRUE)
sortByCol(trd_line_pred_acc_df, col='acc', asc=FALSE)


## by tiered match margins
table(abs(master$mtch_mrgn))
mtchmrgn_params_df <- expand.grid(metric='mtch_mrgn', by=NA, n_min=0, min_diff=seq(1, 7, by=1))
trd_mtchmrgn_pred_acc_df <- createWinPredAccDf(master_df, mtchmrgn_params_df, rm.irr.cols=TRUE)
sortByCol(trd_mtchmrgn_pred_acc_df, col='acc', asc=FALSE)


## by tiered rest margin
table(abs(master$rst - master$o_rst))
trd_rst_params_df <- expand.grid(metric='rst', by=NA, n_min=0, min_diff=seq(1, 9, by=1))
trd_rst_pred_acc_df <- createWinPredAccDf(master_df, trd_rst_params_df, rm.irr.cols=TRUE)
sortByCol(trd_rst_pred_acc_df, col='acc', asc=FALSE)


## by tiered J
range(abs(master$j - master$o_j))
trd_j_params_df <- expand.grid(metric='j', by=NA, n_min=5, 
                         min_diff=c(25, 50, 75, 100, 125, 150, 200, 250, 300, 350, 400))
trd_j_pred_acc_df <- createWinPredAccDf(master_df, trd_j_params_df, rm.irr.cols=TRUE)
trd_j_pred_acc_df


## by tiered simple win percentage
range(abs(master$wPc - master$o_wPc), na.rm=TRUE)
trd_smpl_wPc_params_df <- expand.grid(metric='wPc', by=NA, n_min=5, 
                         min_diff=seq(0.05, 0.5, 0.05))
trd_smpl_wPc_pred_acc_df <- createWinPredAccDf(master_df, trd_smpl_wPc_params_df, rm.irr.cols=TRUE)
trd_smpl_wPc_pred_acc_df


## by tiered variable-specific win percentagea
trd_varsp_wPc_params_df <- expand.grid(metric='wPc', by=c('site', 'cnf', 'OG', 'DG'), n_min=5, 
                         min_diff=seq(0.05, 0.5, 0.05))
trd_varsp_wPc_pred_acc_df <- createWinPredAccDf(master_df, trd_varsp_wPc_params_df, rm.irr.cols=TRUE)
trd_varsp_wPc_pred_acc_df




####











#### create prediction performance dfs of metric combinations

## create a list of metric combination
metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(master_df, metric_cmb_lst)
sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)















