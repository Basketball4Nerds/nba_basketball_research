#### analysis of game pace distribution

range(master$pos)
quantile(master$pos)
tapply(master$pos, master$season, quantile)

mean(master$pos)
sd(master$pos)
tapply(master$pos, master$season, mean)




#### examine metrics' retrospective win prediction strength
cols <- c('p', 'p2x', 'p3x', 'pPnt', 'pFb', 
          'stl', 'ast', 'blk', 'dRb', 'oRb', 'rb', 'trnovrFcd', 'flFcd', 'pos',
          'FGA', 'FGA2x', 'FGA3x', 'FTA',
          'FGM', 'FGM2x', 'FGM3x', 'FTM', 
          'p2xShr', 'p3xShr', 'pFtShr', 'pPntShr', 'pFbShr',
          'FGP', 'FGP2x', 'FGP3x', 'FTP',
          'oRbShr', 'dRbShr', 'oRbPc', 'dRbPc', 'ODRR', 'ODRSR',
          'PPP', 'toPcFcd', 'FTA_p_FGA')
SRWPA_df <- createSimpleRetroWinPredAccDf(master, cols)
SRWPA_df$SRWPS <- round(SRWPA_df$SRWPS, 3)
SRWPA_df <- SRWPA_df[order(-SRWPA_df$SRWPS), ]
SRWPA_df




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















