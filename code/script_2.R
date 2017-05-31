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



#### examine win prediction accuracy of simple metric comparison predictions (of j and wPc)
#### with differing minimum n-game thresholds

## create a params_df
params_df <- expand.grid(metric=c('j', 'wPc'), by=NA, n_min=c(5, 10, 20), min_diff=NA)

## create prediction performance df of simple comparison metrics
smpl_pred_acc_df <- createWinPredAccDf(master_df, params_df, rm.na.cols=TRUE)

## sort by accuracy
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)



#### examine win prediction accuracy of simple metric comparison predictions 
#### (of site, line, match margin, and rest)

## create a params_df
params_df <- expand.grid(metric=c('site', 'line', 'mtch_mrgn', 'rst'),
                         by=NA, n_min=0, min_diff=NA)

## create prediction performance df of simple comparison metrics
smpl_pred_acc_df <- createWinPredAccDf(master_df, params_df, rm.na.cols=TRUE)

## sort by accuracy
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)



#### examine win prediction accuracy of variable-specific metrics (of wPc)

## create a list of parameter lists
params_df <- expand.grid(metric = 'wPc', 
                         by = list('site', 
                                   'cnf', 
                                   'OG', 
                                   'DG', 
                                   c('site', 'cnf'), 
                                   c('site', 'OG'), 
                                   c('site', 'DG')), 
                         n_min = c(5, 10), 
                         min_diff = NA)

## create prediction performance df of variable-specific metrics
varsp_pred_acc_df <- createWinPredAccDf(master_df, params_df, rm.na.cols=TRUE)

## sort by accuracy
sortByCol(varsp_pred_acc_df, col='acc', asc=FALSE)

## prediction performance dfs with n-min game threshold of 5
varsp_pred_acc_df2 <- subset(varsp_pred_acc_df, n_min==5)
sortByCol(varsp_pred_acc_df2, col='acc', asc=FALSE)



#### BY TIER
a <- createWinPred(master_df, metric='site')
b <- createWinPred(master_df, metric='line', min_diff=8)
c <- createWinPred(master_df, metric='mtch_mrgn', min_diff=2)
d <- createWinPred(master_df, metric='j', min_diff=150)
e <- createWinPred(master_df, metric='rst', min_diff=5)
f <- createWinPred(master_df, metric='wPc', min_diff=0.25)
g <- createWinPred(master_df, metric='wPc', by=c('site', 'cnf'), min_diff=0.15)













#### create prediction performance dfs of metric combinations

## create a list of metric combination
metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(master_df, metric_cmb_lst)
sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)















