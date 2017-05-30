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


metrics

### IMPLEMENT TIER SYSTEM!!!!
createWinPredByTier <- function(master_df, metric) {
  
}











#### examine win prediction accuracy of simple predictions and variable-specific metrics

## create variable-by list to examine performance of variable-specific
by_lst <- list('site', 
               'cnf', 
               'OG', 
               'DG', 
               c('site', 'cnf'), 
               c('site', 'OG'), 
               c('site', 'DG'))

## create prediction performance dfs of various metrics (both simple and variable-specific metrics)
smpl_pred_acc_df <- createWinPredAccDf(master, 
                                       metrics=c('site', 'line', 'mtch_mrgn', 'rst', 'j', 'wPc'), 
                                       n_min=c(5, 10))
varsp_pred_acc_df <- createVarSpWinPredAccDf(master, 
                                             metric='wPc', by_lst=by_lst,
                                             n_min=c(5, 10))

## sort by accuracy and view
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)
sortByCol(varsp_pred_acc_df, col='acc', asc=FALSE)

## prediction performance dfs with n-min game threshold of 5
smpl_pred_acc_df2 <- subset(smpl_pred_acc_df, n_min==5 | is.na(n_min))
varsp_pred_acc_df2 <- subset(varsp_pred_acc_df, n_min==5)
sortByCol(smpl_pred_acc_df2, col='acc', asc=FALSE)
sortByCol(varsp_pred_acc_df2, col='acc', asc=FALSE)


createWinPredAccDf()


#### create prediction performance dfs of metric combinations

## create a list of metric combination
metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(master_df, metric_cmb_lst)
sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)












## create variable-by list
by_lst <- list('site', 'cnf')
by_lst <- list('site', 'cnf', 'OG', 'DG')
by_lst <- list('site', 'cnf', 'OG', 'DG', 
               c('site', 'cnf'), c('site', 'OG'), c('site', 'DG'))


by_lst <- list('site', 'cnf')
#by_lst <- list('site', 'cnf', c('site', 'cnf'), 'OG', 'DG')
a <- createVarSpWinPredDf(master_df, metric='wPc', by_lst=by_lst, n_min=5)
w_pred_maj <- createPredByVote(a, maj_vote_cnt=2)
cnf_mtx <- table(master_df$won, w_pred_maj)
acc <- calcAccFrConfMtx(cnf_mtx)
acc

a <- createWinPredDf(master_df, metrics=c('line', 'wPc', 'j', 'site'), n_min=5)
b <- createVarSpWinPredDf(master_df, metric='wPc', by_lst=by_lst, n_min=5)
head(a)
head(b)

c <- cbind.data.frame(a, b)
c$w_pred_maj <- createPredByVote(c, maj_vote_cnt=9)
cnf_mtx <- table(master_df$won, c$w_pred_maj)
acc <- calcAccFrConfMtx(cnf_mtx)
acc
table(!is.na(c$w_pred_maj))




## WORK ON THE TIER SYSTEM; 
