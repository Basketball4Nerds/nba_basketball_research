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



#### examine win prediction accuracy of simple metric comparison predictions

## create a list of parameter lists
params_df <- expand.grid(metric=c('j', 'wPc'), n_min=c(5, 10))
params_df <- rbind.data.frame(params_df, 
                              expand.grid(metric=c('site', 'line', 'mtch_mrgn', 'rst'), n_min=0),
                              stringsAsFactors=FALSE)
params_lst <- apply(params_df, 1, as.list)
params_lst <- lapply(params_lst, function(x) x$n_min <- as.integer(x$n_min))
#params_lst <- lapply(params_lst, function(x) x$min_diff <- as.integer(x$min_diff))

## create prediction performance df of simple comparison metrics
smpl_pred_acc_df <- createWinPredAccDf(master_df, params_lst)

## sort by accuracy
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)

## prediction performance df with n-min game threshold of 5
smpl_pred_acc_df2 <- subset(smpl_pred_acc_df, n_min==5 | is.na(n_min))
sortByCol(smpl_pred_acc_df2, col='acc', asc=FALSE)



#### examine win prediction accuracy of simple metric comparison predictions






#### examine win prediction accuracy of variable-specific metrics

## create variable-by list to examine performance of variable-specific
by_lst <- list('site', 
               'cnf', 
               'OG', 
               'DG', 
               c('site', 'cnf'), 
               c('site', 'OG'), 
               c('site', 'DG'))

## create prediction performance df of variable-specific metrics
varsp_pred_acc_df <- createVarSpWinPredAccDf(master, 
                                             metric='wPc', by_lst=by_lst,
                                             n_min=c(5, 10))

## sort by accuracy
sortByCol(varsp_pred_acc_df, col='acc', asc=FALSE)

## prediction performance dfs with n-min game threshold of 5
varsp_pred_acc_df2 <- subset(varsp_pred_acc_df, n_min==5)
sortByCol(varsp_pred_acc_df2, col='acc', asc=FALSE)




#### create prediction performance dfs of metric combinations

## create a list of metric combination
metric_cmb_lst <- createMetricCmbLst(metrics=c('line', 'wPc', 'j', 'site', 'mtch_mrgn', 'rst'))
smplcmb_pred_acc_df <- createWinPredAccDfByMetCmb(master_df, metric_cmb_lst)
sortByCol(smplcmb_pred_acc_df, col='acc', asc=FALSE)












# ## create variable-by list
# by_lst <- list('site', 'cnf')
# by_lst <- list('site', 'cnf', 'OG', 'DG')
# by_lst <- list('site', 'cnf', 'OG', 'DG', 
#                c('site', 'cnf'), c('site', 'OG'), c('site', 'DG'))
# 
# 
# by_lst <- list('site', 'cnf')
# #by_lst <- list('site', 'cnf', c('site', 'cnf'), 'OG', 'DG')
# a <- createVarSpWinPredDf(master_df, metric='wPc', by_lst=by_lst, n_min=5)
# w_pred_maj <- createPredByVote(a, maj_vote_cnt=2)
# cnf_mtx <- table(master_df$won, w_pred_maj)
# acc <- calcAccFrConfMtx(cnf_mtx)
# acc
# 
# a <- createWinPredDf(master_df, metrics=c('line', 'wPc', 'j', 'site'), n_min=5)
# b <- createVarSpWinPredDf(master_df, metric='wPc', by_lst=by_lst, n_min=5)
# head(a)
# head(b)
# 
# c <- cbind.data.frame(a, b)
# c$w_pred_maj <- createPredByVote(c, maj_vote_cnt=9)
# cnf_mtx <- table(master_df$won, c$w_pred_maj)
# acc <- calcAccFrConfMtx(cnf_mtx)
# acc
# table(!is.na(c$w_pred_maj))





#### create a prediction 
createWinPredByTier <- function(master_df, metric) {
  
}



seq(0, 22, 0.5)

a <- createWinPred(master_df, metric='site')
b <- createWinPred(master_df, metric='line', min_diff=8)
c <- createWinPred(master_df, metric='mtch_mrgn', min_diff=2)
d <- createWinPred(master_df, metric='j', min_diff=150)
e <- createWinPred(master_df, metric='rst', min_diff=5)
f <- createWinPred(master_df, metric='wPc', min_diff=0.25)
g <- createWinPred(master_df, metric='wPc', by=c('site', 'cnf'), min_diff=0.15)


