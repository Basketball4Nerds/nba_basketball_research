
df <- subset(master, season==2000)

smpl_pred_acc_df <- createWinPredAccDf(master_df, n_min=c(5, 10))
varsp_pred_acc_df <- createVarSpWinPredAccDf(master_df, n_min=c(5, 10))
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)
sortByCol(varsp_pred_acc_df, col='acc', asc=FALSE)

smpl_pred_acc_df <- createWinPredAccDf(master_df, n_min=5)
varsp_pred_acc_df <- createVarSpWinPredAccDf(master_df, n_min=5)
sortByCol(smpl_pred_acc_df, col='acc', asc=FALSE)
sortByCol(varsp_pred_acc_df, col='acc', asc=FALSE)



## create variable-by list
by_lst <- list('site', 
               'cnf', 
               'OG', 
               'DG', 
               c('site', 'cnf'), 
               c('site', 'OG'), 
               c('site', 'DG'))
dim(master_df)




a <- createWinPredDf(master_df, metrics=c('wPc', 'j', 'site'))
a <- cbind.data.frame(a, master_df$won)

head(a, 50)
x <- retByMajorityVote(a)
dim(a)

