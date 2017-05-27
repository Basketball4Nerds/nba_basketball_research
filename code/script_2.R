
df <- subset(master, season==2000)



acc_df
createWinPredAccDfByLine(df)

# acc_df <- createWinPredAccDfByWinPcMetrics(master)
# x <- createWinPredAccByWinPcMetrics(master)







## predict that team with higher conf-sp win percentage will win
# scenarios:
# E vs. E
# E vs. W
# W vs. E
# W vs. W
pred <- ifelse(rs$cnf=='E' & rs$o_cnf=='E' & (rs$wPcVsE > rs$o_wPcVsE), TRUE,
               ifelse(rs$cnf=='E' & rs$o_cnf=='W' & (rs$wPcVsW > rs$o_wPcVsE), TRUE, 
                      ifelse(rs$cnf=='W' & rs$o_cnf=='E' & (rs$wPcVsE > rs$o_wPcVsW), TRUE, 
                             ifelse(rs$cnf=='W' & rs$o_cnf=='W' & (rs$wPcVsW > rs$o_wPcVsW), TRUE, FALSE))))
x <- table(pred, rs$won)
calcAccFrConfMtx(x)





















## create variable-by list
by_lst <- list('site', 
               'cnf', 
               'OG', 
               'DG', 
               c('site', 'cnf'), 
               c('site', 'OG'), 
               c('site', 'DG'))

## create variable-by df list
var_df_lst <- lapply(by_lst, function(x) {
  createVarDf(by=x, type='tm-opp')
})


## for each threshold in n-game min threshold vector
for (i in seq_along(n_min)) {
  
  ## get threshold
  thres <- n_min[i]
  
  ## for each by specification
  for (j in seq_along(by_lst)) {
    by <- by_lst[[j]]
    var_df <- var_df_lst[[j]]
    
    ## create prediction
    pred <- createWinPred(master_df=master_df, var_df=var_df, by=by, n_min=thres)
    
    ## create confusion matrix
    cnfMtx <- table(pred, master_df$won)
    #cnfMtx <- table(c(T, T), c(T, F))
    
    ## calculate prediction accuracy
    acc <- calcAccFrConfMtx(cnfMtx, rndDgt=3)
    
    ## add results to the ret_df
    k <- (i-1) * length(by_lst) + j
    ret_df$by[k] <- paste0(by, collapse='_')
    ret_df$n_min[k] <- thres
    ret_df$acc[k] <- acc
    ret_df$n_pred[k] <- sum(cnfMtx)
  } 
}

