
df <- subset(master, season==2000)



# x <- createWinPredAccByWinPcMetrics(master)


## create variable-by list
by_lst <- list('site', 
               'cnf', 
               'OG', 
               'DG', 
               c('site', 'cnf'), 
               c('site', 'OG'), 
               c('site', 'DG'))

## create variable-by df list
var_df_lst <- lapply(by_lst, createVarDf)



