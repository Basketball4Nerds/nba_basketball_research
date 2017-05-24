
df <- subset(master, season==2000)





## this is a highly redundant column but provides programmatic convenience 
## for the following task; delete later
df$o_site <- ifelse(df$site=='H', 'A', 'H')

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
  createvar_df(by=x, type='tm-opp')
})




by <- by_lst[[5]]
var_df <- var_df_lst[[5]]
by
var_df


head(x)
nMin <- 5

## initialize predictions to a vector of NAs
pred <- rep(NA, nrow(df))

## for each team instance of a game
for (i in 1:nrow(df)) {

  ## create column selectors
  tm_ftrs_cols <- by
  o_ftrs_cols <- paste0('o_', by)

  ## extract specific features of the game for team and opponent
  ## as.matrix() needed to correctly convert to character
  tm_ftrs <- as.character(as.matrix(df[i, tm_ftrs_cols]))
  o_tm_ftrs <- as.character(as.matrix(df[i, o_ftrs_cols]))

  ## create index to extract proper column labels from var_df
  ind_cond_lst <- list()
  for (j in seq_along(tm_ftrs_cols)) {
    ind_cond_lst[[j]] <- var_df[[tm_ftrs_cols[j]]]==tm_ftrs[j] & var_df[[o_ftrs_cols[j]]]==o_tm_ftrs[j]
  }
  ind_cond_mtx <- as.matrix(do.call(rbind.data.frame, ind_cond_lst))
  ind <- as.logical(apply(ind_cond_mtx, 2, all))
  ## ind <- var_df[[tm_ftrs_cols]]==tm_ftrs & var_df[[o_ftrs_cols]]==o_tm_ftrs

  ## proceed only if column name retrieval is successful
  ## (i.e. skip when a team belongs to OG/DG of "U" the unknown)
  if (any(ind)) {
    
    ## get relevant win percentages to compare
    wPc <- df[i, var_df$wPcCols[ind]]
    o_wPc <- df[i, var_df$o_wPcCols[ind]]
    
    ## get relevant n-game counts  
    nGm <- df[i, var_df$nCols[ind]]
    o_nGm <- df[i, var_df$o_nCols[ind]]
    
    ## if n-game does not meet minimum n-game threshold,
    ## then don't make any prediction and skip
    if ((nGm < nMin) | (o_nGm < nMin)) next

    ## assign win prediction based on feature-specific win percentage
    pred[i] <- ifelse(wPc > o_wPc, TRUE, 
                      ifelse(wPc < o_wPc, FALSE, NA))
  }
}

cnfMtx <- table(pred, df$won)
calcAccFrConfMtx(cnfMtx, rndDgt=3)






getPredAccs <- function(masterDf, minN) {

}


## this function returns df of win prediction accuracies
## when predicting wins by various win percentage metrics
createWinPcMetricsWinPredAccDf <- function(df) {

}


## create confusion matrix
cnfMtx <- table(df$won, pred)

## calculate prediction accuracy
acc <- calcAccFrConfMtx(cnfMtx)
acc

