############ FUNCTIONS TO PREDICT AND CALCULATE ACCURACY PERCENTAGES ################


## this function returns df of win prediction accuracies
## when predicting wins by various combinations of win metrics
createWinPredAccDfByMetCmb <- function(master_df, metric_cmb_lst) {
  
  ## create empty vector to store values
  label_vec <- c()
  acc_vec <- c()
  n_pred_vec <- c()
  
  ## for each metric combination, get prediction performance
  for (metric_cmb in metric_cmb_lst) {
    
    ## create prediction df using combination of metrics
    pred_df <- createWinPredDf(master_df, metrics=metric_cmb, n=5)
    
    ## for both all-agree and all-but-one-agree majority vote methods
    for (maj_vote_cnt in c(length(metric_cmb)-1, length(metric_cmb))) {
      
      ## create label
      label <- paste0(c(metric_cmb, paste0('vtmaj', maj_vote_cnt)), collapse='-')
      
      ## make prediction using majority vote method
      w_pred_maj <- createPredByVote(pred_df, maj_vote_cnt=maj_vote_cnt)
      
      ## create confusion matrices
      cnf_mtx <- table(master_df$won, w_pred_maj)
      
      ## add results to vector
      label_vec <- c(label_vec, label)
      acc_vec <- c(acc_vec, calc_acc_fr_cnf_mtx(cnf_mtx))
      n_pred_vec <- c(n_pred_vec, sum(cnf_mtx))
    }
  }
  
  ## construct performance df
  perf_df <- cbind.data.frame(label_vec, acc_vec, n_pred_vec)
  names(perf_df) <- gsub('_vec', '', names(perf_df))
  
  ## return
  return(perf_df)
}




