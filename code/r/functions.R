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



## this function moves all files found in a directory to another
move_files_to_another_dir <- function(from_dir, to_dir) {
  
  ## get list of names of files to move
  file_nms <- list.files(from_dir)
  file_nms <- file_nms[grepl('\\.', file_nms)]
  
  ## move each file to a different directory
  for (file_nm in file_nms) {
    
    ## create origin file path
    orig_file_path <- file.path(from_dir, file_nm)
    
    ## create destination file path
    dest_file_path <- file.path(to_dir, file_nm)
    
    ## move file
    file.rename(from=orig_file_path, to=dest_file_path)
  }
}



## this function takes master_df as input and creates "predictive df" 
## that contains predictor variables and is used to make predictions 
create_predictive_df <- function(master_df) {
  
  ## define prediction column (dependent variable)
  dep_col <- 'won'
  
  ## set base columns
  base_cols <- c('gid', 'season', 'date', 'site', 'playoffs', 'team', 'o_team')
  
  ## get general and variable-specific count cols
  cnt_cols <- names(master_df)[grepl('^n_', names(master_df))]
  o_cnt_cols <- names(master_df)[grepl('^o_n_', names(master_df))]
  
  ## get J, wpc, and cumperf cols for team
  j_cols <- names(master_df)[grepl('^j[0-9]+$', names(master_df))]
  wpc_cols <- names(master_df)[grepl('^wpc_', names(master_df))]
  cumperf_cols <- names(master_df)[grepl("^(?!o_).*cumperf_", names(master_df), perl = TRUE)]
  
  ## get J, wpc, and cumperf cols for opponent
  o_j_cols <- names(master_df)[grepl('^o_j[0-9]+$', names(master_df))]
  o_wpc_cols <- names(master_df)[grepl('^o_wpc_', names(master_df))]
  o_cumperf_cols <- names(master_df)[grepl("^o_.*cumperf_", names(master_df), perl = TRUE)]
  
  ## save cols to compare into variables
  tm_cols <- c(j_cols, wpc_cols, cumperf_cols, 'rst')
  o_cols <- c(o_j_cols, o_wpc_cols, o_cumperf_cols, 'o_rst')
  
  ## initialize return df
  ret_df <- master_df[ , c(base_cols, dep_col, 'line')]
  
  ## for each pair of team and opponent metric  
  for (i in 1:length(tm_cols)) {
    tm_col <- tm_cols[i]
    o_col <- o_cols[i]
    
    ## calculate the diff and add to ret_df
    ret_df[ , tm_col] <- master_df[ , tm_col] - master_df[ , o_col]
  }
  
  ## convert site info as predictor variable
  ret_df$home <- ifelse(master_df$site=='H', 1, 0)
  
  ## store match margin info as predictor variable
  ret_df$mtchmrgn <- master_df$mtch_w - master_df$mtch_l
  
  ## return
  return(ret_df)
}


