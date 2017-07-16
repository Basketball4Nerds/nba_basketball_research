## this function creates df of simple retrospective win prediction strengths (RWPS)
## by using raw performance metrics
create_smpl_retro_win_pred_acc_df <- function(master_df, raw_perf_cols=NULL) {
  
  ## if cols not set, assign values to cols
  if (is.null(raw_perf_cols)) {
    raw_perf_cols <- c('p', 'p2x', 'p3x', 'pPnt', 'pFb', 
                       'stl', 'ast', 'blk', 'dRb', 'oRb', 'rb', 'trnovrFcd', 'flFcd', 'pos',
                       'FGA', 'FGA2x', 'FGA3x', 'FTA',
                       'FGM', 'FGM2x', 'FGM3x', 'FTM', 
                       'p2xShr', 'p3xShr', 'pFtShr', 'pPntShr', 'pFbShr',
                       'FGP', 'FGP2x', 'FGP3x', 'FTP',
                       'oRbShr', 'dRbShr', 'oRbPc', 'dRbPc', 'ODRR', 'ODRSR',
                       'oeff', 'toPcFcd', 'FTA_p_FGA')
  }
  
  ## initialize empty vectors to store values
  col_vec <- acc_vec <- n_pred_vec <- c()
  
  ## calculate retrospective win prediction strength (RWPS) for each raw performance metric
  for (col in raw_perf_cols) {
    
    ## construct o_col (opponent's metric) based on a given col
    if (grepl('Fcd', col)) {
      o_col <- gsub('Fcd', '', col)
    }
    else if (grepl('[A-Za-z]_p_[A-Za-z]', col)) {
      o_col <- unlist(strsplit(col, split='_p_'))
      o_col <- paste0(o_col, 'A')
      o_col <- paste0(o_col, collapse='_p_')
    }
    else {
      o_col <- paste0(col, 'A')
    }
    
    ## if o_col is not found in dataset, skip to the next metric
    if (!(o_col %in% names(master_df))) {
      print(paste('Unable to locate the following metric:', col))
      next
    }
    
    ## make a simple retrospective prediction
    pred <- ifelse(master_df[ , col] > master_df[ , o_col], TRUE, 
                   ifelse(master_df[ , col] < master_df[ , o_col], FALSE, NA))
    
    ## create confusion matrix
    cnf_mtx <- table(master_df$won, pred)
    
    ## calculate prediction accuracy
    acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
    
    ## calculate the number of data points to calculate retro pred acc
    n_pred <- sum(cnf_mtx)
    
    ## add vals to vectors
    col_vec <- c(col_vec, col)
    acc_vec <- c(acc_vec, acc)
    n_pred_vec <- c(n_pred_vec, n_pred)
  }
  
  ## create return df
  ret_df <- cbind.data.frame(metric = col_vec, 
                             SRWPS = acc_vec, 
                             n_pred = n_pred_vec)
  
  ## sort return df
  ret_df <- sortByCol(ret_df, col='SRWPS', asc=FALSE)
  
  ## return
  return(ret_df)
}


## this function takes in win pred acc df and plots  min_diff vs. acc 
plot_wpa <- function(wpa_df) {
  ggplot(wpa_df, aes(x=min_diff, y=acc, color=metric)) + 
    geom_point(aes(size=n_pred)) +
    geom_line(aes(group=metric)) + 
    facet_grid(. ~ min_n)
}



## this function 




## this function creates all possible combinations (nCr) of given metrics
## and returns as a list
create_metric_combo_lst <- function(metrics, n=NULL) {
  
  ## initialize empty metric combination list
  metric_cmb_lst <- list()

  ## set n if not specified
  if (is.null(n)) 
    n <- 2:length(metrics)
  
  ## create all possible combinations of metrics and add to list  
  for (i in n) {
    metric_cmb_lst <- c(metric_cmb_lst, lapply(apply(combn(metrics, i), 2, as.list), unlist))    
  }
  
  ## return
  return(metric_cmb_lst)
}



## 







