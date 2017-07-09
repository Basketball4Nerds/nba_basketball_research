## this function predicts win if team's metric is higher than that of opponent
pred_win_higher_val <- function(tm_vals, o_vals, min_diff=NULL) {
  
  ## make prediction
  if (is.null(min_diff) || min_diff==0) {
    pred <- ifelse(tm_vals > o_vals, TRUE,
                   ifelse(tm_vals < o_vals, FALSE, NA)) 
  } else {
    pred <- ifelse(tm_vals - o_vals >= abs(min_diff), TRUE,
                   ifelse(tm_vals - o_vals <= -abs(min_diff), FALSE, NA))
  }

  ## return 
  return(pred)
}


## this function predicts win if team's metric is lower than that of opponent
pred_win_lower_val <- function(tm_vals, o_vals, min_diff=NULL) {
  
  ## make prediction
  if (is.null(min_diff) || min_diff==0) {
    pred <- ifelse(tm_vals < o_vals, TRUE,
                   ifelse(tm_vals > o_vals, FALSE, NA))
  } else {
    pred <- ifelse(o_vals - tm_vals >= abs(min_diff), TRUE,
                   ifelse(tm_vals - o_vals >= abs(min_diff), FALSE, NA))
  }
  
  ## return 
  return(pred)
}


## this function creates win pred acc df of given metric columns
create_win_pred_acc_df <- function(master_df, metric_cols, min_diff=NULL, min_n=0) {

  ## create vector of opponent metrics
  o_metric_cols <- paste0('o_', metric_cols)

  ## create vector of gm cnt cols 
  gm_cnt_cols <- paste0('n_', unlist(lapply(strsplit(metric_cols, '_'), function(x) {rev(x)[1]})))
  o_gm_cnt_cols <- paste0('o_', gm_cnt_cols)
  
  ## create vector of metrics used for evaluation
  metrics <- unlist(lapply(strsplit(metric_cols, '_'), function(x) {x[1]}))

  ## initialize vectors to store values
  acc_vec <- n_pred_vec <- min_n_vec <- min_diff_vec <- c()
  
  ## if min_diff is NULL, replace with 0 (in order to run for-loop later)
  if (is.null(min_diff)) min_diff <- 0

  ## for each metric col
  for (i in 1:length(metric_cols)) {
    
    ## get metric cols to compare
    metric_col <- metric_cols[i]
    o_metric_col <- o_metric_cols[i]
    
    ## get metric used for comparison
    metric <- metrics[i]

    ## get gm cnt cols for filtering
    gm_cnt_col <- gm_cnt_cols[i]
    o_gm_cnt_col <- o_gm_cnt_cols[i]
    
    ## for each min_diff specified 
    for (md in min_diff) {
      
      ## make prediction
      # case when higher metric val predicts win
      if (metric %in% c('oeff', 'FGP', 'rqP', 'pos', 'wpc')) {
        pred <- pred_win_higher_val(tm_vals=master_df[[metric_col]], 
                                    o_vals=master_df[[o_metric_col]], 
                                    min_diff=md)
      } 
      
      # case when lower metric val predicts win
      else if (metric %in% c('oeffA', 'FGPA', 'rqPA', 'posA', 'line')) {
        pred <- pred_win_lower_val(tm_vals=master_df[[metric_col]], 
                                   o_vals=master_df[[o_metric_col]], 
                                   min_diff=md)
      } 
      
      # error case
      else {
        stop(paste('Function unequipped to make predictions for the following metric:', metric))
      }
      
      ## for each min_n specified 
      for (mn in min_n) {
        
        ## nullify predictions that were made with fewer than min_n
        pred[master_df[[gm_cnt_col]] < mn | master_df[[o_gm_cnt_col]] < mn] <- NA
        
        ## calculate accuracy and number of data points (i.e. number of predictions made)
        cnf_mtx <- table(master_df$won, pred)
        acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
        n_pred <- sum(cnf_mtx)
        
        ## add values to vectors
        acc_vec <- c(acc_vec, acc)
        n_pred_vec <- c(n_pred_vec, n_pred)
        min_n_vec <- c(min_n_vec, mn)
        min_diff_vec <- c(min_diff_vec, md)
      }
    }
  }
  
  ## create df from vectors
  acc_df <- cbind.data.frame(metric = metric_cols, 
                             acc = acc_vec, 
                             n_pred = n_pred_vec, 
                             min_n = min_n_vec,
                             min_diff = min_diff_vec)
  
  ## sort by highest acc
  acc_df <- sortByCol(acc_df, col='acc', asc=FALSE)
  
  ## return 
  return(acc_df)
}

