## this function returns a vector of metrics being evaluated from given metric columns; 
## example metric cols: 
# x <- c('site', 'line', 'j10', 'mtch_mrgn', 'mtchmrgn', 'FGP_cumperf_site', 'wpc_cnf')
get_metrics_fr_metric_cols <- function(metric_cols) {
  metrics <- unlist(lapply(strsplit(metric_cols, '_'), function(x) {x[1]}))
  metrics <- gsub('j[0-9]*', 'j', metrics)
  return(metrics)
}


## this function returns a vector of corresponding gm cnt cols from given metric columns
get_gm_cnt_cols_fr_metric_cols <- function(metric_cols) {
  
  ## get vector of metrics being evaluated
  metrics <- get_metrics_fr_metric_cols(metric_cols)
  
  ## initialize vector to store gm cnt col names
  gm_cnt_cols <- c()
  
  ## for each metric column name (and its metric)
  for (i in 1:length(metric_cols)) {
    
    ## get metric column name and its metric in evaluation
    metric_col <- metric_cols[i]
    metric <- metrics[i]
    
    ## if metric can be variable-specific (e.g. site-specific)
    if (metric %in% c('wpc', 'cumperf')) {
      
      ## get vary-by suffix from metric column name
      varyby_suffix <- rev(strsplit(metric_col, '_')[[1]])[1]

      ## construct gm cnt col
      gm_cnt_col <- paste0('n_', varyby_suffix)
    }
    
    ## if metric cannot be variable-specific
    else if (metric %in% c('j', 'line', 'site', 'mtchmrgn')) {
      gm_cnt_col <- 'n_gen'
    } 
    
    ## append gm cnt col nm to vector
    gm_cnt_cols <- c(gm_cnt_cols, gm_cnt_col)
  }
  
  ## return
  return(gm_cnt_cols)
}


## this function returns a vector of corresponding opponent metric columns from given metric columns
get_o_metric_cols_fr_metric_cols <- function(metric_cols) {
  metric_cols <- gsub('^o_', '', metric_cols)
  metrics <- get_metrics_fr_metric_cols(metric_cols)
  o_metric_cols <- paste0('o_', metric_cols)
  o_metric_cols[metrics %in% c('line', 'site', 'mtchmrgn')] <- NA
  return(o_metric_cols)
}


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


## this function predicts win by site (simply that home team will win)
pred_win_by_site <- function(site) {

  ## predict win if home game
  pred <- site=='H'

  ## return
  return(pred)
}


## this function predicts win by line (simply that favored team will win)
pred_win_by_line <- function(line, min_diff=NULL) {
 
  ## make pred when min_diff is set
  if (is.null(min_diff) || min_diff==0) {
    pred <- ifelse(line < 0, TRUE, 
                   ifelse(line > 0, FALSE, NA))
  } 
  
  ## make pred when min_diff is not set
  else {
    pred <- ifelse(line <= -abs(min_diff), TRUE, 
                   ifelse(line >= abs(min_diff), FALSE, NA))
  }
  
  ## return
  return(pred)
}


## this function predicts win by matchup margin 
## (simple that whoever won more previously against the other team would win)
pred_win_by_mtchmrgn <- function(mtchmrgn, min_diff=NULL) {
  
  ## make pred when min_diff is set
  if (is.null(min_diff) || min_diff==0)   {
    pred <- ifelse(mtchmrgn > 0 , TRUE, 
                   ifelse(mtchmrgn < 0, FALSE, NA))
  } 
  
  ## make pred when min_diff is not set
  else {
    pred <- ifelse(mtchmrgn >= abs(min_diff), TRUE, 
                   ifelse(mtchmrgn <= -abs(min_diff), FALSE, NA))
  } 

  ## return  
  return(pred)
}


## create win pred acc df with rst cols
quantile(master_df$line, na.rm=TRUE)
rst_wpa_df <- create_win_pred_acc_df(master_df, 
                                     metric_cols='rst', 
                                     min_diff=c(1, 2, 3))



## this function creates win pred acc df of given metric columns
create_win_pred_acc_df <- function(master_df, metric_cols, min_diff=NULL, min_n=0) {

  ## clean metric_cols (in case opponent metric cols were given)
  metric_cols <- gsub('^o_', '', metric_cols)
  
  ## create vector of opponent metrics
  o_metric_cols <- get_o_metric_cols_fr_metric_cols(metric_cols)
  
  ## create vector of metrics used for evaluation
  metrics <- get_metrics_fr_metric_cols(metric_cols)

  ## initialize vectors to store values
  metric_col_vec <- acc_vec <- n_pred_vec <- min_n_vec <- min_diff_vec <- c()
  
  ## if min_diff is NULL, replace with 0 (in order to run for-loop later)
  if (is.null(min_diff)) min_diff <- 0

  ## for each metric col
  for (i in 1:length(metric_cols)) {

    ## get metric used for evaluation
    metric <- metrics[i]
    
    ## get specific metric column 
    metric_col <- metric_cols[i]
    
    ## get specific opponent metric column (if applicable)
    if (!(metric %in% c('line', 'site', 'mtchmrgn')))
      o_metric_col <- o_metric_cols[i]

    ## get gm cnt cols for filtering
    gm_cnt_col <- gm_cnt_cols[i]
    o_gm_cnt_col <- o_gm_cnt_cols[i]
    
    ## for each min_diff specified 
    for (md in min_diff) {
      
      ## make prediction
      # case when higher metric val predicts win
      if (metric %in% c('oeff', 'FGP', 'rqP', 'pos', 'wpc', 'j', 'rst')) {
        pred <- pred_win_higher_val(tm_vals=master_df[[metric_col]], 
                                    o_vals=master_df[[o_metric_col]], 
                                    min_diff=md)
      } 
      
      # case when lower metric val predicts win
      else if (metric %in% c('oeffA', 'FGPA', 'rqPA', 'posA')) {
        pred <- pred_win_lower_val(tm_vals=master_df[[metric_col]], 
                                   o_vals=master_df[[o_metric_col]], 
                                   min_diff=md)
      } 
      
      # case when home-team is predicted to win
      else if (metric=='site') {
        pred <- pred_win_by_site(site=master_df$site)
      }
      
      # case when favored team is predicted to win
      else if (metric=='line') {
        pred <- pred_win_by_line(line=master_df$line)
      }
      
      # case when 
      else if (metric=='mtchmrgn') {
        pred <- pred_win_by_mtchmrgn(mtchmrgn=master_df$mtchmrgn)
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
        metric_col_vec <- c(metric_col_vec, metric_col)
        acc_vec <- c(acc_vec, acc)
        n_pred_vec <- c(n_pred_vec, n_pred)
        min_n_vec <- c(min_n_vec, mn)
        min_diff_vec <- c(min_diff_vec, md)
      }
    }
  }
  
  ## create df from vectors
  acc_df <- cbind.data.frame(metric = metric_col_vec, 
                             acc = acc_vec, 
                             n_pred = n_pred_vec, 
                             min_n = min_n_vec,
                             min_diff = min_diff_vec)
  
  ## sort by highest acc
  acc_df <- sortByCol(acc_df, col='acc', asc=FALSE)
  
  ## return 
  return(acc_df)
}




