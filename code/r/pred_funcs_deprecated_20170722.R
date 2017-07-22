


## this function returns a vector of metrics being evaluated from given metric columns; 
get_metrics_fr_metric_cols <- function(metric_cols) {
  metrics <- unlist(lapply(strsplit(metric_cols, '_'), function(x) {x[1]}))
  metrics <- gsub('j[0-9]*', 'j', metrics)
  return(metrics)
}


## this function returns a vector of corresponding gm cnt cols from given metric columns
get_gm_cnt_cols_fr_metric_cols <- function(metric_cols) {
  
  ## clean metric_cols (in case opponent metric cols were given)
  metric_cols <- gsub('^o_', '', metric_cols)
  
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
    if (metric %in% c('wpc', 'cumperf', 
                      'oeff', 'oeffA', 'FGP', 'FGPA', 
                      'rqP', 'rqPA', 'pos', 'posA')) {
      
      ## get vary-by suffix from metric column name
      varyby_suffix <- rev(strsplit(metric_col, '_')[[1]])[1]
      
      ## construct gm cnt col
      gm_cnt_col <- paste0('n_cumcnt_', varyby_suffix)
    }
    
    ## if metric cannot be variable-specific
    else if (metric %in% c('j', 'line', 'site', 'mtchmrgn', 'rst')) {
      gm_cnt_col <- 'n_cumcnt_gen'
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


## this fucntion checks validity of wpa_df under the following premise:
## given the same metric and min_diff, there must be a fewer number of 
## predictions made for higher min_n
is_valid_wpa_df <- function(wpa_df) {
  
  ## split wpa_df into a list of dfs using metric and min_diff
  wpa_df_lst <- split(wpa_df, list(wpa_df$metric, wpa_df$min_diff))
  
  ## check validity by checking there exists a lower n_pred for a higher min_n
  valid_cond_lst <- lapply(wpa_df_lst, function(x) {
    x <- sortByCol(x, col='min_n')
    is.sorted(rev(x$n_pred))
  })
  
  ## convert list of validity conditions to vector
  valid_conds <- unlist(valid_cond_lst)
  
  ## return TRUE if all validity conditions are TRUE
  return(all(valid_conds))
}


## this function makes win prediction based on a given metric column and master_df;
## it is capable of making predictions based on the following metrics: 
## oeff, oeffA, FGP, FGPA, rqP, rqPA, pos, posA, wpc, j, rst, line, site, mtchmrgn
pred_win_by_metric_col <- function(master_df, metric_col, min_diff=NULL, min_n=0) {
  
  ## create metric used for evaluation
  metric <- get_metrics_fr_metric_cols(metric_col)
  
  ## get specific opponent metric column (if applicable)
  if (!(metric %in% c('line', 'site', 'mtchmrgn')))
    o_metric_col <- get_o_metric_cols_fr_metric_cols(metric_col)
  
  ## create gm cnt cols used for filtering
  gm_cnt_col <- get_gm_cnt_cols_fr_metric_cols(metric_col)
  o_gm_cnt_col <- paste0('o_', gm_cnt_col)
  
  ## make prediction
  # case when higher metric val predicts win
  if (metric %in% c('oeff', 'FGP', 'rqP', 'pos', 'wpc', 'j', 'rst')) {
    pred <- pred_win_higher_val(tm_vals=master_df[[metric_col]], 
                                o_vals=master_df[[o_metric_col]], 
                                min_diff=min_diff)
  } 
  
  # case when lower metric val predicts win
  else if (metric %in% c('oeffA', 'FGPA', 'rqPA', 'posA')) {
    pred <- pred_win_lower_val(tm_vals=master_df[[metric_col]], 
                               o_vals=master_df[[o_metric_col]], 
                               min_diff=min_diff)
  } 
  
  # case when home-team is predicted to win
  else if (metric=='site') {
    pred <- pred_win_by_site(site=master_df$site)
  }
  
  # case when favored team is predicted to win
  else if (metric=='line') {
    pred <- pred_win_by_line(line=master_df$line, min_diff=min_diff)
  }
  
  # case when 
  else if (metric=='mtchmrgn') {
    pred <- pred_win_by_mtchmrgn(mtchmrgn=master_df$mtchmrgn, min_diff=md)
  }
  
  # error case
  else {
    stop(paste('pred_win_by_metric_col() unequipped to make predictions for the following metric:', metric))
  }
  
  ## nullify predictions that were made with fewer than min_n
  pred[master_df[[gm_cnt_col]] < min_n | master_df[[o_gm_cnt_col]] < min_n] <- NA
  
  ## return
  return(pred)
}


## this function creates win pred acc df of given metric columns
create_win_pred_acc_df <- function(master_df, metric_cols, min_diff=NULL, min_n=0) {
  
  ## clean metric_cols (in case opponent metric cols were given)
  metric_cols <- gsub('^o_', '', metric_cols)
  
  ## initialize vectors to store values
  metric_col_vec <- acc_vec <- n_pred_vec <- min_n_vec <- min_diff_vec <- c()
  
  ## if min_diff is NULL, replace with 0 (in order to run for-loop later)
  if (is.null(min_diff)) min_diff <- 0
  
  ## for each metric col
  for (metric_col in metric_cols) {
    
    ## for each min_diff specified 
    for (md in min_diff) {
      
      ## for each min_n specified 
      for (mn in min_n) {
        
        ## make prediction by metric column
        pred <- pred_win_by_metric_col(master_df, metric_col=metric_col, min_diff=md, min_n=mn)
        
        ## calculate accuracy and number of data points (i.e. number of predictions made)
        cnf_mtx <- table(master_df$won, pred)
        acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
        n_pred <- sum(cnf_mtx)
        # n_pred <- sum(!is.na(pred))
        
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
  if (is_valid_wpa_df(acc_df))
    return(acc_df)
  stop('Incorrect wpa_df produced; create_win_pred_acc_df() may be buggy.')
}



## this function takes in a list/df of prediction vectors and 
## returns a resultant prediction vector by "majority vote" method
pred_win_by_maj_vote <- function(pred_obj, maj_vote_cnt=NULL) {
  
  ## set majority vote count if not specified
  if (is.null(maj_vote_cnt)) {
    
    ## if pred obj is a list of pred vectors
    if (is.list(pred_obj))
      maj_vote_cnt <- ceiling(length(pred_obj) / 2)
    
    ## if pred obj is a df of pred vectors
    else if (is.data.frame(pred_obj))
      maj_vote_cnt <- ceiling(ncol(pred_obj) / 2)
  }
  
  ## get a vector of win prediction vote counts
  w_pred_vote_cnts <- Reduce('+', lapply(pred_obj, is.true))
  
  ## get a vector of loss prediction vote counts
  l_pred_vote_cnts <- Reduce('+', lapply(pred_obj, is.false))
  
  ## make win prediction by majority vote
  w_preds <- ifelse(w_pred_vote_cnts >= maj_vote_cnt & w_pred_vote_cnts >= l_pred_vote_cnts, TRUE, 
                    ifelse(l_pred_vote_cnts >= maj_vote_cnt & l_pred_vote_cnts >= w_pred_vote_cnts, FALSE, NA))
  
  ## return
  return(w_preds)
}