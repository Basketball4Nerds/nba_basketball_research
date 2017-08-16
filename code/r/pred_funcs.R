
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
    else if (metric %in% c('j', 'line', 'site', 'mtchmrgn', 'rst', 'home')) {
      gm_cnt_col <- 'n_cumcnt_gen'
    } 
    
    ## stop if 
    else {
      stop(paste0("Unable to find relevant gm cnt col the following metric: ", metric))
    }
    
    ## append gm cnt col nm to vector
    gm_cnt_cols <- c(gm_cnt_cols, gm_cnt_col)
  }
  
  ## return
  return(gm_cnt_cols)
}


## this function predicts win if team's metric is higher than that of opponent
pred_win_higher_val <- function(x, y=NULL, min_diff=NULL) {
  
  ## determine differential
  if (is.null(y)) {
    diff <- x
  } else {
    diff <- x - y
  }
  
  ## make prediction
  if (is.null(min_diff) || min_diff==0) {
    pred <- ifelse(diff > 0, TRUE,
                   ifelse(diff < 0, FALSE, NA)) 
  } else {
    pred <- ifelse(diff >= abs(min_diff), TRUE,
                   ifelse(diff <= -abs(min_diff), FALSE, NA))
  }

  ## return 
  return(pred)
}


## this function predicts win if team's metric is lower than that of opponent
pred_win_lower_val <- function(x, y=NULL, min_diff=NULL) {
  
  ## determine differential
  if (is.null(y)) {
    diff <- x
  } else {
    diff <- x - y
  }
  
  ## make prediction
  if (is.null(min_diff) || min_diff==0) {
    pred <- ifelse(diff < 0, TRUE,
                   ifelse(diff > 0, FALSE, NA))
  } else {
    pred <- ifelse(-diff >= abs(min_diff), TRUE,
                   ifelse(diff >= abs(min_diff), FALSE, NA))
  }

  ## return 
  return(pred)
}


## this function predicts win by site (simply that home team will win)
pred_win_by_site <- function(site) {

  ## predict win if home game
  pred <- site=='H' | site=='home'

  ## return
  return(pred)
}


## this function makes win prediction based on a given metric column and predictive_df;
## it is capable of making predictions based on the following metrics: 
## oeff, oeffA, FGP, FGPA, rqP, rqPA, pos, posA, wpc, j, rst, line, site, mtchmrgn
pred_win_by_metric_col <- function(predictive_df, metric_col, min_diff=NULL, min_n=0) {
  
  ## create metric used for evaluation
  metric <- get_metrics_fr_metric_cols(metric_col)
  
  ## create gm cnt cols used for filtering
  gm_cnt_col <- get_gm_cnt_cols_fr_metric_cols(metric_col)
  o_gm_cnt_col <- paste0('o_', gm_cnt_col)
  
  ## make prediction
  # case when higher metric val predicts win
  if (metric %in% c('oeff', 'FGP', 'rqP', 'pos', 'wpc', 'j', 'rst', 'mtchmrgn')) {
    pred <- pred_win_higher_val(x=predictive_df[[metric_col]], min_diff=min_diff)
  } 
  
  # case when lower metric val predicts win
  else if (metric %in% c('oeffA', 'FGPA', 'rqPA', 'posA', 'line')) {
    pred <- pred_win_lower_val(x=predictive_df[[metric_col]], min_diff=min_diff)
  } 
  
  ## case when home team is predicted to win
  else if (metric=='home') {
    pred <- as.logical(predictive_df[[metric_col]])
  }
  
  # case when home team is predicted to win
  else if (metric=='site') {
    pred <- pred_win_by_site(site=predictive_df$site)
  }
  
  # error case
  else {
    stop(paste('pred_win_by_metric_col() unequipped to make predictions for the following metric:', metric))
  }
  
  ## nullify predictions that were made with fewer than min_n
  pred[predictive_df[[gm_cnt_col]] < min_n | predictive_df[[o_gm_cnt_col]] < min_n] <- NA
  
  ## return
  return(pred)
}


## this function creates win pred acc df of given metric columns
create_win_pred_acc_df <- function(predictive_df, metric_cols, min_diff=NULL, min_n=0) {
  
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
        pred <- pred_win_by_metric_col(predictive_df, metric_col=metric_col, min_diff=md, min_n=mn)

        ## calculate accuracy and number of data points (i.e. number of predictions made)
        cnf_mtx <- table(predictive_df$won, pred)
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
  
  # ## check acc_df validity
  # if (is_valid_wpa_df(acc_df))
  #   stop('Incorrect wpa_df produced; create_win_pred_acc_df() may be buggy.')
  
  ## return 
  return(acc_df)
}


## this function takes in a list/df of prediction vectors and 
## returns a resultant probability vector whose probability
## is calculated by (# of TRUE) / (# of all votes)
calc_wprob_fr_pred_votes_obj <- function(pred_obj, rnd_dgt=3) {
  
  ## get a vector of win prediction vote counts
  w_pred_vote_cnts <- Reduce('+', lapply(pred_obj, is.true))
  
  ## get a vector of total vote counts
  if (is.list(pred_obj)) {
    total_vote_cnts <- length(pred_obj)
  } else if (is.data.frame(pred_obj)) {
    total_vote_cnts <- ncol(pred_obj)    
  }
  
  ## make win prediction by majority vote
  probs <- round(w_pred_vote_cnts / total_vote_cnts, rnd_dgt)
  
  ## return
  return(probs)
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



## this function takes predictive_df (whose each metric column is a metric difference
## in performance between a team and its opponent) and converts the df
## to predictive_votes_df, where each metric column contains prediction values, TRUE or FALSE;
convert_to_predictive_votes_df <- function(predictive_df, cols) {
  
  ## for each col specified, make win/loss prediction
  for (col in cols) {
    predictive_df[[col]] <- pred_win_by_metric_col(predictive_df, metric_col=col)
  }
  
  ## return
  return(predictive_df)
}


## this function creates win probability df
create_wprob_df <- function(predictive_df, predictors, wprob_col='wprob', base_cols=c('date', 'team', 'o_team')) {
  
  ## create predictive_votes_df
  pvotes_df <- convert_to_predictive_votes_df(predictive_df[ , c(base_cols, predictors)], cols=predictors)
  
  ## add win probability calculated by number of votes
  pvotes_df$wprob <- calc_wprob_fr_pred_votes_obj(pvotes_df[ , predictors]) 
  
  ## select only base cols and win prob col
  wprob_df <- pvotes_df[ , c(base_cols, wprob_col)]  
  
  ## return
  return(wprob_df)
}

