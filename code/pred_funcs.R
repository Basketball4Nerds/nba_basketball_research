## this function predicts win if team's metric is higher than that of opponent
pred_win_higher_val <- function(tm_vals, o_vals, min_diff=NULL) {
  
  ## make prediction
  if (is.null(min_diff)) {
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
  if (is.null(min_diff)) {
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
pred_win_by_site <- function(master_df, params) {
  
  ## stop if minimum differential was specified for site metric  
  if (!is.na(params$min_diff)) 
    stop('Invalid params given. min_diff cannot be specified with site metric')
  
  ## stop if variable-specification set
  if (!is.na(params$by)) 
    stop(sprintf('Invalid params given. by cannot be specified with %s metric.', params$metric))
  
  ## set n_min value to 0 if NA
  params$n_min <- ifelse(is.na(params$n_min), 0, params$n_min)
  
  ## predict win if home game
  pred <- master_df$site=='H'
  
  ## snuff out certain predictions and replace with NA by using min n-game threshold
  pred[master_df$n_gen < params$n_min | master_df$o_n_gen < params$n_min] <- NA
  
  ## return  
  return(pred)
}


# non-variable metrics: 
# site -> pred_win_by_site
# line -> pred_win_by_comp
# mtchmrgn -> pred_win_by_mtchmrgn 
#          -> pred_win_by_comp
# j -> pred_win_by_comp 
# rst -> pred_win_by_comp
# wPc -> pred_win_by_comp

# variable metrics: 
# wPc -> pred_win_by_comp 
# 'oeff_cum_gen', 'oeffA_cum_gen' -> pred_win_by_comp
# 'FGP_cum_gen', 'FGPA_cum_gen' -> pred_win_by_comp

# n_min
# min_diff
# vary_by



## this function predicts win by comparing two metrics
pred_win_by_comp <- function(master_df, params, higher_num_ind_bttr_perf) {
  
  ## create opponent metric if not present in params
  if (!('o_metric' %in% params)) o_metric <- paste0('o_', metric)
  
  ## return error if opponent metric not found in df
  if (!(o_metric %in% names(master_df))) stop('Could not find opponent metric.')
  
  ## set comparison 1 and comparison 2
  if (higher_num_ind_bttr_perf) 
    # when higher number represents higher performance
    { comp1 <- master_df[[metric]]; comp2 <- master_df[[o_metric]] }
  else 
    # when higher number represents lower performance
    { comp1 <- master_df[[o_metric]]; comp2 <- master_df[[metric]] }

  ## make prediction
  if (is.na(params$min_diff)) {
    # when min_diff is not set  
    pred <- ifelse(comp1 > comp2, TRUE,
                   ifelse(comp1 < comp2, FALSE, NA)) 
  } else {
    # when min_diff is set
    pred <- ifelse(comp1 - comp2 >= params$min_diff, TRUE,
                   ifelse(comp1 - comp2 <= -params$min_diff, FALSE, NA))
  }

  ## return
  return(pred)
}






## this function predicts win by line (simply that favored team will win)
pred_win_by_line <- function(master_df, params) {
  
  ## stop if variable-specification set
  if (!is.na(params$by)) 
    stop(sprintf('Invalid params given. by cannot be specified with %s metric.', params$metric))
  
  ## set n_min value to 0 if NA
  params$n_min <- ifelse(is.na(params$n_min), 0, params$n_min)
  
  ## make pred when min_diff is set
  if (is.na(params$min_diff)) {
    pred <- ifelse(master_df$line < 0, TRUE, 
                   ifelse(master_df$line > 0, FALSE, NA))
  } 
  
  ## make pred when min_diff is not set
  else {
    pred <- ifelse(master_df$line <= -params$min_diff, TRUE, 
                   ifelse(master_df$line >= params$min_diff, FALSE, NA))
  }
  
  ## snuff out certain predictions and replace with NA by using min n-game threshold
  pred[master_df$n_gen < params$n_min | master_df$o_n_gen < params$n_min] <- NA
  
  ## return
  return(pred)
}


# params <- list(metric='site', n_min=10, min_diff=NA, by='cnf')
# x <- predWinBySite(master_df, params)
# 
# params <- list(metric='line', n_min=10, min_diff=10, by='cnf')
# x <- predWinByLine(master_df, params)
# 
# params <- list(metric='mtch_mrgn', n_min=10, min_diff=2, by=NA)
# x <- predWinByMtchMrgn(master_df, params)


## this function predicts that whichever team who has won more games 
## against the other team will win
pred_win_by_mtchmrgn <- function(master_df, params) {
  
  ## stop if variable-specification set
  if (!is.na(params$by)) 
    stop(sprintf('Invalid params given. by cannot be specified with %s metric.', params$metric))
  
  ## make pred when min_diff is set
  if (is.na(params$min_diff))   {
    pred <- ifelse(master_df$mtch_mrgn > 0 , TRUE, 
                   ifelse(master_df$mtch_mrgn < 0, FALSE, NA))
  } 
  
  ## make pred when min_diff is not set
  else {
    pred <- ifelse(master_df$mtch_mrgn >= params$min_diff, TRUE, 
                   ifelse(master_df$mtch_mrgn <= -params$min_diff, FALSE, NA))
  } 
  
  ## snuff out certain predictions and replace with NA by using min n-game threshold
  pred[master_df$n_gen < params$n_min | master_df$o_n_gen < params$n_min] <- NA
  
  ## return  
  return(pred)
}














predWinVarSp <- function(master_df, params) {
  
  ## create var_df to obtain rows of variations
  var_df <- createVarDf(by=params$by)
  
  ## initialize prediction values
  pred <- rep(NA, nrow(master_df))
  
  ## for each variation listed in var_df
  for (i in 1:nrow(var_df)) {
    var_df_row <- var_df[i, ]
    
    ## select team and opponent metrics
    if (metric=='wPc') {
      metric_col <- var_df_row$wPcCol
      o_metric_col <- var_df_row$o_wPcCol
    } else if (metric=='sma10') {
      next  # skip for not; come back later and modify this part
    }
    
    ## create index of metric comparisons to make
    ind <- createVarSpIndex(master_df, var_df_row, n_min)
    
    ## make predictions for the applicable index
    if (is.na(min_diff)) {
      pred[ind] <- ifelse(master_df[[metric_col]] > master_df[[o_metric_col]], TRUE,
                          ifelse(master_df[[metric_col]] < master_df[[o_metric_col]], FALSE, NA))[ind]
    } else {
      pred[ind] <- ifelse(master_df[[metric_col]] - master_df[[o_metric_col]] >= min_diff, TRUE,
                          ifelse(master_df[[metric_col]] - master_df[[o_metric_col]] <= -min_diff, FALSE, NA))[ind]
    }
  }
}

## this function takes master_df, var_df, by, and n_min
## and output a vector of win predictions
pred_win <- function(master_df, params) {
  
  ## stop if a given metric cannot be variable-specific (e.g. site cannot be varied by conference)
  if (params$metric %in% c('site', 'line', 'mtchmrgn', 'j', 'rst')) 
    if (!all(is.na(params$by))) stop('Invalid params given. The metric cannot be variable-specific.')

  
  ## 
  if (params$metric=='site')
    pred <- pred_win_by_site(master_df, params)
  else if (params$metric=='line') 
    pred <- pred_win_by_line(master_df, params)
  else if (params$metric=='mtchmrgn')
    pred <- pred_win_by_mtchmrg


  ## return
  return(pred)
}
