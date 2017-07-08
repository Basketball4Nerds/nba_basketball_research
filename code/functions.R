
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


############ FUNCTIONS TO PREDICT AND CALCULATE ACCURACY PERCENTAGES ################

## this function creates a "variation df"
## which lists out different combinations
## of team vs. opponent based on 
## site, conference, and off/def rank group;
## it also lists variable-specific team tags
createVarDf <- function(by=c('site', 'cnf', 'OG', 'DG'),
                        include.opp.cols=TRUE,
                        metric=c('w_pc')) {
  
  ## weed out error cases
  by <- unique(by)
  if (!all(by %in% c('site', 'cnf', 'OG', 'DG'))) stop('Incorrect variable given. Try again.')
  
  ## specify variable options
  site_opts <- c('H', 'A')
  cnf_opts <- c('E', 'W')
  OG_opts <- c('A', 'B', 'C')
  DG_opts <- c('A', 'B', 'C')
  
  ## create options list
  tm_opts_lst <- list()
  o_opts_lst <- list()
  for (var in by) {
    tm_var_opts <- get(paste0(var, '_opts'))
    tm_opts_lst <- c(tm_opts_lst, list(tm_var_opts))
    o_var_opts <- get(paste0(var, '_opts'))
    o_opts_lst <- c(o_opts_lst, list(o_var_opts))
  }
  opts_lst <- c(tm_opts_lst, o_opts_lst)
  names(opts_lst) <- c(by, paste0('o_', by))
  
  ## create variable df from variable options list
  var_df <- expand.grid(opts_lst)
  
  ## weed out incorrect cases (e.g. two teams both can't have home games)
  if ('site' %in% by) 
    var_df <- var_df[var_df$site != var_df$o_site, ]
  
  ## initialize specifity tag
  tm_tags <- o_tags <- rep('', nrow(var_df))
  
  ## append H/A specificity
  if ('site' %in% names(var_df)) {
    tm_tags <- paste0(tm_tags, '_a', var_df$site)
    o_tags <- paste0(o_tags, '_a', var_df$o_site)
  }
  
  ## append vs E/W specificity
  if ('cnf' %in% names(var_df)) {
    tm_tags <- paste0(tm_tags, '_v', var_df$o_cnf)
    o_tags <- paste0(o_tags, '_v', var_df$cnf)
  }
  
  ## append vs offense group specificity
  if ('OG' %in% names(var_df)) {
    tm_tags <- paste0(tm_tags, '_vOG', var_df$o_OG)
    o_tags <- paste0(o_tags, '_vOG', var_df$OG)
  }
  
  ## append vs defense group specificity
  if ('DG' %in% names(var_df)) {
    tm_tags <- paste0(tm_tags, '_vDG', var_df$o_DG)
    o_tags <- paste0(o_tags, '_vDG', var_df$DG)
  }
  
  ## incorporate the comparable metrics into the variation df
  var_df <- cbind(var_df, tm_tags=tm_tags, o_tags=o_tags)
  
  ## if opponent metrics are not desired
  if (!include.opp.cols) {
    var_df <- var_df[ , !grepl('o_', names(var_df))]
    var_df <- unique(var_df)
  }
  
  ## apply as.character function to each column
  var_df <- sapply(var_df, as.character)
  
  ## turn back into data frame
  var_df <- as.data.frame(var_df, stringsAsFactors=FALSE)
  
  ## return
  return(var_df)
}



## this function creates df of simple retrospective win prediction strengths (RWPS)
## by given metrics
createSimpleRetroWinPredAccDf <- function(df, cols) {

  ## initialize an empty list
  lst <- list()

  ## calculate retrospective win prediction strength (RWPS) for each metric
  for (col in cols) {

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
    if (!(o_col %in% names(df))) {
      print(paste('Unable to locate the following metric:', col))
      next
    }

    ## make a simple retrospective prediction
    pred <- df[ , col] > df[ , o_col]

    ## create confusion matrix
    cnf_mtx <- table(df$won, pred)

    ## calculate prediction accuracy
    acc <- calc_acc_fr_cnf_mtx(cnf_mtx)

    ## calculate the number of data points to calculate retro pred acc
    nDp <- sum(cnf_mtx)

    ## create a list element
    lstElm <- c(col, acc, nDp)

    ## append list element to list
    lst <- c(lst, list(lstElm))

  }

  ## collapse list into df
  retDf <- do.call(rbind.data.frame, lst)

  ## set colnames for df
  names(retDf) <- c('metric', 'SRWPS', 'nDp')

  ## set proper data type
  retDf$SRWPS <- as.numeric(as.character(retDf$SRWPS))
  retDf$nDp <- as.integer(as.character(retDf$nDp))

  ## return
  return(retDf)
}


## this function returns a vector of variable-specific index
createVarSpIndex <- function(master_df, var_df_row, n_min) {
  
  ## grab variable columns
  var_cols <- grep('^(o_)?site$|^(o_)?cnf$|^(o_)?OG$|^(o_)?DG$', names(var_df_row), value=TRUE)
  
  ## initialize list of indices      
  ind_lst <- list()
  
  ## populate list of indices for each variable
  for (var_col in var_cols) {
    var_val <- var_df_row[[var_col]]
    ind <- master_df[[var_col]]==var_val
    ind_lst <- c(ind_lst, list(ind))
  }

  ## add to list of indices requirement for n-game minimum threshold
  min_n_col <- paste0('n', var_df_row$tm_tags)
  o_min_n_col <- paste0('o_n', var_df_row$o_tags)
  if (all(c(min_n_col, o_min_n_col) %in% names(master_df))) 
    ind <- (master_df[[min_n_col]] >= n_min) && (master_df[[o_min_n_col]] >= n_min)
  else
    ind <- (master_df$n_gen >= n_min) && (master_df$o_n_gen >= n_min)
  ind_lst <- c(ind_lst, list(ind))
  
  ## reduce list of index conditions with "and" operator
  ind <- Reduce('&', ind_lst)
  
  ## return
  return(ind)
}


## this function converts params_df and converts to params_lst
convertParamsDfToLst <- function(params_df) {
  params_lst <- apply(params_df, 1, as.list)
  params_lst <- lapply(params_lst, function(x) {
    x$n_min <- as.integer(x$n_min)
    x$min_diff <- as.numeric(x$min_diff)
    x
  })
  return(params_lst)
}








## this function takes in a number of vectors (in a list or df) and 
## returns a resultant vector by "majority vote" method
createPredByVote <- function(pred_obj, maj_vote_cnt) {
  
  ## get number of votes made for each game
  if (class(pred_obj)=='data.frame') 
    n <- ncol(pred_obj)
  else if (class(pred_obj)=='list') 
    n <- length(pred_obj)
  
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


## this function returns df of win prediction accuracies
## when predicting wins by various win metrics
createWinPredAccDf <- function(master_df, params_df, rm.irr.cols=FALSE) {
  
  ## get params_lst from params_df
  params_lst <- convertParamsDfToLst(params_df)

  ## create empty vectors to store values
  acc_vec <- n_pred_vec <- c()

  ## for each list of parameters
  for (params in params_lst) {
    
    print(params)
    
    ## make prediction, calculate accuracy, calculate sample size
    pred <- createWinPred(master_df, params)
    cnf_mtx <- table(master_df$won, pred)
    acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
    n_pred <- sum(cnf_mtx)

    ## append result to vectors
    acc_vec <- c(acc_vec, acc)
    n_pred_vec <- c(n_pred_vec, n_pred)
    print('suc')
  }

  ## create return df
  ret_df <- cbind.data.frame(params_df, acc=acc_vec, n_pred=n_pred_vec)

  # remove column whose values are all NAs or all 0s
  if (rm.irr.cols) {
    for (col in names(ret_df)) {
      if (all(is.na(ret_df[[col]]))) ret_df[[col]] <- NULL
      if (is.numeric(ret_df[[col]]) && all(ret_df[[col]]==0)) ret_df[[col]] <- NULL
    }
  }

  ## return
  return(ret_df)
}


## this function creates a df of win predictions
createWinPredDf <- function(master_df, params_df) {
  
  ## initialize empty list to store vectors of predictions
  pred_lst <- list()
  
  ## make prediction using each metric
  for (params in params_lst) {
    pred <- createWinPred(master_df, params)
    pred_lst <- c(pred_lst, list(pred))
  }
  
  # ## label the list elements
  # names(pred_lst) <- paste0('w_pred_by_', metrics)
  
  ## convert list of predictions to df
  pred_df <- do.call(cbind.data.frame, pred_lst)
  
  ## return
  return(pred_df)
}


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


## this function create all possible combinations of given metrics
## and returns as a list
createMetricCmbLst <- function(metrics) {
  
  ## initialize empty metric combination list
  metric_cmb_lst <- list()
  
  ## create all possible combinations of metrics and add to list
  for (i in 2:length(metrics)) {
    metric_cmb_lst <- c(metric_cmb_lst, lapply(apply(combn(metrics, i), 2, as.list), unlist))
  }
  
  ## return
  return(metric_cmb_lst)
}







