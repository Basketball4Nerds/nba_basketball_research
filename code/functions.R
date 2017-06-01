
############ FUNCTIONS TO PREDICT AND CALCULATE ACCURACY PERCENTAGES ################

## this function calculates prediction accuracy from confusion matrix
calcAccFrConfMtx <- function(confMtx, rndDgt=3) {
  confMtx <- as.matrix(confMtx)
  accPerc <- sum(diag(confMtx)) / sum(confMtx)
  accPerc <- round(accPerc, rndDgt)
  return(accPerc)
}

## this function creates a "variation df"
## which lists out different combinations
## of team vs. opponent based on 
## site, conference, and off/def rank group;
## it also lists which team metric should be compared which
## opponent metric
createVarDf <- function(by=c('site', 'cnf', 'OG', 'DG'),
                        include.opp.cols=TRUE, 
                        metric=c('w_pc')) {
  
  ## possible combinations are: 
  # - site-cnf
  # - site-OG
  # - site-DG
  ## not allowed combinations are:
  # - cnf-OG
  # - cnf-DG
  # - OG-DG
  
  ## weed out error cases
  by <- unique(by)
  if (!all(by %in% c('site', 'cnf', 'OG', 'DG'))) stop('Incorrect variable given. Try again.')
  if (length(by) > 2) stop('Too many variables given. Please limit to 2.')    
  else if (length(by)==2 && !('site' %in% by)) stop('Incorrect variable combinations provided. With two variable combinations, one must be site.')
  
  ## specify variable options
  site_opts <- c('H', 'A')
  cnf_opts <- c('E', 'W')
  OG_opts <- c('A', 'B', 'C')
  DG_opts <- c('A', 'B', 'C')
  
  ## create varDf by expanding options
  if (length(by)==1) {
    opts <- get(paste0(by, '_opts'))
    varDf <- expand.grid(opts, opts)
    names(varDf) <- c(by, paste0('o_', by))
  }  else if (length(by)==2) {
    opts1 <- get(paste0(by[1], '_opts'))
    opts2 <- get(paste0(by[2], '_opts'))
    varDf <- expand.grid(opts1, opts2, opts1, opts2)
    names(varDf) <- c(by[1], by[2], paste0('o_', by[1]), paste0('o_', by[2]))
  }
  
  ## weed out incorrect cases (e.g. two teams both can't have home games)
  if ('site' %in% names(varDf)) {
    varDf <- varDf[varDf$site != varDf$o_site, ]
  }
  
  ## initialize values 
  wPcCols <- rep('wPc', nrow(varDf))
  o_wPcCols <- rep('o_wPc', nrow(varDf))
  
  ## append H/A specificity
  if ('site' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, varDf$site)
    o_wPcCols <- paste0(o_wPcCols, varDf$o_site)
  }
  
  ## append vs E/W specificity
  if ('cnf' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, 'Vs', varDf$o_cnf)
    o_wPcCols <- paste0(o_wPcCols, 'Vs', varDf$cnf)
  } 
  
  ## append vs offense group specificity
  if ('OG' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, 'VsOG', varDf$o_OG)
    o_wPcCols <- paste0(o_wPcCols, 'VsOG', varDf$OG)
  }
  
  ## append vs defense group specificity  
  if ('DG' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, 'VsDG', varDf$o_DG)
    o_wPcCols <- paste0(o_wPcCols, 'VsDG', varDf$DG)
  }
  
  ## add win and n game columns
  wCols <- gsub('^wPc', 'w', wPcCols)
  nCols <- gsub('^wPc', 'n', wPcCols)
  o_wCols <- gsub('^o_wPc', 'o_w', o_wPcCols)
  o_nCols <- gsub('^o_wPc', 'o_n', o_wPcCols)
  
  ## incorporate the comparable metrics into the variation df 
  varDf <- cbind(varDf, 
                 wPcCol=wPcCols, o_wPcCol=o_wPcCols, 
                 nCol=nCols, o_nCol=o_nCols, wCol=wCols, o_wCol=o_wCols)
  
  ## if opponent metrics are not desired
  if (!include.opp.cols) {
    varDf <- varDf[ , !grepl('o_', names(varDf))]
    varDf <- unique(varDf)
  }
  
  ## apply as.character function to each column
  varDf <- sapply(varDf, as.character)
  
  ## turn back into data frame
  varDf <- as.data.frame(varDf, stringsAsFactors=FALSE)
  
  ## return
  return(varDf)
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
    cnfMtx <- table(df$won, pred)
    
    ## calculate prediction accuracy
    acc <- calcAccFrConfMtx(cnfMtx)
    
    ## calculate the number of data points to calculate retro pred acc
    nDp <- sum(cnfMtx)
    
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
  by_cols <- grep('^(o_)?site$|^(o_)?cnf$|^(o_)?OG$|^(o_)?DG$', names(var_df_row), value=TRUE)
  
  ## initialize list of indices      
  ind_lst <- list()
  
  ## populate list of indices for each variable
  for (by_col in by_cols) {
    by_val <- var_df_row[[by_col]]
    ind <- master_df[[by_col]]==by_val
    ind_lst <- c(ind_lst, list(ind))
  }
  
  ## add to list of indices requirement for n-game minimum threshold
  min_n_col <- var_df_row$nCol
  o_min_n_col <- var_df_row$o_nCol
  ind <- master_df[[min_n_col]] >= n_min & master_df[[o_min_n_col]] >= n_min
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


## this function takes master_df, var_df, by, and n_min
## and output a vector of win predictions
createWinPred <- function(master_df, params) {

  ## get parameters
  metric <- params$metric
  by <- params$by
  n_min <- params$n_min
  min_diff <- abs(params$min_diff)

  ## stop if a given metric cannot be variable-specific (e.g. site cannot be varied by conference)
  if (metric %in% c('site', 'line', 'mtch_mrgn', 'j', 'rst')) 
    if (!all(is.na(by))) stop('Invalid params given. The metric cannot be variable-specific.')

  ## stop if minimum differential was specified for site metric  
  if (metric=='site')
    if (!is.na(min_diff)) stop('Invalid params given. min_diff cannot be specified with site metric.')

  ## if 'by' variable is not specified and not applicable,
  ## make simple predictions without 'by' variable 
  if (all(is.na(by))) {
    
    ## for site metric, predict that home team will win
    if (metric=='site') {
      pred <- master_df$site=='H'
    }
    
    ## predict that favored team will win
    else if (metric=='line') {
      if (is.na(min_diff)) {
        pred <- ifelse(master_df$line < 0, TRUE, 
                       ifelse(master_df$line > 0, FALSE, NA))
      } else {
        pred <- ifelse(master_df$line <= -min_diff, TRUE, 
                       ifelse(master_df$line >= min_diff, FALSE, NA))
      }
    }
    
    ## predict that whichever team who has won more games 
    ## against the other team will win
    else if (metric=='mtch_mrgn') {
      if (is.na(min_diff))   {
        pred <- ifelse(master_df$mtch_mrgn > 0 , TRUE, 
                       ifelse(master_df$mtch_mrgn < 0, FALSE, NA))
      } else {
        pred <- ifelse(master_df$mtch_mrgn >= min_diff, TRUE, 
                       ifelse(master_df$mtch_mrgn <= -min_diff, FALSE, NA))
      }
    } 
    
    ## predict that whichever team with higher metric will win
    else if (metric %in% c('j', 'rst', 'wPc')) {
      
      ## create opponent metric
      o_metric <- paste0('o_', metric)
      
      ## predict that whichever team that has more/higher rest, J, or win percentage will win the game
      if (is.na(min_diff)) {
        pred <- ifelse(master_df[[metric]] > master_df[[o_metric]], TRUE,
                       ifelse(master_df[[metric]] < master_df[[o_metric]], FALSE, NA))
      } else {
        pred <- ifelse(master_df[[metric]] - master_df[[o_metric]] >= min_diff, TRUE,
                       ifelse(master_df[[metric]] - master_df[[o_metric]] <= -min_diff, FALSE, NA))
      }
    } 
    
    ## stop if wrong metric given
    else {
      stop('Metric not found in the master dataset.')
    }      
    
    ## filter out predictions by using min n-game threshold
    pred[master_df$n < n_min | master_df$o_n < n_min] <- NA
  }


  ## if 'by' variable is specified and applicable,
  ## make variable-specific (e.g. cnf-specific) predictions
  else {
    
    ## create var_df to obtain rows of variations
    var_df <- createVarDf(by=by)

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

  ## return
  return(pred)
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
    
    ## make prediction, calculate accuracy, calculate sample size
    pred <- createWinPred(master_df, params)
    cnf_mtx <- table(master_df$won, pred)
    acc <- calcAccFrConfMtx(cnf_mtx)
    n_pred <- sum(cnf_mtx)

    ## append result to vectors
    acc_vec <- c(acc_vec, acc)
    n_pred_vec <- c(n_pred_vec, n_pred)
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
      acc_vec <- c(acc_vec, calcAccFrConfMtx(cnf_mtx))
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
