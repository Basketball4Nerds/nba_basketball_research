
## this function creates a list of initialized team objects
createTmObLst <- function(gm_sch_df) {
  
  ## initialize an empty list
  tm_obs_lst <- vector(mode = "list", length = length(TEAMS))
  
  ## label the list objects
  names(tm_obs_lst) <- TEAMS
  
  ## for each team name
  for (team_name in TEAMS) {
    
    ## create team object
    tm_ob <- Team(name=team_name, gm_sch_df=gm_sch_df)
    
    ## add to list of team objects
    tm_obs_lst[[team_name]] <- tm_ob
  }
  
  ## return
  return(tm_obs_lst)
}






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
    ind <- (master_df$n >= n_min) && (master_df$o_n >= n_min)
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
    acc <- calcAccFrConfMtx(cnf_mtx)
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


## this function takes in a vector a retro-fills any NA values 
## e.g. c(NA, 1, 2, NA, NA, 3) would become c(1, 1, 2, 3, 3, 3)
retro_fill_nas <- function(x) {
  
  ## get a vector of non-NA indices 
  complete_nna_indices <- which(!is.na(x))
  
  ## for each non-NA index, apply retro-fill
  for (ind in complete_nna_indices) {
    
    ## get value to retro-fill
    retro_fill_val <- x[ind]
    
    ## get indices of values to retro-fill
    complete_retro_na_indices <- which(is.na(x[1:ind]))
    
    ## retro-fill with value
    x[complete_retro_na_indices] <- retro_fill_val
  }
  
  ## return
  return(x)
}


## this function creates performance-standing-by-date df
create_std_by_date_df <- function(master_df, metric) {
  
  ## create df with relevant columns
  df <- master_df[, c('date', 'team', metric)]
  
  ## create an initial standing-by-date df that contains NA "holes"
  std_by_date_df <- dcast(df, date ~ team, value.var=metric)
  
  ## store game dates in a variable
  gm_dates <- std_by_date_df$date
  
  ## remove game dates column in df and store as row names
  std_by_date_df$date <- NULL
  rownames(std_by_date_df) <- gm_dates
  
  ## for each team
  for (team in names(std_by_date_df)) {
    
    ## grab team's vector with NA "holes" to retro-fill
    x <- std_by_date_df[, team]
    
    ## retro-fill NAs
    x <- retro_fill_nas(x)
    
    ## first game date (and dates prior to the first game) should be assigned NA
    first_gm_date <- df[df$team==team, 'date'][1]
    na_indices <- 1:which(first_gm_date==gm_dates)
    x[na_indices] <- NA
    
    ## put retro-filled vector back into df
    std_by_date_df[, team] <- x
  }
  
  ## return standing-by-date df
  return(std_by_date_df)
}


## this function takes in a vector of numeric values and return letter-based 
## ranks (A, B, C, ...) based on quantile distribution, where A signifies the best 
## performance indicator
ret_ABC_rnks_by_qntl <- function(x, higher_num_bttr_perf) {
  
  ## use previous seasons to calculate quantiles
  qntls <- quantile(x, probs=seq(0, 1, 1/3), na.rm=TRUE)
  
  ## if higher numeric value signifies better performance
  if (higher_num_bttr_perf) {
    rnks <- ifelse(x >= qntls[3], 'A', 
                   ifelse(x <= qntls[2], 'C', 'B'))
  }
  
  ## if higher numeric value signifies lower performance
  else {
    rnks <- ifelse(x >= qntls[3], 'C',
                   ifelse(x <= qntls[2], 'A', 'B'))
  }
  
  ## return
  return(rnks)
}


## this function takes in a vector of numeric values and return letter-based
## ranks (A, B, C) based on standard deviation, where A signifies the best 
## performance indicator
ret_ABC_rnks_by_sd <- function(x, higher_num_bttr_perf) {

  ## calculate mean and sd of te numeric values provided
  mean <- mean(x)
  sd <- sd(x)
  
  ## if higher numeric value signifies better performance
  if (higher_num_bttr_perf) {
    rnks <- ifelse(x >= (mean + sd), 'A', 
                   ifelse(x <= (mean - sd), 'C', 'B'))
  }
  
  ## if higher numeric value signifies lower performance
  else {
    rnks <- ifelse(x >= (mean + sd), 'C',
                   ifelse(x <= (mean - sd), 'A', 'B'))
  }  
    
  ## return 
  return(rnks)
}



## this function takes in a df and returns an empty copy of the original
create_empty_df_copy <- function(df) {
  empty_df_copy <- matrix(NA, nrow=nrow(df), ncol=ncol(df))
  colnames(empty_df_copy) <- colnames(df)
  rownames(empty_df_copy) <- rownames(df)
  return(empty_df_copy)
}

## this function takes in standing-by-date df and creates 
## rank-standing-by-date df
create_rnkd_tm_std_by_date_df <- function(master_df, metric, higher_num_bttr_perf) {

  ## apply the following lines of code by season
  ms_df <- ddply(master_df, 'season', function(ss_df) {
    
    ## create standing-by-date df (consists of numeric values)
    std_by_date_df <- create_std_by_date_df(ss_df, metric)
    
    ## initialize two ranked-standings-by-date dfs 
    ## by copying dimensions from original df
    qntl_rnkd_std_by_date_df <- create_empty_df_copy(std_by_date_df)  # quantile derivation
    sd_rnkd_std_by_date_df <- create_empty_df_copy(std_by_date_df)  # standard deviation derivation
    
    ## index where all row values are filled and NA-free
    complete_row_strt_ind <- max(apply(std_by_date_df, 2, function(x) min(which(!is.na(x)))))
    
    ## starting from rows where performance values are present for all teams
    for (i in complete_row_strt_ind:nrow(std_by_date_df)) {
      
      ## get current standing performance as a vector
      x <- as.numeric(std_by_date_df[i, ])
      
      ## fill in quantile-derivation ranks
      qntl_rnks <- ret_ABC_rnks_by_qntl(x, higher_num_bttr_perf)
      qntl_rnkd_std_by_date_df[i, ] <- qntl_rnks
      
      ## fill in sd-derivation ranks
      sd_rnks <- ret_ABC_rnks_by_sd(x, higher_num_bttr_perf)
      sd_rnkd_std_by_date_df[i, ] <- sd_rnks
    }
    
    ## convert matrix to df
    qntl_rnkd_std_by_date_df <- as.data.frame(qntl_rnkd_std_by_date_df, stringsAsFactors=FALSE)
    sd_rnkd_std_by_date_df <- as.data.frame(sd_rnkd_std_by_date_df, stringsAsFactors=FALSE)
    
    ## add date column  
    qntl_rnkd_std_by_date_df$date <- as.Date(rownames(qntl_rnkd_std_by_date_df))
    sd_rnkd_std_by_date_df$date <- as.Date(rownames(sd_rnkd_std_by_date_df))
    
    ## get rank nomenclature prefix from metric
    rnk_nm_prefix <- strsplit(metric, '_')[[1]][1]
    
    ## melt dfs
    qntl_rnk_df <- melt(qntl_rnkd_std_by_date_df, 
                        id.vars='date', variable.name='team', 
                        value.name=paste0(rnk_nm_prefix, '_qntl_rnk'))
    sd_rnk_df <- melt(sd_rnkd_std_by_date_df, 
                      id.vars='date', variable.name='team', 
                      value.name=paste0(rnk_nm_prefix, '_sd_rnk'))
    
    ## merge two dfs
    mrgd_df <- merge(qntl_rnk_df, sd_rnk_df, by=c('team', 'date'))
    
    ## return df for single season
    mrgd_df
  })

  
  ## un-factor team column
  ms_df$team <- as.character(ms_df$team)
  
  ## return
  return(ms_df)
}


## this function adds A-B-C offensive/defensive rank standings columns
add_rnk_cols <- function(master_df) {
  
  ## create ranked-team-standing-by-date df for offensive and defensive efficiency
  a <- create_rnkd_tm_std_by_date_df(master_df, metric='oeff_cum_gen', higher_num_bttr_perf=TRUE)
  b <- create_rnkd_tm_std_by_date_df(master_df, metric='oeffA_cum_gen', higher_num_bttr_perf=FALSE)
  c <- create_rnkd_tm_std_by_date_df(master_df, metric='FGP_cum_gen', higher_num_bttr_perf=TRUE)
  d <- create_rnkd_tm_std_by_date_df(master_df, metric='FGPA_cum_gen', higher_num_bttr_perf=FALSE)
  
  ## merge those dfs onto original master df
  master_df <- left_join(master_df, a, by=c('season', 'team', 'date'))
  master_df <- left_join(master_df, b, by=c('season', 'team', 'date'))
  master_df <- left_join(master_df, c, by=c('season', 'team', 'date'))
  master_df <- left_join(master_df, d, by=c('season', 'team', 'date'))
  
  ## return
  return(master_df)
}

