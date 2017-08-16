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



## this function takes master_df as input and creates "predictive df" 
## that contains predictor variables and is used to make predictions 
create_predictive_df <- function(master_df, include_cnt_cols=TRUE) {
  
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
  if (include_cnt_cols) {
    ret_df <- master_df[ , c(base_cols, dep_col, cnt_cols, o_cnt_cols, 'line')]    
  } else {
    ret_df <- master_df[ , c(base_cols, dep_col, 'line')]
  }

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


## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



## this function takes a list of ranked vectors (ordered by some sort of ranking method)
## and returns a list of rank placements for each of the rank position; 
## > rank_placement[[1]]  # first rank populated by the following values
## 'a', 'b', 'a', 'a', 'b', 'c', 'd'
## > rank_placement[[2]]  # second rank populated by the following values
## 'c', 'a', 'b', 'b', 'a', 'd', 'a'
get_rnk_population_lst <- function(rnkd_vec_lst) {
  
  ## initialize an empty rank placement list
  max_length <- max(unlist(lapply(rnkd_vec_lst, length)))
  rnk_population_lst <- vector(mode='list', length=max_length)  

  ## remove empty ranked vectors from the list
  rnkd_vec_lst <- rnkd_vec_lst[lapply(rnkd_vec_lst, length) > 0]
  
  ## loop through list of ranked vector
  for (rnkd_vec in rnkd_vec_lst) {
    
    ## un-factor ranked vector 
    rnkd_vec <- as.character(rnkd_vec)
    
    ## for each ranked vector, record its placement 
    for (i in 1:length(rnkd_vec)) {
      rnk_population_lst[[i]] <- c(rnk_population_lst[[i]], rnkd_vec[i])
    }
  }
  
  ## return
  return(rnk_population_lst)
}


## this function takes in predictive_df and a vector of predictors
## and returns a list of vectors, where the first element of the list
## contains predictors that performed best in each season, 
## second element contains predictors that performed second best in each season,
## and so forth and so on
get_pred_perf_rnk_plcmnt_lst <- function(predictive_df, 
                                         predictors, 
                                         rank_method=c('pred_acc', 'smry_stat_sprd'), 
                                         ...) {
  
  ## get list of ranked metrics
  rnkd_metrics_lst <- get_rnkd_metrics_lst(predictive_df, predictors, rank_method[1])
  
  ## remove empty ranked vectors from the list
  rnkd_metrics_lst <- rnkd_metrics_lst[lapply(rnkd_metrics_lst, length) > 0]

  ## get rank population list
  rnk_population_lst <- get_rnk_population_lst(rnkd_metrics_lst)
  
  ## return
  return(rnk_population_lst)
}


## this function creates a list of ranked metrics, which is 
## used to create prediction performance rank placement
get_rnkd_metrics_lst <- function(predictive_df, 
                                 predictors,
                                 rank_method=c('pred_acc', 'smry_stat_sprd'), 
                                 ...) {
  
  ## set rank method
  rank_method <- rank_method[1]
  
  ## split predictive df by season
  df_lst <- split(predictive_df, predictive_df$season)
  
  ## remove empty df from the list
  df_lst <- df_lst[lapply(df_lst, nrow) > 0]
  
  if (rank_method=='pred_acc') {
    
    ## create list of ranked metrics 
    rnkd_metrics_lst <- lapply(df_lst, function(x) {
      
      ## create sorted win pred acc df
      wpa_df <- create_win_pred_acc_df(predictive_df=x, metric_cols=predictors, ...)
      wpa_df <- sortByCol(wpa_df, col='acc', asc=FALSE)
      wpa_df <- subset(wpa_df, acc > 0.5)
      
      ## store ranked metrics as an element to the list
      as.character(wpa_df$metric)
    })
  }
  
  else if (rank_method=='smry_stat_sprd') {
    
    ## create list of ranked metrics 
    rnkd_metrics_lst <- lapply(df_lst, function(x) {

      ## split data by outcome
      won_df <- subset(x, won); lost_df <- subset(x, !won)
      
      ## create sorted variable importance df (measured by summary stats spread)
      varimp_df <- create_varimp_df(lost_df, won_df, 
                                    predictors=predictors, 
                                    normalize=TRUE)
      varimp_df <- sortByCol(varimp_df, col='spread', asc=FALSE)

      ## store ranked metrics as an element to the list
      as.character(varimp_df$var)
    })
  }
  
  else {
    stop('Please provide a valid ranking method to evaluate the metrics.')
  }

  ## return
  return(rnkd_metrics_lst)
}


