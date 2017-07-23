
master_df <- subset(master, season %in% 2012:2015)


## create predictive df;
## (takes the difference between tm metric columns and 
## opp metric columns in the master df and saves the difference
## as columns in predictive df)
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))



## this function takes in a list of vectors and returns
get_pred_perf_rnk_plcmnt_lst <- function(predictive_df, predictor_vars, ...) {
  
  ## split predictive df by season
  df_lst <- split(predictive_df, predictive_df$season)
  
  ## remove empty df from the list
  df_lst <- df_lst[lapply(df_lst, nrow) > 0]
  
  ## create list of ranked metrics
  rnkd_metrics_lst <- lapply(df_lst, function(x) {
    
    ## create sorted win pred acc df
    wpa_df <- create_win_pred_acc_df(predictive_df=x, metric_cols=predictor_vars, ...)
    wpa_df <- sortByCol(wpa_df, col='acc', asc=FALSE)
    
    ## store ranked metrics as an element to the list
    as.character(wpa_df$metric)
  })
    
  ## get rank population list
  rnk_population_lst <- get_rnk_population_lst(rnkd_metrics_lst)
  
  ## return
  return(rnk_population_lst)
}

x <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=wpc_cols, min_n=5)
x
x <- get_pred_perf_rnk_plcmnt_lst(predictive_df, predictor_vars=cumperf_cols, min_n=5)
x
