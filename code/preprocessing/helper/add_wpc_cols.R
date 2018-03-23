## this function adds win percentage columns to master df
## this function adds varied-by-variable win percentage columns to master df
add_wpc_cols <- function(master_df, rnd_dgt=3, add_opp_cols=FALSE) {
  
  ## get original column names
  orig_cols <- names(master_df)
  
  ## get a vector of win count column names
  win_cnt_cols <- sort(colnames(master_df)[grepl('^w_cumcnt_', colnames(master_df))])
  
  ## get a vector of game count column names
  gm_cnt_cols <- sort(colnames(master_df)[grepl('^n_cumcnt_', colnames(master_df))])
  
  ## create a vector win percent column names
  win_pc_cols <- gsub('^w_cumcnt', 'wpc', win_cnt_cols)
  
  ## for each pair of w-and-n column names
  for (i in 1:length(win_cnt_cols)) {
    win_cnt_col <- win_cnt_cols[i]
    gm_cnt_col <- gm_cnt_cols[i]
    win_pc_col <- win_pc_cols[i]
    
    ## calculate win perc and add as a column
    master_df[win_pc_col] <- round(master_df[win_cnt_col] / master_df[gm_cnt_col], rnd_dgt)
  }
  
  ## replace NaN w/ NA
  master_df[is.nan.data.frame(master_df)] <- NA
  
  ## fill in opponent columns
  if (add_opp_cols) {
    
    ## get new columns created
    new_cols <- setdiff(colnames(master_df), orig_cols)
    
    ## create win percentage columns for opponent
    master_df <- fill_in_opp_cols(master_df, cols=new_cols)
  }
  
  ## return 
  return(master_df)
}
