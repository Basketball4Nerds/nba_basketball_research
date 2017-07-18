
## subset df
master_df <- subset(master, season==2012)

kakao <- create_kakao(master_df)
kakao <- sortByCol(kakao, c('team', 'date'))
tail(kakao)
create_kakao <- function(master_df) {

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
  ret_df <- master_df[ , c(base_cols, dep_col, 'line')]

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


tail(master_df$rqPA_cumperf_gen)
tail(master_df$rqPA_cumperf_oeffaQntlRnk)



## this function 

x <- master_df[ , 1:120]
head(x)

y <- add_cum_perf_cols(x,
                       metric=c('rqP', 'rqPA'))
head(y)







