
## this function add variable-specific win, loss, and game counts 
add_cumcnt_cols <- function(master_df, 
                            cols=c('w', 'l', 'n'), 
                            vary_by=NULL, 
                            new_colnm_apnd_str=NULL,
                            add_opp_cols=FALSE) {
  
  ## recursive base case (when no aggregation variable was given)
  if (is.null(vary_by)) {
    
    ## original cols
    orig_cols <- names(master_df)
    
    ## split df by team-season
    df_lst <- split(master_df, list(master_df$team, master_df$season))
    
    ## remove empty team-season df from the list
    df_lst <- df_lst[lapply(df_lst, nrow) > 0]
    
    ## add cumulative count columns for each team-season df
    df_lst <- lapply(df_lst, function(df) {
      
      ## order the base subsets
      df <- df[order(df$date), ]
      
      ## calculate nrow
      n <- nrow(df)
      
      ## for each column
      for (col in cols) {
        
        ## calculate running counts
        if (col=='w')
          run_cnts <- c(0, cumsum(df$won)[-n])
        else if (col=='l')
          run_cnts <- c(0, cumsum(!df$won)[-n])
        else if (col=='n')
          run_cnts <- seq(0, n-1)
        
        ## create new col name
        new_colnm <- create_new_cum_colnm(col, new_colnm_apnd_str, type='cumcnt')
        
        ## add run count as a column
        df[[new_colnm]] <- run_cnts
      }
      
      ## add to list
      df
    })
    
    ## collapse list of dfs into df
    master_df <- do.call(rbind.data.frame, c(df_lst, stringsAsFactors=FALSE))
    
    ## proper row names
    rownames(master_df) <- 1:nrow(master_df)
    
    ## add opponent columns
    if (add_opp_cols) { 
      new_cols <- setdiff(names(master_df), orig_cols)
      master_df <- fill_in_opp_cols(master_df, cols=new_cols) 
    } 
    
    ## return
    return(master_df)
  }
  
  ## orig cols
  orig_cols <- names(master_df)
  
  ## for each aggregation variable
  for (var in vary_by) {
    
    ## add cumulative count cols
    master_df <- ddply(master_df, assure_correct_varyby_vars(var), function(x) {
      add_cumcnt_cols(x, cols=cols, vary_by=NULL, new_colnm_apnd_str=var, add_opp_cols=FALSE)
    })
  }
  
  ## fill in opponent columns
  if (add_opp_cols) { 
    new_cols <- setdiff(names(master_df), orig_cols)
    master_df <- fill_in_opp_cols(master_df, cols=new_cols) 
  }
  
  ## return
  return(master_df)
}

