## this function adds new columns for moving averages 
## (e.g. simple moving averages or running standard deviations)
add_movavg_cols <- function(master_df, 
                            cols, 
                            type=c('sma', 'ema', 'cummean'), 
                            n=10, 
                            cover_less_than_n=TRUE, 
                            vary_by=NULL, 
                            new_colnm_apnd_str=NULL, 
                            rnd_dgt=3, 
                            add_opp_cols=FALSE) {
  
  ## recursive base case
  if (is.null(vary_by)) {
    
    ## original cols
    orig_cols <- names(master_df)
    
    ## split df by team-season
    df_lst <- split(master_df, list(master_df$team, master_df$season))
    
    ## remove empty team-season df from the list
    df_lst <- df_lst[lapply(df_lst, nrow) > 0]
    
    ## add moving average columns for each team-season df
    df_lst <- lapply(df_lst, function(df) {
      
      ## set type
      type <- type[1]
      
      ## order the base subsets
      df <- df[order(df$date), ]
      
      ## for each column
      for (col in cols) {
        
        ## for cummulative mean (up-to-date moving average)
        if (type=='cummean') {
          
          ## calculate MA
          run_vals <- calc_movavg_vals(vals=df[[col]], type=type)
          
          ## offset (shift) MA vals by 1
          run_vals <- c(NA, run_vals[-length(run_vals)])
          
          ## round MA values
          run_vals <- round(run_vals, rnd_dgt)
          
          ## create new column name
          new_colnm <- create_new_cum_colnm(col, new_colnm_apnd_str, type)
          
          ## add MA vals to df as column
          df[[new_colnm]] <- run_vals
        }
        
        ## for SMA and EMA
        else {
          
          ## in case n is a vector of integers
          for (i in n) {
            
            ## calculate MA
            run_vals <- calc_movavg_vals(vals=df[[col]], n=i, cover_less_than_n=cover_less_than_n)
            
            ## offset (shift) MA vals by 1
            run_vals <- c(NA, run_vals[-length(run_vals)])
            
            ## round MA values
            run_vals <- round(run_vals, rnd_dgt)
            
            ## create new column name
            new_colnm <- create_new_cum_colnm(col, new_colnm_apnd_str, type, i)
            
            ## add MA vals to df as column
            df[[new_colnm]] <- run_vals
          }
        }
      }
      
      ## store to list
      df
    })
    
    ## collapse list of dfs into df
    master_df <- do.call(rbind.data.frame, c(df_lst, stringsAsFactors=FALSE))
    
    ## fill in opponent columns
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
    
    ## add moving average cols
    master_df <- ddply(master_df, assure_correct_varyby_vars(var), function(x) {
      add_movavg_cols(x, 
                      cols=cols, 
                      type=type, 
                      n=n, 
                      cover_less_than_n=cover_less_than_n, 
                      vary_by=NULL, 
                      new_colnm_apnd_str=var,
                      rnd_dgt=rnd_dgt,
                      add_opp_cols=FALSE)
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

