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
    
    ## split df by team-season
    df_lst <- split(master_df, list(master_df$team, master_df$season))
    
    ## remove empty team-season df from the list
    df_lst <- df_lst[lapply(df_lst, nrow) > 0]
    
    ## add moving average columns for each team-season df
    df_lst <- lapply(df_lst, function(df) {
      
      ## get original column names
      orig_cols <- names(master_df)
      
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
      
      ## add opponent columns
      if (add_opp_cols) {
        
        ## get new columns created
        new_cols <- setdiff(colnames(df), orig_cols)
        
        ## create win percentage columns for opponent
        df <- fill_in_opp_cols(df, cols=new_cols)
      }
      
      ## store to list
      df
    })
    
    ## collapse list of dfs into df
    master_df <- do.call(rbind.data.frame, c(df_lst, stringsAsFactors=FALSE))
    
    ## return
    return(master_df)
  }
  
  
  ## for each aggregation variable
  for (var in vary_by) {
    
    ## add moving average cols
    master_df <- ddply(master_df, treat_varyby_vars(var), function(x) {
      add_movavg_cols(x, 
                      cols=cols, 
                      type=type, 
                      n=n, 
                      cover_less_than_n=cover_less_than_n, 
                      vary_by=NULL, 
                      new_colnm_apnd_str=var,
                      rnd_dgt=rnd_dgt,
                      add_opp_cols=add_opp_cols)
    })
  }
  
  ## return
  return(master_df)
}


## this function add variable-specific win, loss, and game counts 
add_cum_cnt_cols <- function(master_df, 
                             cols=c('w', 'l', 'n'), 
                             vary_by=NULL, 
                             new_colnm_apnd_str=NULL,
                             add_opp_cols=FALSE) {
  
  ## recursive base case (when no aggregation variable was given)
  if (is.null(vary_by)) {
    
    ## split df by team-season
    df_lst <- split(master_df, list(master_df$team, master_df$season))
    
    ## remove empty team-season df from the list
    df_lst <- df_lst[lapply(df_lst, nrow) > 0]
    
    ## add cumulative count columns for each team-season df
    df_lst <- lapply(df_lst, function(df) {
      
      ## get original column names
      orig_cols <- names(df)
      
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
      
      ## add opponent columns
      if (add_opp_cols) {
        
        ## get new columns created
        new_cols <- setdiff(colnames(df), orig_cols)
        
        ## create win percentage columns for opponent
        df <- fill_in_opp_cols(df, cols=new_cols)
      } 
      
      ## add to list
      df
    })
    
    ## collapse list of dfs into df
    master_df <- do.call(rbind.data.frame, c(df_lst, stringsAsFactors=FALSE))
    
    ## return
    return(master_df)
  }
  
  ## for each aggregation variable
  for (var in vary_by) {
    
    ## add cumulative count cols
    master_df <- ddply(master_df, treat_varyby_vars(var), function(x) {
      add_cum_cnt_cols(x, cols=cols, vary_by=NULL, new_colnm_apnd_str=var)
    })
  }
  
  ## return
  return(master_df)
}


## this function adds cumulative sum columns
add_cum_sum_cols <- function(master_df, 
                             cols, 
                             vary_by=NULL, 
                             new_colnm_apnd_str=NULL, 
                             rnd_dgt=3, 
                             add_opp_cols=FALSE) {

  ## recursion base case
  if (is.null(vary_by)) {
    
    ## split df by team-season
    df_lst <- split(master_df, list(master_df$team, master_df$season))
    
    ## remove empty team-season df from the list
    df_lst <- df_lst[lapply(df_lst, nrow) > 0]
    
    ## add moving average columns for each team-season df
    df_lst <- lapply(df_lst, function(df) {
      
      ## get original column names
      orig_cols <- names(df)
      
      ## order the base subsets
      df <- df[order(df$date), ]

      ## for each column
      for (col in cols) {
        
        ## calculate MA
        run_vals <- cumsum(df[[col]])
        
        ## offset (shift) MA vals by 1
        run_vals <- c(NA, run_vals[-length(run_vals)])
        
        ## round MA values
        run_vals <- round(run_vals, rnd_dgt)
        
        ## create new column name
        new_colnm <- create_new_cum_colnm(col, new_colnm_apnd_str, type='cumsum')
        
        ## add MA vals to df as column
        df[[new_colnm]] <- run_vals
      }
      
      ## add opponent columns
      if (add_opp_cols) {
        
        ## get new columns created
        new_cols <- setdiff(colnames(df), orig_cols)
        
        ## create win percentage columns for opponent
        df <- fill_in_opp_cols(df, cols=new_cols)
      }
      
      ## return
      return(df)
    })
   
    ## collapse list of dfs into df
    master_df <- do.call(rbind.data.frame, c(df_lst, stringsAsFactors=FALSE))
    
    ## return
    return(master_df)
  }
  
  ## for each aggregation variable
  for (var in vary_by) {
    
    ## add moving average cols
    master_df <- ddply(master_df, treat_varyby_vars(var), function(x) {
      add_cum_sum_cols(x, 
                      cols=cols, 
                      vary_by=NULL, 
                      new_colnm_apnd_str=var,
                      rnd_dgt=rnd_dgt,
                      add_opp_cols=add_opp_cols)
    })
  }
  
  ## return
  return(master_df)
}


## this function adds cumulative performance columns
add_cum_perf_cols <- function(master_df, 
                              metric=c('oeff', 'oeffA', 
                                       'FGP', 'FGPA', 
                                       'rqP', 'rqPA',
                                       'pos', 'posA'), 
                              vary_by=NULL,
                              new_colnm_apnd_str=NULL,
                              rnd_dgt=3,
                              add_opp_cols=FALSE) {
  
  ## recursion base case
  if (is.null(vary_by)) {

    ## get original column names
    orig_cols <- names(master_df)

    ## initialize an empty vector to store columns for cumulative sum calculations
    cumsum_cols <- c()
    
    ## iteratively add to cumsum_col vector by given metric
    for (m in metric) {
      if (m=='oeff') cumsum_cols <- c(cumsum_cols, 'p', 'pos')
      else if (m=='oeffA') cumsum_cols <- c(cumsum_cols, 'pA', 'posA')  
      else if (m=='FGP') cumsum_cols <- c(cumsum_cols, 'FGM', 'FGA')
      else if (m=='FGPA') cumsum_cols <- c(cumsum_cols, 'FGMA', 'FGAA')
      else if (m=='rqP') cumsum_cols <- c(cumsum_cols, 'rqP')
      else if (m=='rqPA') cumsum_cols <- c(cumsum_cols, 'rqPA')
      else if (m=='pos') cumsum_cols <- c(cumsum_cols, 'pos')
      else if (m=='posA') cumsum_cols <- c(cumsum_cols, 'posA')
    }
    
    ## remove duplicate cols
    cumsum_cols <- unique(cumsum_cols)

    ## add cum sum columns
    print('hola')
    print(cumsum_cols)
    master_df <- add_cum_sum_cols(master_df, 
                           cols=cumsum_cols,
                           vary_by=NULL,
                           new_colnm_apnd_str=new_colnm_apnd_str,
                           add_opp_cols=FALSE)
    print('tia')
    
    ## add cum cnt columns (if applicable)
    if (any(metric %in% c('rqP', 'rqPA', 'pos', 'posA'))) {
      master_df <- add_cum_cnt_cols(master_df, 
                                    cols='n', 
                                    vary_by=NULL, 
                                    new_colnm_apnd_str=new_colnm_apnd_str)
    }
    
    ## create a named vector of cumulative performance column names
    cumperf_colnms <- create_new_cum_colnm(col=metric, type='cumperf', new_colnm_apnd_str)
    names(cumperf_colnms) <- metric
    
    ## create a named vector of cumulative sum column names
    cumsum_colnms <- create_new_cum_colnm(col=cumsum_cols, type='cumsum', new_colnm_apnd_str)
    names(cumsum_colnms) <- cumsum_cols    
    
    ## create cumulative cnt column name
    n_cumcnt_colnm <- create_new_cum_colnm(col='n', type='cumcnt', new_colnm_apnd_str)

    ## offensive efficiency: points per possesion x100
    if ('oeff' %in% metric) {
      master_df[[cumperf_colnms['oeff']]] <- 
        master_df[[cumsum_colnms['p']]] / master_df[[cumsum_colnms['pos']]] * 100  
    } 
    
    ## opponent offensive efficiency: points per possession x100
    if ('oeffA' %in% metric) {
      master_df[[cumperf_colnms['oeffA']]] <- 
        master_df[[cumsum_colnms['pA']]] / master_df[[cumsum_colnms['posA']]] * 100
    } 
    
    ## field goal percentage
    if ('FGP' %in% metric) {
      master_df[[cumperf_colnms['FGP']]] <- 
        master_df[[cumsum_colnms['FGM']]] / master_df[[cumsum_colnms['FGA']]]
    }
    
    ## field goal percentage allowed
    if ('FGPA' %in% metric) {
      master_df[[cumperf_colnms['FGPA']]] <- 
        master_df[[cumsum_colnms['FGMA']]] / master_df[[cumsum_colnms['FGAA']]]
    }
    
    ## regular quarter points
    if ('rqP' %in% metric) {
      master_df[[cumperf_colnms['rqP']]] <- 
        master_df[[cumsum_colnms['rqP']]] / master_df[[n_cumcnt_colnm]]
    }
    
    ## regular quarter points allowed
    if ('rqPA' %in% metric) {
      master_df[[cumperf_colnms['rqPA']]] <- 
        master_df[[cumsum_colnms['rqPA']]] / master_df[[n_cumcnt_colnm]]
    }

    ## possessions
    if ('pos' %in% metric) {
      master_df[[cumperf_colnms['pos']]] <- 
        master_df[[cumsum_colnms['pos']]] / master_df[[n_cumcnt_colnm]]
    }
    
    if ('posA' %in% metric) {
      master_df[[cumperf_colnms['posA']]] <- 
        master_df[[cumsum_colnms['posA']]] / master_df[[n_cumcnt_colnm]]
    }

    ## remove intermediary columns
    # master_df <- rm_colnms_by_regex_mtch(master_df, regex_expr='cumsum')
    # master_df <- rm_colnms_by_regex_mtch(master_df, regex_expr='cumcnt')
    master_df <- master_df[ , c(orig_cols, cumperf_colnms)]
    
    ## round digits
    master_df <- round_df(master_df, rnd_dgt)
    
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
  
  ## for each aggregation variable
  for (var in vary_by) {
    
    ## add cumulative perf cols
    master_df <- ddply(master_df, treat_varyby_vars(var), function(x) {
      add_cum_perf_cols(x, 
                        metric=metric, 
                        vary_by=NULL, 
                        new_colnm_apnd_str=var,
                        rnd_dgt=rnd_dgt, 
                        add_opp_cols=add_opp_cols)
    })
  }
  
  ## return
  return(master_df)
}




