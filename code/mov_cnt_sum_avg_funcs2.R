## this function tweaks vary-by variables for proper aggregation
treat_varyby_vars <- function(vary_by) {
  
  ## remove 'o_' prefices if exist
  vary_by <- gsub('^o_', '', vary_by)
  
  ## add 'o_' to each vary-by variable except for site
  vary_by <- ifelse(vary_by=='site', vary_by, paste0('o_', vary_by))
  
  ## return
  vary_by
}


## this function creates new column name for various moving cnt, sum, avg functions
create_new_cum_col_nm <- function(col, 
                                  new_colnm_apnd_str, 
                                  type=c('cumcnt', 'cumsum', 'cummean', 'sma', 'ema'),
                                  n=NULL) {
  
  ## set type
  type <- tolower(type[1])
  
  ## replace new column name append string with 'gen' if nothing was specified
  new_colnm_apnd_str <- ifelse(is.null(new_colnm_apnd_str) || new_colnm_apnd_str=='',
                               'gen', new_colnm_apnd_str)
  
  ## case for cum count
  if (type=='cumcnt') {
    new_colnm <- paste0(col, '_', new_colnm_apnd_str)
  } 
  
  ## case for cum sum or cum mean
  else if (type %in% c('cumsum', 'cummean')) {
    new_colnm <- paste0(col, '_', type, '_', new_colnm_apnd_str) 
  } 
  
  ## case for moving average (with n)
  else if (type %in% c('sma', 'ema')) { 
    new_colnm <- paste0(col, '_', tolower(type), n, '_', new_colnm_apnd_str) 
  }
  
  ## error case
  else {
    stop('Wrong type given to create new column name.') 
  }

  ## replace double underscores to one if exist
  new_colnm <- gsub('__', '_', new_colnm)
  
  ## return 
  return(new_colnm)
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
      new_colnm <- create_new_cum_col_nm(col, new_colnm_apnd_str, type='cumcnt')
        
        ## add run count as a column
        df[[run_cnt_colnm]] <- run_cnts
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
          new_colnm <- create_new_cum_col_nm(col, new_colnm_apnd_str, type)
          
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
            new_colnm <- create_new_cum_col_nm(col, new_colnm_apnd_str, type, i)

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
        new_colnm <- create_new_cum_col_nm(col, new_colnm_apnd_str, type='cumsum')
        
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
    
    ## initialize empty vector to contain columns for cumulative sum calculations
    cum_cols <- c()
    
    ## iteratively add to cum_col vector by given metric
    for (m in metric) {
      if (m=='oeff') cum_cols <- c(cum_cols, 'p', 'pos')
      else if (m=='oeffA') cum_cols <- c(cum_cols, 'pA', 'posA')  
      else if (m=='FGP') cum_cols <- c(cum_cols, 'FGM', 'FGA')
      else if (m=='FGPA') cum_cols <- c(cum_cols, 'FGMA', 'FGAA')
      else if (m=='rqP') cum_cols <- c(cum_cols, 'rqP')
      else if (m=='rqPA') cum_cols <- c(cum_cols, 'rqPA')
      else if (m=='pos') cum_cols <- c(cum_cols, 'pos')
      else if (m=='posA') cum_cols <- c(cum_cols, 'posA')
    }
    
    ## remove duplicate cols
    cum_cols <- unique(cum_cols)
    
    ## add cumulative sum columns
    master_df <- add_cum_sum_cols(master_df, 
                           cols=cum_cols,
                           vary_by=NULL,
                           new_colnm_apnd_str=new_colnm_apnd_str,
                           add_opp_cols=FALSE)
    
    ## create a vector of cumulative performance column names
    cumperf_colnms <- paste0(metric, '_cumperf_', new_colnm_apnd_str)
    names(cumperf_colnms) <- metric

    ## create a vector of cumsum metric names required to calculate cumperf metrics
    cumsum_target_metrics <- c('p', 'pA', 
                             'pos', 'posA', 
                             'FGM', 'FGMA', 
                             'FGA', 'FGAA', 
                             'rqP', 'rqPA', 
                             'n')
    cumsum_colnms <- paste0(cumsum_target_metrics, '_cumsum_', new_colnm_apnd_str)
    cumsum_colnms <- gsub('n_cumsum_', 'n_', cumsum_colnms)
    cumsum_colnms <- gsub('n_gen', 'n', cumsum_colnms)

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
        master_df[[cumsum_colnms['rqP']]] / master_df[[cumsum_colnms['n']]]
    }
    
    ## regular quarter points allowed
    if ('rqPA' %in% metric) {
      master_df[[cumperf_colnms['rqPA']]] <- 
        master_df[[cumsum_colnms['rqPA']]] / master_df[[cumsum_colnms['n']]]
    }

    ## possessions
    if ('pos' %in% metric) {
      master_df[[cumperf_colnms['pos']]] <- 
        master_df[[cumsum_colnms['pos']]] / master_df[[cumsum_colnms['n']]]
    }
    
    if ('posA' %in% metric) {
      master_df[[cumperf_colnms['posA']]] <- 
        master_df[[cumsum_colnms['posA']]] / master_df[[cumsum_colnms['n']]]
    }

    ## remove intermediary "cumsum" columns
    master_df <- rm_colnms_by_regex_mtch(master_df, regex_expr='cumsum')
    
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







