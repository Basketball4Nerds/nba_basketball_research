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
  new_colnm_apnd_str <- ifelse(is.null(new_colnm_apnd_str), 'gen', new_colnm_apnd_str)
  
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
                             new_colnm_apnd_str='', 
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
## WORK ON THIS FUNCTION HERE!!!!
add_cum_perf_cols <- function(df, 
                              metric=c('oeff', 'oeffA', 
                                       'FGP', 'FGPA', 
                                       'rqP', 'rqPA',
                                       'pos', 'posA'), 
                              vary_by=NULL,
                              rnd_dgt=3,
                              add_opp_cols=FALSE) {
  
  ## recursion base case
  if (is.null(vary_by)) {
    
    ## get original column names
    orig_cols <- names(df)
    
    ## order the base subsets
    df <- df[order(df$date), ]
    
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
    }
    
    ## add cumulative sum columns
    df <- add_cum_sum_cols(df, 
                           cols=cum_cols,
                           agg_vars=NULL,
                           new_colnm_apnd_str='gen',
                           add_opp_cols=FALSE)
    
    ## general offensive efficiency: points per possesion x100
    if ('oeff' %in% metric) {
      df$oeff_cum_gen <- df$p_cumsum_gen / df$pos_cumsum_gen * 100    
    } 
    
    ## general opponent offensive efficiency: points per possession x100
    if ('oeffA' %in% metric) {
      df$oeffA_cum_gen <- df$pA_cumsum_gen / df$posA_cumsum_gen * 100
    } 
    
    ## general field goal percentage
    if ('FGP' %in% metric) {
      df$FGP_cum_gen <- df$FGM_cumsum_gen / df$FGA_cumsum_gen
    }
    
    ## general field goal percentage allowed
    if ('FGPA' %in% metric) {
      df$FGPA_cum_gen <- df$FGMA_cumsum_gen / df$FGAA_cumsum_gen
    }
    
    ## general regular quarter points
    if ('rqP' %in% metric) {
      df$rqP_cum_gen <- df$rqP_cumsum_gen / df$n
    }
    
    ## general regular quarter points allowed
    if ('rqPA' %in% metric) {
      df$rqPA_cum_gen <- df$rqPA_cumsum_gen / df$n
    }
    
    ## remove intermediary "cumsum" columns
    df <- rm_colnms_by_regex_mtch(df, regex_expr='cumsum')
    
    ## round digits
    df <- round_df(df, rnd_dgt)
    
    ## fill in opponent columns
    if (add_opp_cols) {
      
      ## get new columns created
      new_cols <- setdiff(colnames(df), orig_cols)
      
      ## create win percentage columns for opponent
      df <- fill_in_opp_cols(df, cols=new_cols)
    }
    
    ## return
    return(df)
  }
  
  ## for each aggregation subset, apply function
  output_df <- ddply(df, vary_by, function(x) {
    add_cum_gen_perf_cols(df=x, metric=metric, vary_by=NULL, rnd_dgt=rnd_dgt, add_opp_cols=add_opp_cols)
  })
  
  ## return
  return(output_df)
}
