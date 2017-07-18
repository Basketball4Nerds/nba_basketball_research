############ MOVING AVERAGE FUNCTIONS ################

## this function calculates moving averages
calc_movavg_vals <- function(vals, 
                             type=c('SMA', 'EMA', 'cummean'), 
                             n=NULL, 
                             cover_less_than_n=TRUE) {
  
  ## specify moving average type
  type <- type[1]
  
  ## make sure there is no NAs in the vals
  ## (moving average function will break)
  if (any(is.na(vals))) 
    stop('There is NA in numeric values provided. Cannot compute moving average.')
  
  ## get length of given vector
  len <- length(vals)
  
  ## if length of vector is less than 1
  if (len==0) stop('Please provide vector of length greater than 0.')
  
  ## if length of vector is equal to 1, 
  ## then there is nothing to overage over; return NA
  if (len==1) return(NA)
  
  ## return cummulative mean if cummean method selected
  if (type=='cummean') { return(cummean(vals)) }
  
  ## the rest of the function deals with for SMA and EMA
  
  ## regular moving average;
  ## doesn't provide moving average values for indices less than n;
  ## e.g. if n=5, no moving average values from 1st through 4th
  if (!cover_less_than_n) {
    
    ## if there are less numbers in the vector than n
    ## to average over, then moving average is a vector of NAs
    if (len < n) { run_vals <- rep(NA, len) } 
    
    ## calculate MAs under normal condition
    else { run_vals <- do.call(type, args=list(x=vals, n=n)) }
  }
  
  ## modified moving average;
  ## if n is larger than length of vectors to cover,
  ## then n adjusts to "fit" the vector and cover;
  ## e.g. if n=5, then:
  ## 2nd MA val will be from 1st and 2nd vals,
  ## 3rd MA val will be from 1st, 2nd, 3rd vals,
  ## 4th MA val will be from 1st, 2nd, 3rd, 4th vals,
  ## 5th MA val will be from 1st, 2nd, 3rd, 4th, 5th vals,
  ## 6th MA val will be from 2nd, 3rd, 4th, 5th, 6th vals
  else {
    
    ## if there are less numbers in the vector than n to average over,
    ## then shorten n to fit the length of the vector
    if (len <= n) { n <- len }
    
    ## first calculate MA normally
    run_vals <- do.call(type, args=list(x=vals, n=n))
    
    ## then go back and fill in i < n entries
    run_vals[1] <- vals[1]
    for (i in 2:(n-1)) {
      args <- list(x=vals[1:i], n=i)
      run_val <- do.call(type, args=args)[i]
      run_vals[i] <- run_val
    }
  }
  
  ## return  
  return(run_vals)
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
    master_df <- ddply(master_df, assure_correct_varyby_vars(var), function(x) {
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
    master_df <- ddply(master_df, assure_correct_varyby_vars(var), function(x) {
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
    master_df <- ddply(master_df, assure_correct_varyby_vars(var), function(x) {
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
  
  
  ## get original column names
  orig_cols <- names(master_df)
  
  ## initialize an empty vector to store columns for cumulative sum calculations
  cols_to_cumsum <- c()

  ## get a vector cols for cumulative sum calculation
  if ('oeff' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'p', 'pos')
  if ('oeffA' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'pA', 'posA')  
  if ('FGP' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'FGM', 'FGA')
  if ('FGPA' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'FGMA', 'FGAA')
  if ('rqP' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'rqP')
  if ('rqPA' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'rqPA')
  if ('pos' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'pos')
  if ('posA' %in% metric) cols_to_cumsum <- c(cols_to_cumsum, 'posA')
  
  ## remove duplicate cols
  cols_to_cumsum <- unique(cols_to_cumsum)
  
  ## add cum sum columns
  master_df <- add_cum_sum_cols(master_df,
                                cols=cols_to_cumsum,
                                vary_by=vary_by,
                                new_colnm_apnd_str=new_colnm_apnd_str,
                                add_opp_cols=FALSE)
  
  ## for the metrics that require gm-cnt cols
  if (any(c('rqP', 'rqPA', 'pos', 'posA') %in% metric)) {
    
    ## add cum cnt columns
    master_df <- add_cum_cnt_cols(master_df, 
                                  cols='n', 
                                  vary_by=vary_by, 
                                  new_colnm_apnd_str=new_colnm_apnd_str,
                                  add_opp_cols=FALSE)
  }
  
  ## get cumsum columns required for oeff calculations
  p_cumsum_cols <- sort(colnames(master_df)[grepl('^p_cumsum_', colnames(master_df))])
  pA_cumsum_cols <- sort(colnames(master_df)[grepl('^pA_cumsum_', colnames(master_df))])
  pos_cumsum_cols <- sort(colnames(master_df)[grepl('^pos_cumsum_', colnames(master_df))])
  posA_cumsum_cols <- sort(colnames(master_df)[grepl('^posA_cumsum_', colnames(master_df))])
  FGM_cumsum_cols <- sort(colnames(master_df)[grepl('^FGM_cumsum_', colnames(master_df))])
  FGMA_cumsum_cols <- sort(colnames(master_df)[grepl('^FGMA_cumsum_', colnames(master_df))])
  FGA_cumsum_cols <- sort(colnames(master_df)[grepl('^FGA_cumsum_', colnames(master_df))])
  FGAA_cumsum_cols <- sort(colnames(master_df)[grepl('^FGAA_cumsum_', colnames(master_df))])
  rqP_cumsum_cols <- sort(colnames(master_df)[grepl('^rqP_cumsum_', colnames(master_df))])
  rqPA_cumsum_cols <- sort(colnames(master_df)[grepl('^rqPA_cumsum_', colnames(master_df))])
  
  ## get a vector of gm-cnt columns
  gm_cnt_cols <- sort(colnames(master_df)[grepl('^n_', colnames(master_df))])

  ## create cumperf column names
  oeff_cumperf_cols <- gsub('^p_cumsum', 'oeff_cumperf', p_cumsum_cols)
  oeffA_cumperf_cols <- gsub('^pA_cumsum', 'oeffA_cumperf', pA_cumsum_cols)
  FGP_cumperf_cols <- gsub('^FGM_cumsum', 'FGP_cumperf', FGM_cumsum_cols)
  FGPA_cumperf_cols <- gsub('^FGMA_cumsum', 'FGPA_cumperf', FGMA_cumsum_cols)
  rqP_cumperf_cols <- gsub('^rqP_cumsum', 'rqP_cumperf', rqP_cumsum_cols)
  rqPA_cumperf_cols <- gsub('^rqPA_cumsum', 'rqPA_cumperf', rqPA_cumsum_cols)
  pos_cumperf_cols <- gsub('^pos_cumsum', 'pos_cumperf', pos_cumsum_cols)
  posA_cumperf_cols <- gsub('^posA_cumsum', 'posA_cumperf', posA_cumsum_cols)
  
  head(master)
  ## offensive efficiency: points per possesion x100
  if ('oeff' %in% metric) {
    for (i in 1:length(oeff_cumperf_cols)) {
      master_df[[oeff_cumperf_cols[i]]] <- (master_df[[p_cumsum_cols[i]]] / master_df[[pos_cumsum_cols[i]]]) * 100
    }
  } 
  
  ## opponent offensive efficiency: points per possession x100
  if ('oeffA' %in% metric) {
    for (i in 1:length(oeffA_cumperf_cols)) {
      master_df[[oeffA_cumperf_cols[i]]] <- (master_df[[pA_cumsum_cols[i]]] / master_df[[posA_cumsum_cols[i]]]) * 100
    }
  } 
  
  ## field goal percentage
  if ('FGP' %in% metric) {
    for (i in 1:length(FGP_cumperf_cols)) {
      master_df[[FGP_cumperf_cols[i]]] <- round(master_df[[FGM_cumsum_cols[i]]] / master_df[[FGA_cumsum_cols[i]]], rnd_dgt)
    }
  }
  
  ## field goal percentage allowed
  if ('FGPA' %in% metric) {
    for (i in 1:length(FGPA_cumperf_cols)) {
      master_df[[FGPA_cumperf_cols[i]]] <- round(master_df[[FGMA_cumsum_cols[i]]] / master_df[[FGAA_cumsum_cols[i]]], rnd_dgt)
    }
  }
  
  ## regular quarter points
  if ('rqP' %in% metric) {
    for (i in 1:length(rqP_cumperf_cols)) {
      master_df[[rqP_cumperf_cols[i]]] <- round(master_df[[rqP_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
    }
  }
  
  ## regular quarter points allowed
  if ('rqPA' %in% metric) {
    for (i in 1:length(rqPA_cumperf_cols)) {
      master_df[[rqPA_cumperf_cols[i]]] <- round(master_df[[rqPA_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
    }
  }
  
  ## possessions
  if ('pos' %in% metric) {
    for (i in 1:length(pos_cumperf_cols)) {
      master_df[[pos_cumperf_cols[i]]] <- round(master_df[[pos_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
    }
  }
  
  ## possessions allowed
  if ('posA' %in% metric) {
    for (i in 1:length(posA_cumperf_cols)) {
      master_df[[posA_cumperf_cols[i]]] <- round(master_df[[posA_cumsum_cols[i]]] / master_df[[gm_cnt_cols[i]]], rnd_dgt)
    }
  }
  
  ## remove intermediary columns (remove cumcnt and cumsum columns)
  cumperf_cols <- colnames(master_df)[grepl('_cumperf_', colnames(master_df))]
  master_df <- master_df[ , unique(c(orig_cols, cumperf_cols))]
  
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

