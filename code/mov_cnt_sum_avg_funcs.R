############ MOVING AVERAGE FUNCTIONS ################

## this function determines if given df is a single season-team df
is_sngl_ssn_tm_df <- function(df) {
  return(length(unique(df$season))==1 && length(unique(df$team))==1) 
}


## this function adjusts add_vars based on df type for recursion purpose
treat_agg_vars_for_recursion <- function(df, agg_vars) {

  ## if df contains only single team-season data
  if (is_sngl_ssn_tm_df(df)) 
    
    ## then keep agg_vars as-is
    agg_vars <- agg_vars
  
  ## if df contains multiple team-season data
  else
    
    ## then add team and season to agg_vars vector
    agg_vars <- unique(c('team', 'season', agg_vars))
  
  ## return
  return(agg_vars)
}


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
add_movavg_cols <- function(df, cols, 
                            type=c('SMA', 'EMA', 'cummean'), 
                            n=10, cover_less_than_n=TRUE, 
                            agg_vars=NULL, new_colnm_apnd_str='', 
                            rnd_dgt=3, add_opp_cols=FALSE) {
  
  ## set type
  type <- type[1]
  
  ## adjust add_vars based on df type for recursion
  agg_vars <- treat_agg_vars_for_recursion(df, agg_vars)

  ## base case
  if (is.null(agg_vars)) {
    
    ## get original column names
    orig_cols <- names(df)

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
        run_valsColNm <- paste0(col, '_', tolower(type), new_colnm_apnd_str)
        
        ## add MA vals to df as column
        df[[run_valsColNm]] <- run_vals
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
          run_valsColNm <- paste0(col, '_', tolower(type), i, new_colnm_apnd_str)
          
          ## add MA vals to df as column
          df[[run_valsColNm]] <- run_vals
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
    
    ## return
    return(df)
  }
  
  ## for each aggregation subset, apply function
  output_df <- ddply(df, agg_vars, function(x) {
    add_movavg_cols(df=x, cols=cols, type=type, n=n, cover_less_than_n=cover_less_than_n, 
                    agg_vars=NULL, new_colnm_apnd_str=new_colnm_apnd_str, rnd_dgt=rnd_dgt)
  })
  
  ## return
  return(output_df)
}


## this function add variable-specific win, loss, and game counts 
add_cum_cnt_cols <- function(df, cols=c('w', 'l', 'n'), 
                             agg_vars=NULL, 
                             new_colnm_apnd_str='',
                             add_opp_cols=FALSE) {
  
  ## adjust add_vars based on df type for recursion
  agg_vars <- treat_agg_vars_for_recursion(df, agg_vars)
  
  ## recursionn base case
  if (is.null(agg_vars)) {
    
    ## get original column names
    orig_cols <- names(df)

    ## order the base subsets
    df <- df[order(df$date), ]
    
    ## calculate nrow
    n <- nrow(df)
    
    ## for each column
    for (col in cols) {
      
      ## create new column name
      run_cnt_colnm <- paste0(col, '_', new_colnm_apnd_str)
      run_cnt_colnm <- gsub('__', '_', run_cnt_colnm)
      
      ## calculate running counts
      if (col=='w')
        run_cnts <- c(0, cumsum(df$won)[-n])
      else if (col=='l')
        run_cnts <- c(0, cumsum(!df$won)[-n])
      else if (col=='n')
        run_cnts <- seq(0, n-1)
      
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
    
    ## return
    return(df)
  }
  
  ## for each aggregation subset, apply function
  output_df <- ddply(df, agg_vars, function(x) {
    add_cum_cnt_cols(df=x, cols=cols, agg_vars=NULL, new_colnm_apnd_str=new_colnm_apnd_str)
  })
  
  ## return
  return(output_df)
}


## this function adds cumulative sum columns
add_cum_sum_cols <- function(df, cols, agg_vars=NULL, 
                             new_colnm_apnd_str='', 
                             rnd_dgt=3, add_opp_cols=FALSE) {
  
  ## adjust add_vars based on df type for recursion
  agg_vars <- treat_agg_vars_for_recursion(df, agg_vars)
  
  ## recursion base case
  if (is.null(agg_vars)) {
    
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
      run_valsColNm <- paste0(col, '_cumsum_', new_colnm_apnd_str)
      run_valsColNm <- gsub('__', '_', run_valsColNm)
      
      ## add MA vals to df as column
      df[[run_valsColNm]] <- run_vals
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
  }

  ## for each aggregation subset, apply function
  output_df <- ddply(df, agg_vars, function(x) {
    add_cum_sum_cols(df=x, cols=cols, agg_vars=NULL, new_colnm_apnd_str=new_colnm_apnd_str, rnd_dgt=rnd_dgt)
  })
  
  ## return
  return(output_df)
}





