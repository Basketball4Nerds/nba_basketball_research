############ MOVING AVERAGE FUNCTIONS ################

## this function adjusts add_vars based on df type for recursion purpose
treat_agg_vars_for_recursion <- function(df, agg_vars) {

  ## keep agg_vars as-is if df consists of only one team, one season
  if (length(unique(df$season))==1 && length(unique(df$team))==1) 
    agg_vars <- agg_vars
  
  ## add team and season to agg_vars vector if df consists of multiple team-season's  
  else
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
    addMaCols(df=x, cols=cols, type=type, n=n, cover_less_than_n=cover_less_than_n, 
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




## this function adds cumulative general performance columns based on given metrics
add_cum_gen_perf_cols <- function(df, 
                                  metric=c('oeff', 'oeffA', 
                                           'FGP', 'FGPA', 
                                           'rqP', 'rqPA'), 
                                  agg_vars=NULL,
                                  rnd_dgt=3,
                                  add_opp_cols=FALSE) {
  
  ## adjust add_vars based on df type for recursion
  agg_vars <- treat_agg_vars_for_recursion(df, agg_vars)

  ## recursion base case
  if (is.null(agg_vars)) {
    
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
    df <- add_cum_sum_cols(df, cols=cum_cols,
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
    # df <- rm_colnms_by_regex_mtch(df, regex_expr='cumsum')
    
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
  output_df <- ddply(df, agg_vars, function(x) {
    add_cum_gen_perf_cols(df=x, metric=metric, agg_vars=NULL, rnd_dgt=rnd_dgt, add_opp_cols=add_opp_cols)
  })
  
  ## return
  return(output_df)
}
