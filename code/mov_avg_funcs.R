############ MOVING AVERAGE FUNCTIONS ################

## this function calculates moving averages
calcMovAvgVals <- function(vals, type=c('SMA', 'EMA', 'cummean'), n=NULL, coverLessThanN=TRUE) {
  
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
  if (!coverLessThanN) {
    
    ## if there are less numbers in the vector than n
    ## to average over, then moving average is a vector of NAs
    if (len < n) { runVals <- rep(NA, len) } 
    
    ## calculate MAs under normal condition
    else { runVals <- do.call(type, args=list(x=vals, n=n)) }
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
    runVals <- do.call(type, args=list(x=vals, n=n))
    
    ## then go back and fill in i < n entries
    runVals[1] <- vals[1]
    for (i in 2:(n-1)) {
      args <- list(x=vals[1:i], n=i)
      runVal <- do.call(type, args=args)[i]
      runVals[i] <- runVal
    }
  }
  
  ## return  
  return(runVals)
}


## this function adds new columns for moving averages
## (e.g. simple moving averages or running standard deviations)
addMaCols <- function(df, cols, type=c('SMA', 'EMA', 'cummean'), n=10, 
                      coverLessThanN=TRUE, aggVars=NULL, colApndStr='', rndDgt=3) {
  
  ## set type
  type <- type[1]
  
  ## base case
  if (is.null(aggVars)) {
    
    ## order the base subsets
    df <- df[order(df$date), ]
    
    ## for each column
    for (col in cols) {
      
      ## for cummulative mean (up-to-date moving average)
      if (type=='cummean') {
        
        ## calculate MA
        runVals <- calcMovAvgVals(vals=df[[col]], type=type)
        
        ## offset (shift) MA vals by 1
        runVals <- c(NA, runVals[-length(runVals)])
        
        ## round MA values
        runVals <- round(runVals, rndDgt)
        
        ## create new column name
        runValsColNm <- paste0(col, '_', tolower(type), colApndStr)
        
        ## add MA vals to df as column
        df[[runValsColNm]] <- runVals
      }
      
      ## for SMA and EMA
      else {
        
        ## in case n is a vector of integers
        for (i in n) {
          
          ## calculate MA
          runVals <- calcMovAvgVals(vals=df[[col]], n=i, coverLessThanN=coverLessThanN)
          
          ## offset (shift) MA vals by 1
          runVals <- c(NA, runVals[-length(runVals)])
          
          ## round MA values
          runVals <- round(runVals, rndDgt)
          
          ## create new column name
          runValsColNm <- paste0(col, '_', tolower(type), i, colApndStr)
          
          ## add MA vals to df as column
          df[[runValsColNm]] <- runVals
        }
      }
    }
    
    ## return
    return(df)
  }
  
  df <- sortByCol(df, aggVars, asc=TRUE)
  
  ## for each aggregation subset, apply ad
  outputDF <- ddply(df, aggVars, function(x) {
    addMaCols(df=x, cols=cols, type=type, n=n, coverLessThanN=coverLessThanN, 
              aggVars=NULL, colApndStr=colApndStr, rndDgt=rndDgt)
  })
  
  ## return
  return(outputDF)
}





## this function 
add_cum_cnt_cols <- function(df, cols=c('w', 'l', 'n'), 
                       agg_vars=c('team', 'season'), 
                       new_colnm_apnd_str='') {
  
  ## recursionn base case
  if (is.null(agg_vars)) {
    
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
    
    ## return
    return(df)
  }

  ## for each aggregation subset, apply ad
  output_df <- ddply(df, agg_vars, function(x) {
    add_cum_cnt_cols(df=x, cols=cols, agg_vars=NULL, new_colnm_apnd_str=new_colnm_apnd_str)
  })
  
  ## return
  return(output_df)
  
}


## this function adds cumulative sum columns
addCumSumCols <- function(df, cols, agg_vars=c('team', 'season'), 
                          new_colnm_apnd_str='', rnd_dgt=3) {
  
  ## recursionn base case
  if (is.null(agg_vars)) {
    
    ## order the base subsets
    df <- df[order(df$date), ]
    
    ## for each column
    for (col in cols) {
      
      ## calculate MA
      runVals <- cumsum(df[[col]])

      ## offset (shift) MA vals by 1
      runVals <- c(NA, runVals[-length(runVals)])
      
      ## round MA values
      runVals <- round(runVals, rnd_dgt)
      
      ## create new column name
      runValsColNm <- paste0(col, '_cumsum_', new_colnm_apnd_str)
      runValsColNm <- gsub('__', '_', runValsColNm)
      
      ## add MA vals to df as column
      df[[runValsColNm]] <- runVals
    }
    
    ## return
    return(df)
    
  }

  df <- sortByCol(df, agg_vars, asc=TRUE)
  
  ## for each aggregation subset, apply ad
  output_df <- ddply(df, agg_vars, function(x) {
    addCumSumCols(df=x, cols=cols, agg_vars=NULL, new_colnm_apnd_str=new_colnm_apnd_str, rnd_dgt=rnd_dgt)
  })
  
  ## return
  return(output_df)
}