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
