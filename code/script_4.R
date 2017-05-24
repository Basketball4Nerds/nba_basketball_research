

createSimpleRetroWinPredAccDf <- function(df, cols) {
  
  ## initialize an empty list
  lst <- list()
  
  ## calculate retrospective win prediction strength (RWPS) for each metric
  for (col in cols) {
    
    ## construct o_col (opponent's metric) based on a given col
    if (grepl('Fcd', col)) {
      o_col <- gsub('Fcd', '', col)      
    } 
    else if (grepl('[A-Za-z]_p_[A-Za-z]', col)) {
      o_col <- unlist(strsplit(col, split='_p_'))
      o_col <- paste0(o_col, 'A')
      o_col <- paste0(o_col, collapse='_p_')
    } 
    else {
      o_col <- paste0(col, 'A')      
    }
    
    ## if o_col is not found in dataset, skip to the next metric
    if (!(o_col %in% names(df))) {
      print(paste('Unable to locate the following metric:', col))
      next
    }
    
    ## make a simple retrospective prediction 
    pred <- df[ , col] > df[ , o_col]
    
    ## create confusion matrix
    cnfMtx <- table(df$won, pred)
    
    ## calculate prediction accuracy
    acc <- calcAccFrConfMtx(cnfMtx)
    
    ## calculate the number of data points to calculate retro pred acc
    nDp <- sum(cnfMtx)
    
    ## create a list element
    lstElm <- c(col, acc, nDp)
    
    ## append list element to list
    lst <- c(lst, list(lstElm))
    
  }
  
  ## collapse list into df
  retDf <- do.call(rbind.data.frame, lst)
  
  ## set colnames for df
  names(retDf) <- c('metric', 'SRWPS', 'nDp')
  
  ## set proper data type 
  retDf$SRWPS <- as.numeric(as.character(retDf$SRWPS))
  retDf$nDp <- as.integer(as.character(retDf$nDp))
  
  ## return
  return(retDf)
}
