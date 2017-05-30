############ OPTIMAL WEIGHTS FUNCTIONS ################

## this function calculates an average prediction error
calcAvgPredErr <- function(df, outcomeVar, projVar, wgts=NULL) {
  
  if (is.null(wgts)) {
    ret <- mean(abs(df[[outcomeVar]] - df[[projVar]]), na.rm=TRUE)    
  }
  
  else {
    wgtsMtrx <- matrix(wgts, ncol=1)
    indVarsMtrx <- as.matrix(df[ , indVars])
    proj <- as.numeric(indVarsMtrx %*% wgtsMtrx)
    err <- df[[outcomeVar]] - proj
    ret <- mean(abs(err), na.rm=TRUE)
  }
  
  return(ret)
}