#head(master$qtrpts)
## this function adds pts columns for each quarter and overtime
addQtrOtPtsCols <- function(df, qtrPtsCols) {
  qtrPtsCols <- sort(qtrPtsCols)
  qtrPtsDf <- createQtrOtPtsDf(df, qtrPtsCols[1])
  oQtrPtsDf <- createQtrOtPtsDf(df, qtrPtsCols[2])
  oQtrPtsDf$nOT <- NULL
  names(oQtrPtsDf) <- paste0(names(oQtrPtsDf), 'A')
  df <- cbind(df, qtrPtsDf, oQtrPtsDf)
  return(df)
}
