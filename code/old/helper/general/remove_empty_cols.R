## this function removes empty columns
remove_empty_cols <- function(df) {
  empty_cols <- list_empty_cols(df)
  return(df[ , setdiff(names(df), empty_cols)])
}

