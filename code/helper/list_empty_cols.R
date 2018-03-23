## this function returns names of empty columns
list_empty_cols <- function(df) {
  return(names(df)[sapply(df, function(x) all(is.na(x)))])
}

