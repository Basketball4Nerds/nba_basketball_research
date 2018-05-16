
## this function returns names of columns that contain Inf or NaN
list_cols_w_inf_or_nan <- function(df) {
  return(names(df)[sapply(df, function(x) { any(is.infinite(x) || is.nan(x)) })])
}


