## this function removes any columns matched by given regex expression
rm_colnms_by_regex_mtch <- function(df, regex_expr) {
  return(df[ , !grepl(regex_expr, names(df))])
}


