## this function tweaks vary-by variables for proper aggregation
assure_correct_varyby_vars <- function(vary_by) {
  
  ## remove 'o_' prefices if exist
  vary_by <- gsub('^o_', '', vary_by)
  
  ## add 'o_' to each vary-by variable except for site
  vary_by <- ifelse(vary_by=='site', vary_by, paste0('o_', vary_by))
  
  ## return
  vary_by
}


