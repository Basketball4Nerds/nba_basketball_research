## this function creates new column name for various 
## cum cnt, cum sum, mv avg functions
create_new_cum_colnm <- function(col, 
                                 new_colnm_apnd_str, 
                                 type=c('cumcnt', 'cumsum', 'cummean', 
                                        'cumperf', 'sma', 'ema'),
                                 n=NULL) {
  
  ## set type
  type <- tolower(type[1])
  
  ## replace new column name append string with 'gen' if nothing was specified
  new_colnm_apnd_str <- ifelse(is.null(new_colnm_apnd_str) || new_colnm_apnd_str=='',
                               'gen', new_colnm_apnd_str)
  
  ## remove opponent specification from append string (e.g. p_cumsum_cnf, not p_cumsum_o_cnf)
  new_colnm_apnd_str <- gsub('^o_', '', new_colnm_apnd_str)
  
  ## camel-case when append string contains underscore(s)
  ## (e.g. p_cumsum_oeffQntlRnk, not p_cumsum_oeff_qntl_rnk)
  new_colnm_apnd_str <- camelCase(new_colnm_apnd_str)
  
  ## case for cum sum or cum mean
  if (type %in% c('cumcnt', 'cumsum', 'cummean', 'cumperf')) {
    new_colnm <- paste0(col, '_', type, '_', new_colnm_apnd_str) 
  } 
  
  ## case for moving average (with n)
  else if (type %in% c('sma', 'ema')) { 
    new_colnm <- paste0(col, '_', tolower(type), n, '_', new_colnm_apnd_str) 
  }
  
  ## replace double underscores to one if exist
  new_colnm <- gsub('__', '_', new_colnm)
  
  ## return 
  return(new_colnm)
}


