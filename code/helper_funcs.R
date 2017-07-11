############ GENERAL/HELPER/MISCELLANEOUS FUNCTIONS ################

## this function takes in df and "fills in" the selected metrics
## for opponent by join method
fill_in_opp_cols <- function(df, cols) {
  
  ## make a partial copy of df
  df_pc <- df[ , c('date', 'team', cols)]
  
  ## add o_ prefix to all column names except the date
  names(df_pc) <- paste0('o_', names(df_pc))
  names(df_pc)[names(df_pc)=='o_date'] <- 'date'
  
  ## make a left join
  df <- left_join(df, df_pc, by=c('date', 'o_team'))
  
  ## return 
  return(df)
}


## this function removes any columns matched by given regex expression
rm_colnms_by_regex_mtch <- function(df, regex_expr) {
  return(df[ , !grepl(regex_expr, names(df))])
}


## this function takes in a df and returns an empty copy of the original
create_empty_df_copy <- function(df) {
  empty_df_copy <- matrix(NA, nrow=nrow(df), ncol=ncol(df))
  colnames(empty_df_copy) <- colnames(df)
  rownames(empty_df_copy) <- rownames(df)
  return(empty_df_copy)
}


## this function determines if a value is TRUE;
## (can work with NA values)
is.true <- function(x) {
  !is.na(x) & x
}


## this function determines if a value is FALSE;
## (can work with NA values)
is.false <- function(x) {
  !is.na(x) & !x
}


## this function rounds numbers in df to nearest decimal places
## http://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


## this function capitalizes the first letter in a string
## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


## this function determines if values of elements in df are NaN
## http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))  
}


## this function grabs the names of numeric variables
getNumericVarNames <- function(df) {
  numericVars <- colnames(df)[sapply(df, is.numeric)]
  return(numericVars)
}


## this function capitalizes words
capitalizeStr <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


## this function concatenates datasets and exports into a single CSV
## by taking input and output paths as arguments
concatenateDatasets <- function(dirPath, exportPath) {
  
  ## get dataset file names
  rawFileNames <- list.files(dirPath)
  rawCSVFileNames <- rawFileNames[grepl('\\.csv$', rawFileNames)]
  rawCSVFilePaths <- file.path(dirPath, rawCSVFileNames)
  
  ## function to merge multiple datasets into one
  combineCSVs <- function(CSVFilePaths) {
    dataList <- lapply(CSVFilePaths, function(x) {read.csv(file=x, header=T, stringsAsFactors=F)})
    df <- Reduce(function(x, y) {rbind(x, y)}, dataList)
    return(df)
  }
  
  ## load regular and post season datasets
  df <- combineCSVs(rawCSVFilePaths)
  
  ## export
  write.csv(df, exportPath, row.names=FALSE)
}


## this function gets season from a given date
getSeasonFrDate <- function(date) {
  date <- as.Date(date)
  month <- as.integer(format(date, '%m'))
  year <- as.integer(format(date, '%Y'))
  if (month %in% c(10, 11, 12)) 
    season <- year
  else if (month %in% c(1, 2, 3, 4, 5, 6))
    season <- year - 1
  else 
    season <- NULL
  return(season)
}


## this function converts amongst city names, city abbreviations, and team names
convertTeamLabels <- function(df, 
                              from=c('city', 'city_abbr', 'team'), 
                              to=c('city', 'city_abbr', 'team'),
                              team_cols=c('team', 'o_team')) {
  
  from <- from[1]
  to <- to[1]
  
  for (i in 1:nrow(TeamCityConfDf)) {
    city <- TeamCityConfDf$City[i]
    city_abbr <- TeamCityConfDf$CityAbbr[i]
    team <- TeamCityConfDf$Team[i]
    
    for (team_col in team_cols) {
      df[[team_col]] <- gsub(get(from), get(to), df[[team_col]]) 
    }
  }
  
  return(df)
} 


## this function creates odds df by combining 
## expected spreads df and expected total points df
createOddsDf <- function(spreads, totals) {
  
  ## remove payout column from the dfs
  spreads$payout <- totals$payout <- NULL
  
  ## merge dfs
  odds <- merge(spreads, totals, by=c('season', 'date', 'team', 'o_team'), all=TRUE)
  
  ## create projection cols
  odds$pts_proj_om <- (odds$total / 2) - (odds$adjustor / 2)
  odds$pts_alwd_to_o_proj_om <- (odds$total / 2) + (odds$adjustor / 2)
  odds$pts_margin_proj_om <- -odds$adjustor
  odds$win_proj_om <- ifelse(odds$pts_margin_proj_om > 0, TRUE,
                             ifelse(odds$pts_margin_proj_om < 0, FALSE, NA))
  
  ## return
  return(odds)
}


## this function tweaks vary-by variables for proper aggregation
assure_correct_varyby_vars <- function(vary_by) {
  
  ## remove 'o_' prefices if exist
  vary_by <- gsub('^o_', '', vary_by)
  
  ## add 'o_' to each vary-by variable except for site
  vary_by <- ifelse(vary_by=='site', vary_by, paste0('o_', vary_by))
  
  ## return
  vary_by
}


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
  
  ## case for cum count
  if (type=='cumcnt') {
    new_colnm <- paste0(col, '_', new_colnm_apnd_str)
  } 
  
  ## case for cum sum or cum mean
  else if (type %in% c('cumsum', 'cummean', 'cumperf')) {
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


## this function determines if given df is a single season-team df
is_sngl_ssn_tm_df <- function(df) {
  return(length(unique(df$season))==1 && length(unique(df$team))==1) 
}


## this function camel-cases a string
## http://www.stat.cmu.edu/~hseltman/files/camelCase.R
camelCase = function(sv, upper=FALSE, capIsNew=FALSE, alreadyTrimmed=FALSE) {
  if (!is.character(sv)) stop("'sv' must be a string vector")
  if (!alreadyTrimmed) sv = gsub("[[:space:]]*$", "", gsub("^[[:space:]]*", "", sv))
  if (capIsNew) {
    sv = gsub("([A-Z])", " \\1", sv)
    sv = gsub("^[[:space:]]", "", sv)
    sv = tolower(sv)
  }
  apart = strsplit(sv, split="[[:space:][:punct:]]")
  apart = lapply(apart, tolower)
  capitalize = function(x) paste0(toupper(substring(x,1,1)), substring(x,2))
  if (upper) {
    apart = lapply(apart, capitalize)
  } else {
    apart = lapply(apart, function(x) c(x[1], capitalize(x[-1])))
  }
  return(sapply(apart, paste, collapse=""))
}


## this function calculates accuracy from confusion matrix
calc_acc_fr_cnf_mtx <- function(cnf_mtx, rnd_dgt=3) {
  cnf_mtx <- as.matrix(cnf_mtx)
  acc_pc <- sum(diag(cnf_mtx)) / sum(cnf_mtx)
  acc_pc <- round(acc_pc, rnd_dgt)
  return(acc_pc)
}


## this function returns TRUE if vector is sorted in increasin order
is.sorted <- function(x) { return(!is.unsorted(x)) }


## this function calculates moneyline payout profit in case of met bet; 
## does not include the initial amount wagered;
## https://www.gamblingsites.org/sports-betting/beginners-guide/odds/moneyline/
calc_moneyline_payout_profit <- function(wgr_amt=100, moneyline_odds) {
  
  ## calculate payout profit for bet on underdog
  # if (moneyline_odds > 0) {
  #   payout_profit <- wgr_amt * (moneyline_odds / 100)
  # }
  # 
  # ## payout profit for bet on favored  
  # else {
  #   payout_profit <- wgr_amt / (abs(moneyline_odds) / 100)
  # }
  
  ## calculate payout profit for bet on underdog
  ## (superior method as it allows for vectorization with vector inputs)
  payout_profit <- ifelse(moneyline_odds > 0, 
                          wgr_amt * (moneyline_odds / 100), 
                          wgr_amt / (abs(moneyline_odds) / 100))
  
  ## round down at 2 decimal places (very miniscule underestimation of payout profit)
  payout_profit <- floor(payout_profit * 100) / 100
  
  ## return
  return(payout_profit)
}


## this function calculates and returns expected value based on
## prob of win, payout for win, and money wagered;
## Sport Bet Expected Value Formula: 
# Expected Value = 
# (Probability of Winning) x (Amount Won per Bet) – 
# (Probability of Losing) x (Amount Lost per Bet)
calc_exp_val <- function(wgr_amt, moneyline_odds, w_prob) {
  
  ## calculate probability of loss
  l_prob <- 1 - w_prob
  
  ## calculate win payout profit (in case of met bet)
  w_payout <- calc_moneyline_payout_profit(wgr_amt, moneyline_odds)
  
  ## calculate expected value
  exp_val <- (w_payout * w_prob) - (wgr_amt * l_prob)
  
  ## round down at 2 decimal places if pos
  # payout_profit <- floor(payout_profit * 100) / 100
  
  ## return
  return(exp_val)
}

