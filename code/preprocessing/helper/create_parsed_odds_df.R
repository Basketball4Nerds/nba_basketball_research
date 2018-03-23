## this function takes in raw odds df and returns a parsed version of it
create_parsed_odds_df <- function(odds_df, type=c('spreads', 'totals', 'moneylines')) {
  
  ## set type
  type <- type[1]
  
  ## get names of original columns (that contain the odds) to iterate over 
  orig_odds_cols <- paste0('X', 1:10)
  
  ## base columns
  base_cols <- c('season', 'date', 'team', 'o_team')
  
  ## odds-maker sources
  oddsmakers <- c('pinnacle', 'betonline', 'interlops', 'bookmaker', '5dimes', 
                  'betdsi', 'heritage', 'bovada', 'youwager', 'justbet')
  
  ## processing moneylines df
  if (type=='moneylines') {
    
    ## for each col, convert type from character to numeric
    for (col in orig_odds_cols) {
      odds_df[[col]] <- as.numeric(odds_df[[col]])
    }
    
    ## change colnames
    new_odds_cols <- paste0(oddsmakers, '_ml')
    names(odds_df) <- c(base_cols, new_odds_cols)
    
    ## return
    return(odds_df)
  } 
  
  ## processing spreads/totals df
  else if (type %in% c('spreads', 'totals')) {
    
    ## create base df (that contain non-odds info, such as season, date, team, o_team)
    ret_df <- odds_df[ , base_cols]
    
    ## for each col, parse data
    for (col in orig_odds_cols) {
      
      ## select col, replace alphabet characters, split data
      col_vals <- odds_df[[col]] 
      col_vals <- gsub('^PK', '0 ', col_vals)  # rare PK cases for spreads data
      col_vals_split <- strsplit(col_vals, ' ')
      
      ## extract projections data (totals or lines) and payout data
      projections <- unlist(lapply(col_vals_split, function(x) x[1]))  # lines in case of spreads df; totals projected in case of totals df
      payouts <- unlist(lapply(col_vals_split, function(x) x[2]))  # payout moneylines
      
      ## add to df 
      ret_df <- cbind(ret_df, projections, payouts)
    }
    
    ## create col names
    new_odds_cols <- rep(oddsmakers, each=2)
    if (type=='spreads')
      new_odds_cols <- paste0(new_odds_cols, c('_line', '_payout'))
    else if (type=='totals')
      new_odds_cols <- paste0(new_odds_cols, c('_total', '_payout'))
    names(ret_df) <- c(base_cols, new_odds_cols)
    
    ## return
    return(ret_df)
  }
}