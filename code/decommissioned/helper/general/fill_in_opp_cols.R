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


