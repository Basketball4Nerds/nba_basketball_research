## this function determines if given df is a single season-team df
is_sngl_ssn_tm_df <- function(df) {
  return(length(unique(df$season))==1 && length(unique(df$team))==1) 
}
