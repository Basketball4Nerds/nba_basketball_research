## this function adds game ID (gid) to df;
## gid consists of: 
## - game date without dashes
## - first 3 characters of team 1 
## - first 3 characters of team 2
## team 1 and team 2 are alphabetically ordered
add_gid <- function(df) {
  df$gid <- apply(cbind(format(as.Date(df$date), '%Y%m%d'), 
                        substr(as.character(df$team), 1, 3),
                        substr(as.character(df$o_team), 1, 3)), 
                  1, function(x) paste(sort(x), collapse="")) 
  return(df)
}