## this function gets season from a given date
get_season_fr_date <- function(date) {
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

