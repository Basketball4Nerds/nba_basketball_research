
## this function converts amongst city names, city abbreviations, and team names
convert_team_labels <- function(df, 
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


