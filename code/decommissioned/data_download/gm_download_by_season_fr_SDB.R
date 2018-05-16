## download latest daily games data by season (not by date)
seasons <- 1995:2017
gamesLst <- lapply(seasons, function(season) {
  Sys.sleep(1)
  print(season)
  getRawGamesDataViaApi(season=season)
})
games <- do.call(rbind.data.frame, gamesLst)
games <- games[!is.na(games$pts), ]


## fill in missing quarter scores data manually
subset(games, date=='2016-10-25')
games$quarter_scores[games$date==as.Date('2016-10-25') & games$team=='Cavaliers'] <- '28-20-34-35'
games$quarter_scores[games$date==as.Date('2016-10-25') & games$team=='Knicks'] <- '18-27-19-24'
games$quarter_scores[games$date==as.Date('2016-10-25') & games$team=='Trailblazers'] <- '26-28-23-36'
games$quarter_scores[games$date==as.Date('2016-10-25') & games$team=='Jazz'] <- '26-20-37-21'
games$quarter_scores[games$date==as.Date('2016-10-25') & games$team=='Warriors'] <- '20-26-31-23'
games$quarter_scores[games$date==as.Date('2016-10-25') & games$team=='Spurs'] <- '31-33-33-32'

games$o_quarter_scores[games$date==as.Date('2016-10-25') & games$o_team=='Cavaliers'] <- '28-20-34-35'
games$o_quarter_scores[games$date==as.Date('2016-10-25') & games$o_team=='Knicks'] <- '18-27-19-24'
games$o_quarter_scores[games$date==as.Date('2016-10-25') & games$o_team=='Trailblazers'] <- '26-28-23-36'
games$o_quarter_scores[games$date==as.Date('2016-10-25') & games$o_team=='Jazz'] <- '26-20-37-21'
games$o_quarter_scores[games$date==as.Date('2016-10-25') & games$o_team=='Warriors'] <- '20-26-31-23'
games$o_quarter_scores[games$date==as.Date('2016-10-25') & games$o_team=='Spurs'] <- '31-33-33-32'

## write data as CSV
write.csv(games, './data/games.csv', row.names=FALSE)
