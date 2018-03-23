library(nbastatR)
x <- get_games_box_scores()
x <- get_game_logs(seasons=1980:2016, result_type='team')
y <- as.data.frame(x)
head(y)

table()
z <- subset(y, is.na(x$astTeam))
table(z$yearSeason)
