## implement a function for table shown in the following link:
# - https://www.teamrankings.com/nba/odds-history/results/
# - http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/trends/league/season.html

## What is power ranking? How is it ranked?
# - http://www.covers.com/sports/nba/powerrankings

## check out this page later:
# - http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/statistics/2016-2017/statistics_playoffs.html



library(nbastatR)
x <- get_games_box_scores(game_ids = c(21700002, 21700003), box_score_types = c("Traditional", "Advanced", "Scoring", "Misc", "Usage", "Four Factors", "Tracking"), result_types = c("player", "team"), join_data = TRUE, assign_to_environment = TRUE, return_message = TRUE)
head(x$dataBoxScore)



x <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Traditional", "Advanced", "Scoring", "Misc", "Usage", "Four Factors", "Tracking"), result_types = c("team"))
x$dataBoxScore
View(x$dataBoxScore)


y <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Traditional"), result_types = c("team"))


a <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Advanced"), result_types = c("team"))
b <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Scoring"), result_types = c("team"))
c <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Misc"), result_types = c("team"))

d <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Four Factors"), result_types = c("team"))
e <- get_games_box_scores(game_ids = c(21700002), box_score_types = c("Tracking"), result_types = c("team"))

View(x$dataBoxScore)
View(y$dataBoxScore)
View(a$dataBoxScore)
View(b$dataBoxScore)
View(c$dataBoxScore)
View(d$dataBoxScore)
View(e$dataBoxScore)


fuck <- get_seasons_schedule(seasons=2018)
head(fuck)
