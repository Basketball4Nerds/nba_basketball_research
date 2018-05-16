## get spreads df
spreads <- dbReadTable(conn=con, name='spreads')

## look at number of missing values by column
spreads %>% summarise_all(funs(sum(is.na(.))))


spreads <- spreads %>%
  select(c('season', 'game_date', 'team_abbr', 'opponent_abbr', 'pinnacle_line', 'pinnacle_payout')) %>%
  mutate(
    game_date = as.Date(game_date)
  )
head(spreads)
head(spreads)
names(pregame_perf_df)
table(pregame_perf_df$season)
table(spreads$season)
x <- left_join(pregame_perf_df, spreads, by = c('season', 'game_date', 'team_abbr', 'opponent_abbr'))
