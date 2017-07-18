
## subset df
master_df <- subset(master, season==2012)


## create prediction df
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))




