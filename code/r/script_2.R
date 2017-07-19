## subset df
master_df <- subset(master, season==2012)


## create prediction df
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))



## 
won_df <- subset(predictive_df, won)
lost_df <- subset(predictive_df, !won)
cumperf_cols <- names(won_df)[grepl('_cumperf_', names(won_df))]
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=cumperf_cols, normalize=TRUE)
varimp_df <- sortByCol(varimp_df, 'spread')
varimp_df
plot_varimp(varimp_df)


