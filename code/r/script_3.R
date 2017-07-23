
## subset df
master_df <- subset(master, season %in% 2012)


## create predictive df;
## (takes the difference between tm metric columns and 
## opp metric columns in the master df and saves the difference
## as columns in predictive df)
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))


## split predictive df by outcome
won_df <- subset(predictive_df, won)
lost_df <- subset(predictive_df, !won)


## get various predictor cols
wpc_cols <- names(predictive_df)[grepl('^wpc_', names(predictive_df))]
cumperf_cols <- names(predictive_df)[grepl('_cumperf_', names(predictive_df))]
j_cols <- names(predictive_df)[grepl("^j", names(predictive_df), perl = TRUE)]
misc_predictor_cols <- c('line', 'rst', 'home', 'mtchmrgn')


## all predictor variables
predictor_vars <- c(wpc_cols, cumperf_cols, j_cols, misc_predictor_cols)


## plot variable importance determined by spread -- all predictor variables
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=predictor_vars, normalize=TRUE)
varimp_df
plot_varimp(varimp_df)


## plot variable importance determined by spread -- wpc predictor variables
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=wpc_cols, normalize=TRUE)
varimp_df
plot_varimp(varimp_df)


## plot variable importance determined by spread -- cumperf predictor variables
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=cumperf_cols, normalize=TRUE)
varimp_df
plot_varimp(varimp_df)


## plot variable importance determined by spread -- J predictor variables
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=j_cols, normalize=TRUE)
varimp_df
plot_varimp(varimp_df)

