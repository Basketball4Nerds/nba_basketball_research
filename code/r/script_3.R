
## subset df
master_df <- subset(master, season==2012)


## create predictive df;
## (takes the difference between tm metric columns and 
## opp metric columns in the master df and saves the difference
## as columns in predictive df)
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))


## get various predictor cols
wpc_cols <- names(predictive_df)[grepl('^wpc_', names(predictive_df))]
cumperf_cols <- names(predictive_df)[grepl('_cumperf_', names(predictive_df))]
j_cols <- names(predictive_df)[grepl("^j", names(predictive_df), perl = TRUE)]
misc_predictor_cols <- c('line', 'rst', 'home', 'mtchmrgn')




## get various predictor cols by vary-by type
gen_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)gen', names(predictive_df))]
# site_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)site', names(predictive_df))]
# cnf_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)cnf', names(predictive_df))]
# oeffQntlRnk_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)oeffQntlRnk', names(predictive_df))]
# oeffaQntlRnk_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)oeffaQntlRnk', names(predictive_df))]


## all predictor variables
predictor_vars <- c(wpc_cols, cumperf_cols, j_cols, misc_predictor_cols)


## split predictive df by outcome
won_df <- subset(predictive_df, won)
lost_df <- subset(predictive_df, !won)


## plot variable importance determined by spread
varimp_df1 <- create_varimp_df(won_df, lost_df, predictor_vars=predictor_vars, normalize=TRUE)
varimp_df1
plot_varimp(varimp_df1)


## first determine which metrics produce strong spreads by outcome

## then determine which variable specification produce strong spreads by outcome





