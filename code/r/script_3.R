
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


## get various cumperf predictor cols by metric type
oeff_cumperf_cols <- names(predictive_df)[grepl("^oeff_cumperf_", names(predictive_df), perl = TRUE)]
oeffA_cumperf_cols <- names(predictive_df)[grepl("^oeffA_cumperf_", names(predictive_df), perl = TRUE)]
FGP_cumperf_cols <- names(predictive_df)[grepl("^FGP_cumperf_", names(predictive_df), perl = TRUE)]
FGPA_cumperf_cols <- names(predictive_df)[grepl("^FGPA_cumperf_", names(predictive_df), perl = TRUE)]
rqP_cumperf_cols <- names(predictive_df)[grepl("^rqP_cumperf_", names(predictive_df), perl = TRUE)]
rqPA_cumperf_cols <- names(predictive_df)[grepl("^rqPA_cumperf_", names(predictive_df), perl = TRUE)]
pos_cumperf_cols <- names(predictive_df)[grepl("^pos_cumperf_", names(predictive_df), perl = TRUE)]
posA_cumperf_cols <- names(predictive_df)[grepl("^posA_cumperf_", names(predictive_df), perl = TRUE)]


# ## get various predictor cols by vary-by type
# gen_predictor_cols <- names(predictive_df)[grepl('(wpc_|cumperf_)gen', names(predictive_df))]
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
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=predictor_vars, normalize=TRUE)
varimp_df
plot_varimp(varimp_df)


## first determine which metrics produce strong spreads by outcome


## then determine which variable specification produce strong spreads by outcome





