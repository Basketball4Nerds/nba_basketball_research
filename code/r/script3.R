
## save vector base cols as variable
base_cols <- c('gid', 'season', 'date', 'site', 'playoffs', 
               'team', 'o_team', 'cnf', 'o_cnf', 'won')


## create predictive df;
## (takes the difference between tm metric columns and 
## opp metric columns in the master df and saves the difference
## as columns in predictive df)
predictive_df <- create_predictive_df(master, include_cnt_cols=TRUE)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))


## backup predictive df
write.csv(predictive_df, './data/predictive_df.csv', row.names=FALSE)
# predictive_df <- read.csv('./data/predictive_df.csv', stringsAsFactors=FALSE)
# predictive_df$date <- as.Date(predictive_df$date)


## factorize prediction variable (since many of the algorithms require it)
predictive_df$won <- as.factor(predictive_df$won)


## split data into train and final test data
unique(predictive_df$season)
length(unique(predictive_df$season))
train <- subset(predictive_df, season %in% 1995:2014)
final_test <- subset(predictive_df, season %in% 2015:2016)


## split predictive df by outcome
won_df <- subset(train, won)
lost_df <- subset(train, !won)


## get cumperf cols
oeff_cumperf_cols <- names(train)[grepl("^oeff_cumperf_", names(train), perl = TRUE)]
oeffA_cumperf_cols <- names(train)[grepl("^oeffA_cumperf_", names(train), perl = TRUE)]
FGP_cumperf_cols <- names(train)[grepl("^FGP_cumperf_", names(train), perl = TRUE)]
FGPA_cumperf_cols <- names(train)[grepl("^FGPA_cumperf_", names(train), perl = TRUE)]
rqP_cumperf_cols <- names(train)[grepl("^rqP_cumperf_", names(train), perl = TRUE)]
rqPA_cumperf_cols <- names(train)[grepl("^rqPA_cumperf_", names(train), perl = TRUE)]
pos_cumperf_cols <- names(train)[grepl("^pos_cumperf_", names(train), perl = TRUE)]
posA_cumperf_cols <- names(train)[grepl("^posA_cumperf_", names(train), perl = TRUE)]
cumperf_cols <- names(train)[grepl("_cumperf_", names(train), perl = TRUE)]


## get wpc cols
wpc_cols <- names(train)[grepl('^wpc_', names(train))]


## get J cols
j_cols <- names(train)[grepl("^j", names(train), perl = TRUE)]


## get miscellaneous predictor cols
misc_predictor_cols <- c('line', 'rst', 'home', 'mtchmrgn')


## all predictor variables and prediction variable
predictors <- c(wpc_cols, cumperf_cols, j_cols, misc_predictor_cols)
prediction_var <- 'won'

## plot variable importance determined by spread -- all predictor variables
varimp_df <- create_varimp_df(won_df, lost_df, predictors=predictors, normalize=TRUE)
varimp_df
plot_varimp(varimp_df)


## plot variable importance determined by spread -- wpc predictor variables
wpc_varimp_df <- create_varimp_df(won_df, lost_df, predictors=wpc_cols, normalize=TRUE)
wpc_varimp_df
plot_varimp(wpc_varimp_df)


## plot variable importance determined by spread -- oeff cumperf predictor variables
oeff_varimp_df <- create_varimp_df(won_df, lost_df, predictors=oeff_cumperf_cols, normalize=TRUE)
oeff_varimp_df
plot_varimp(oeff_varimp_df)


## plot variable importance determined by spread -- oeffA cumperf predictor variables
oeffA_varimp_df <- create_varimp_df(won_df, lost_df, predictors=oeffA_cumperf_cols, normalize=TRUE)
oeffA_varimp_df
plot_varimp(oeffA_varimp_df)


## plot variable importance determined by spread -- FGP cumperf predictor variables
FGP_varimp_df <- create_varimp_df(won_df, lost_df, predictors=FGP_cumperf_cols, normalize=TRUE)
FGP_varimp_df
plot_varimp(FGP_varimp_df)


## plot variable importance determined by spread -- FGPA cumperf predictor variables
FGPA_varimp_df <- create_varimp_df(won_df, lost_df, predictors=FGPA_cumperf_cols, normalize=TRUE)
FGPA_varimp_df
plot_varimp(FGPA_varimp_df)


## plot variable importance determined by spread -- rqP cumperf predictor variables
rqP_varimp_df <- create_varimp_df(won_df, lost_df, predictors=rqP_cumperf_cols, normalize=TRUE)
rqP_varimp_df
plot_varimp(rqP_varimp_df)


## plot variable importance determined by spread -- rqPA cumperf predictor variables
rqPA_varimp_df <- create_varimp_df(won_df, lost_df, predictors=rqPA_cumperf_cols, normalize=TRUE)
rqPA_varimp_df
plot_varimp(rqPA_varimp_df)


## plot variable importance determined by spread -- pos cumperf predictor variables
pos_varimp_df <- create_varimp_df(won_df, lost_df, predictors=pos_cumperf_cols, normalize=TRUE)
pos_varimp_df
plot_varimp(pos_varimp_df)


## plot variable importance determined by spread -- posA cumperf predictor variables
posA_varimp_df <- create_varimp_df(won_df, lost_df, predictors=posA_cumperf_cols, normalize=TRUE)
posA_varimp_df
plot_varimp(posA_varimp_df)


## plot variable importance determined by spread -- J predictor variables
j_varimp_df <- create_varimp_df(won_df, lost_df, predictors=j_cols, normalize=TRUE)
j_varimp_df
plot_varimp(j_varimp_df)



## ranking variable importance using mRMR package
mRMR_varimp_df <- create_mRMR_varimp_df(train, predictors, prediction_var='won')
mRMR_varimp_df
