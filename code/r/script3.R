
## create predictive df;
## (takes the difference between tm metric columns and 
## opp metric columns in the master df and saves the difference
## as columns in predictive df)
predictive_df <- create_predictive_df(master)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))


## backup predictive df
write.csv(predictive_df, './data/predictive_df.csv', row.names=FALSE)
# predictive_df <- read.csv('./data/predictive_df.csv', stringsAsFactors=FALSE)
# predictive_df$date <- as.Date(predictive_df$date)


## split data into train and final test data
unique(predictive_df$season)
length(unique(predictive_df$season))
train <- subset(predictive_df, season %in% 1995:2014)
final_test <- subset(predictive_df, season %in% 2015:2016)


## split predictive df by outcome
won_df <- subset(train, won)
lost_df <- subset(train, !won)


## get various predictor cols
wpc_cols <- names(train)[grepl('^wpc_', names(train))]
cumperf_cols <- names(train)[grepl('_cumperf_', names(train))]
j_cols <- names(train)[grepl("^j", names(train), perl = TRUE)]
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

