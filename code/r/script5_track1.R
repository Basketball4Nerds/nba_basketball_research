## In this code track, we will select predictor variables to include in our models
## by hand-picking variables that have demonstrated exceptional predictive power year over year.
## We will then remove collinear variables by measuring each variable's VIF
## and removing the ones with the highest VIF iteratively. We will then use cross validation to 
## compare models (logistic regression, decision tree, n-nearest neighbor) and
## pick the best one.

## https://www.kaggle.com/robertoruiz/dealing-with-multicollinearity



## to pick the best win perc metric
a <- get_pred_perf_rnk_plcmnt_lst(train, 
                                   predictor_vars=wpc_cols, 
                                   rank_method='pred_acc', min_n=5)
lapply(a, table)
# wpc_site picked


## to pick the best oeff cumperf metric
b <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=oeff_cumperf_cols,
                                  rank_method='pred_acc', min_n=5)
lapply(b, table)
# oeff_cumperf_gen, oeff_cumperf_oeffaQntlRnk, oeff_cumperf_site picked


## to pick the best oeffA cumperf metric
c <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=oeffA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(c, table)
# oeffA_cumperf_oeffQntlRnk, oeffA_cumperf_site picked


## to pick the best FGP cumperf metric
d <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=FGP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(d, table)
# FGP_cumperf_site picked


## to pick the best FGPA cumperf metric
e <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=FGPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(e, table)
# FGPA_cumperf_site picked


## to pick the best rqP cumperf metric
f <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=rqP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(f, table)
# rqP_cumperf_gen picked


## to pick the best rqPA cumperf metric
g <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=rqPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(g, table)
# rqPA_cumperf_gen picked


## to pick the best J metric
h <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=j_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(h, table)
# j5 picked


## average out the best performing cumperf metrics of their categories
train$oeff_cumperf_xyz <- round((train$oeff_cumperf_gen + train$oeff_cumperf_oeffaQntlRnk + train$oeff_cumperf_site) / 3, 3)
train$oeffA_cumperf_xyz <- train$oeffA_cumperf_oeffQntlRnk + train$oeffA_cumperf_site 


## track 1 predictor variables
trk1_predictor_vars <- c('wpc_site', 
                            'oeff_cumperf_xyz', 'oeffA_cumperf_xyz', 
                            'FGP_cumperf_site', 'FGPA_cumperf_site', 
                            'rqP_cumperf_gen', 'rqPA_cumperf_gen', 'j5')

x <- train[ , c()]
base_cols

# set.seed(2)
# library(caret)
# fitControl <- trainControl(method = 'cv', number = 10)
# cartGrid <- expand.grid(.cp = seq(0.002, 0.1, by=0.002))
# train(over50k ~ ., data = train, method = 'rpart', trControl = fitControl, tuneGrid = cartGrid)


