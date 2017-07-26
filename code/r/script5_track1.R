## In this code track, we will select predictor variables to include in our models
## by hand-picking variables that have demonstrated exceptional predictive power 
## year over year in each category. We will then remove collinear variables
## by measuring each variable's VIF and removing the ones with the highest VIF iteratively. 
## We will then use cross validation to compare model performances and pick the best one.


## https://www.kaggle.com/robertoruiz/dealing-with-multicollinearity
## https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/


## train dataset for track 1
train_trk1 <- train


## to pick the best win perc metric
a <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                   predictor_vars=wpc_cols, 
                                   rank_method='pred_acc', min_n=5)
lapply(a, table)[[1]]
# wpc_site picked


## to pick the best oeff cumperf metric
b <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=oeff_cumperf_cols,
                                  rank_method='pred_acc', min_n=5)
lapply(b, table)[[1]]
# oeff_cumperf_site picked


## to pick the best oeffA cumperf metric
c <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=oeffA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(c, table)[[1]]
# oeffA_cumperf_site picked


## to pick the best FGP cumperf metric
d <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=FGP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(d, table)[[1]]
# FGP_cumperf_site picked


## to pick the best FGPA cumperf metric
e <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=FGPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(e, table)[[1]]
# FGPA_cumperf_site picked


## to pick the best rqP cumperf metric
f <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=rqP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(f, table)[[1]]
# rqP_cumperf_site picked


## to pick the best rqPA cumperf metric
g <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=rqPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(g, table)[[1]]
# rqPA_cumperf_oeffQntlRnk picked


## to pick the best J metric
h <- get_pred_perf_rnk_plcmnt_lst(train_trk1, 
                                  predictor_vars=j_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(h, table)[[1]]
# j5 picked



## track 1 predictor variables
trk1_predictor_vars <- c('wpc_site', 
                         'oeff_cumperf_site', 'oeffA_cumperf_site', 
                         'FGP_cumperf_site', 'FGPA_cumperf_site', 
                         'rqP_cumperf_site', 'rqPA_cumperf_oeffQntlRnk', 'j5', 
                         'line', 'home', 'mtchmrgn')



## correlation plot
# https://stackoverflow.com/a/19115003
cor_mtx <- round(cor(train_trk1[trk1_predictor_vars], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')


## remove highly correlated variables via VIF calc
trk1_predictor_vars_2 <- vif_func(in_frame=train_trk1[ , trk1_predictor_vars], 
                                  thresh=5, trace=TRUE)


## set parameters
trk1_formula <- create_model_formula(trk1_predictor_vars_2, 'won')
trControl <- trainControl(method='cv', number=10)
#tuneGrid <- expand.grid(.cp = seq(0.002, 0.1, by=0.002))


## prepare train dataset for modeling
train_trk1 <- train_trk1[complete.cases(train_trk1), ]  # use only complete cases
train_trk1$won <- as.factor(train_trk1$won)  # factorize prediction var


## get list of model candidates
names(getModelInfo())


## set seed
set.seed(123)


## create various models for performance comparison:
# logistic regression
# decision tree
# support vector machine
# naive bayes
# k-nearest neighbor
# random forest
# generalized boosted regression
trk1_glm_model <- train(trk1_formula, data=train_trk1, method='glm', trControl=trControl)
trk1_rpart_model <- train(trk1_formula, data=train_trk1, method='rpart', trControl=trControl)
trk1_svm_model <- train(trk1_formula, data=train_trk1, method='svmLinear', trControl=trControl)
trk1_nb_model <- train(trk1_formula, data=train_trk1, method='nb', trControl=trControl)
trk1_knn_model <- train(trk1_formula, data=train_trk1, method='knn', trControl=trControl)
trk1_rf_model <- train(trk1_formula, data=train_trk1, method='rf', trControl=trControl)
trk1_gbm_model <- train(trk1_formula, data=train_trk1, method='gbm', trControl=trControl)


## save R model objects (since they are very time-consuming to recreate)
saveRDS(trk1_glm_model, "./data/RDS/trk1_glm_model.rds")
saveRDS(trk1_rpart_model, "./data/RDS/trk1_rpart_model.rds")
saveRDS(trk1_svm_model, "./data/RDS/trk1_svm_model.rds")
saveRDS(trk1_nb_model, "./data/RDS/trk1_nb_model.rds")
saveRDS(trk1_knn_model, "./data/RDS/trk1_knn_model.rds")
saveRDS(trk1_rf_model, "./data/RDS/trk1_rf_model.rds")
saveRDS(trk1_gbm_model, "./data/RDS/trk1_gbm_model.rds")


## check model performances
print(trk1_glm_model)
print(trk1_rpart_model)
print(trk1_svm_model)
print(trk1_nb_model)
print(trk1_knn_model)
print(trk1_rf_model)
print(trk1_gbm_model)

trk1_glm_model$results$Accuracy
trk1_rpart_model$results$Accuracy
trk1_svm_model$results$Accuracy
trk1_nb_model$results$Accuracy
trk1_knn_model$results$Accuracy
trk1_rf_model$results$Accuracy
trk1_gbm_model$results$Accuracy

