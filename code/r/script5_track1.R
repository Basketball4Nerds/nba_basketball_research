## In this code track, we will select predictor variables to include in our models
## by hand-picking variables that have demonstrated exceptional predictive power 
## year over year in each category. We will then remove collinear variables
## by measuring each variable's VIF and removing the ones with the highest VIF iteratively. 
## We will then use cross validation to compare model performances and pick the best one.


## to pick the best win perc metric
a <- get_pred_perf_rnk_plcmnt_lst(train, 
                                   predictor_vars=wpc_cols, 
                                   rank_method='pred_acc', min_n=5)
lapply(a, table)[[1]]
# wpc_site picked


## to pick the best oeff cumperf metric
b <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=oeff_cumperf_cols,
                                  rank_method='pred_acc', min_n=5)
lapply(b, table)[[1]]
# oeff_cumperf_site picked


## to pick the best oeffA cumperf metric
c <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=oeffA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(c, table)[[1]]
# oeffA_cumperf_site picked


## to pick the best FGP cumperf metric
d <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=FGP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(d, table)[[1]]
# FGP_cumperf_site picked


## to pick the best FGPA cumperf metric
e <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=FGPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(e, table)[[1]]
# FGPA_cumperf_site picked


## to pick the best rqP cumperf metric
f <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=rqP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(f, table)[[1]]
# rqP_cumperf_site picked


## to pick the best rqPA cumperf metric
g <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictor_vars=rqPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(g, table)[[1]]
# rqPA_cumperf_oeffQntlRnk picked


## to pick the best J metric
h <- get_pred_perf_rnk_plcmnt_lst(train, 
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
cor_mtx <- round(cor(train[trk1_predictor_vars], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')


## remove highly correlated variables via VIF calc
trk1_predictor_vars_2 <- vif_func(in_frame=train[ , trk1_predictor_vars], 
                                  thresh=5, trace=TRUE)
trk1_predictor_vars_2


## create track 1 parent formula
trk1_formula_orig <- create_model_formula(trk1_predictor_vars_2, 'won')



#### logistic regression track ####

## first logistic regression
trk1_glm_model0 <- glm(trk1_formula_orig, data=train_complete, family='binomial')
print(trk1_glm_model0)
summary(trk1_glm_model0)


## filter out predictors again by selecting variabales deemed stat. sig. by GLM
glm_statsig_predictors <- summary(trk1_glm_model0)$coefficients[ , 4] < 0.05
glm_statsig_predictors <- names(glm_statsig_predictors[glm_statsig_predictors])
glm_statsig_predictors <- setdiff(glm_statsig_predictors, '(Intercept)')
glm_statsig_predictors


## create child formula for GLM
trk1_formula_glm1 <- create_model_formula(glm_statsig_predictors, 'won')


## second logistic regression using child formula
## cross validation result for glm
glm_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_formula_glm1)





range(cv_pred_df$prob)
hist(cv_pred_df$prob)


trk1_glm_model1 <- glm(trk1_formula_glm1, data=train_complete, family='binomial')
print(trk1_glm_model1)
summary(trk1_glm_model1)

## 
pred <- predict(trk1_glm_model1, type='response') > 0.5

##







xxx











# #### decision tree track ####
# 
# ## decision tree
# set.seed(123)
# trk1_rpart_model0 <- rpart(trk1_formula_orig, data=train_complete)
# fit.rp <- rpart(as.factor(y) ~ ., train.data, control=rpart.control(cp=.001))
# fit.rp2 <- rpart(as.factor(y) ~ ., train.data, control=(cp=.005))
# 
# 
# 
# 
# 
# #### support vector machine track ####
# trk1_svm_model0 <- svm(trk1_formula_orig, data=train_complete)
# 
# 
# 
# 
# 
# #### naive bayes track ####
# trk1_nb_model <- nb(trk1_formula, data=train_complete)
# 
# 
# 
# 
# 
# #### k-nearest neighbor track ####
# # trk1_knn_model <- knn(trk1_formula, data=train_complete, method='knn', trControl=trControl)


