## In this code track, we will select predictor variables to include in our models
## by hand-picking variables that have demonstrated exceptional predictive power 
## year over year in each category. We will then remove collinear variables
## by measuring each variable's VIF and removing the ones with the highest VIF iteratively. 
## We will then use cross validation to compare model performances and pick the best one.


## to pick the best win perc metric
a <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=wpc_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(a, table)[[1]]
# wpc_site picked


## to pick the best oeff cumperf metric
b <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=oeff_cumperf_cols,
                                  rank_method='pred_acc', min_n=5)
lapply(b, table)[[1]]
# oeff_cumperf_site picked


## to pick the best oeffA cumperf metric
c <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=oeffA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(c, table)[[1]]
# oeffA_cumperf_site picked


## to pick the best FGP cumperf metric
d <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=FGP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(d, table)[[1]]
# FGP_cumperf_site picked


## to pick the best FGPA cumperf metric
e <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=FGPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(e, table)[[1]]
# FGPA_cumperf_site picked


## to pick the best rqP cumperf metric
f <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=rqP_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(f, table)[[1]]
# rqP_cumperf_site picked


## to pick the best rqPA cumperf metric
g <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=rqPA_cumperf_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(g, table)[[1]]
# rqPA_cumperf_oeffQntlRnk picked


## to pick the best J metric
h <- get_pred_perf_rnk_plcmnt_lst(train, 
                                  predictors=j_cols, 
                                  rank_method='pred_acc', min_n=5)
lapply(h, table)[[1]]
# j5 picked



## track 1 predictor variables
trk1_predictors <- c('wpc_site', 
                         'oeff_cumperf_site', 'oeffA_cumperf_site', 
                         'FGP_cumperf_site', 'FGPA_cumperf_site', 
                         'rqP_cumperf_site', 'rqPA_cumperf_oeffQntlRnk', 'j5', 
                         'line', 'home', 'mtchmrgn')


## correlation plot
# https://stackoverflow.com/a/19115003
cor_mtx <- round(cor(train[trk1_predictors], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')


## remove highly correlated variables via VIF calc
trk1_trimmed_predictors <- vif_func(in_frame=train[ , trk1_predictors], 
                                  thresh=5, trace=TRUE)
trk1_trimmed_predictors


## create track 1 original formula
trk1_orig_formula <- create_model_formula(trk1_trimmed_predictors, 'won')



#### logistic regression track ####

## create formula by selecting only stat. sig. variables
trk1_glm_model <- glm(trk1_orig_formula, data=train_complete, family='binomial')
glm_statsig_predictors <- get_statsig_predictors_fr_lm(trk1_glm_model, p_val=0.05)
glm_statsig_predictors
trk1_glm_formula <- create_model_formula(glm_statsig_predictors, 'won')

## get cross validation result 
glm1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='glm')  # cv using original predictors
glm2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_glm_formula, method='glm')  # cv using stat. sig. predictors

## evaludate average model performance by threshold
glm1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(glm1_cv_pred_df)
glm2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(glm2_cv_pred_df)
glm1_perf_by_thres_df
glm2_perf_by_thres_df

## glm1 (original) picked



#### decision tree track ####

## get cross validation result
rpart1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart')
rpart2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart', control=rpart.control(cp=0.001))
rpart3_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart', control=rpart.control(cp=0.003))
rpart4_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart', control=rpart.control(cp=0.005))

## evaludate average model performance by threshold
rpart1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart1_cv_pred_df)
rpart2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart2_cv_pred_df)
rpart3_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart3_cv_pred_df)
rpart4_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart4_cv_pred_df)
rpart1_perf_by_thres_df
rpart2_perf_by_thres_df
rpart3_perf_by_thres_df
rpart4_perf_by_thres_df

## rpart3 selected



#### support vector machine track ####

# takes too long; approach abandoned

# ## get cross validation result
# svm_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='svmlinear')
# 
# ## evaludate average model performance by threshold
# svm_perf_by_thres_df <- create_bythres_cv_perf_agg_df(svm_cv_pred_df)
# svm_perf_by_thres_df



#### naive bayes track ####

## get cross validation result
nb_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='nb')

## evaludate average model performance by threshold
nb_perf_by_thres_df <- create_bythres_cv_perf_agg_df(nb_cv_pred_df)
nb_perf_by_thres_df 




#### k-nearest neighbor track ####

## get cross validation result
knn5_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='knn', k=5)
knn10_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='knn', k=10)
knn20_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='knn', k=20)
knn50_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='knn', k=50)
knn100_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='knn', k=100)

## evaludate average model performance by threshold
knn5_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn5_cv_pred_df)
knn10_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn10_cv_pred_df)
knn20_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn20_cv_pred_df)
knn50_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn50_cv_pred_df)
knn100_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn100_cv_pred_df)
knn5_perf_by_thres_df 
knn10_perf_by_thres_df 
knn20_perf_by_thres_df 
knn50_perf_by_thres_df 
knn100_perf_by_thres_df  

## knn-100 picked



#### random forest track ####

## get cross validation result
rf_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rf',
                                         seed=123)

## evaludate average model performance by threshold
rf_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rf_cv_pred_df)
rf_perf_by_thres_df 



#### neural net track ####

## calculate n hidden nodes
n_hidden_nodes <- round(length(trk1_predictors) * 2/3 + 1)
n_hidden_nodes

## selecting weight decay
# https://stats.stackexchange.com/questions/273189/what-is-the-weight-decay-loss

## get cross validation result
nnet1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='nnet',
                                           decay=0.001, maxit=500, trace=FALSE, 
                                           n_hidden_nodes=n_hidden_nodes, seed=123)
nnet2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='nnet',
                                           decay=0.003, maxit=500, trace=FALSE, 
                                           n_hidden_nodes=n_hidden_nodes, seed=123)
nnet3_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='nnet',
                                           decay=0.005, maxit=500, trace=FALSE, 
                                           n_hidden_nodes=n_hidden_nodes, seed=123)

## evaludate average model performance by threshold
nnet1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(nnet1_cv_pred_df)
nnet2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(nnet2_cv_pred_df)
nnet3_perf_by_thres_df <- create_bythres_cv_perf_agg_df(nnet3_cv_pred_df)
nnet1_perf_by_thres_df 
nnet2_perf_by_thres_df 
nnet3_perf_by_thres_df 



#### ridge logistic regression ####

## get a rough estimate for optimal lambda
## https://stackoverflow.com/questions/30565457/getting-glmnet-coefficients-at-best-lambda
lambdas <- 10^seq(4, -4, by = -.1)
set.seed(123)
cv_fit <- cv.glmnet(x=as.matrix(train_complete[ , trk1_predictors]), 
                    y=train_complete[ , prediction_var], 
                    family='binomial', alpha=0, lambda=lambdas)
plot(cv_fit)

cv_fit$lambda.min  # lambda with min deviance
cv_fit$lambda.1se  # 1 standard error away from the min lambda
# it's a common practice to choose lambda.1se over lambda.min

## get cross validation result
ridge1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='glmnet',
                                             alpha=0, lambda=0.0025)
ridge2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='glmnet',
                                             alpha=0, lambda=0.05)

## evaludate average model performance by threshold
ridge1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(ridge1_cv_pred_df)
ridge2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(ridge2_cv_pred_df)
ridge1_perf_by_thres_df 
ridge2_perf_by_thres_df 

## ridge1 selected



#### lasso logistic regression ####

## get cross validation result
lasso_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, 
                                            method='glmnet', alpha=1, lambda=0)

## evaludate average model performance by threshold
lasso_perf_by_thres_df <- create_bythres_cv_perf_agg_df(lasso_cv_pred_df)
lasso_perf_by_thres_df 



#### elastic net logistic regression ####

## get cross validation result
enet1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, 
                                            method='glmnet', alpha=0.5, lambda=0.0025)
enet2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, 
                                            method='glmnet', alpha=0.5, lambda=0.05)

## evaludate average model performance by threshold
enet1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(enet1_cv_pred_df)
enet2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(enet2_cv_pred_df)
enet1_perf_by_thres_df 
enet2_perf_by_thres_df 

## enet1 selected




#### model ensemble (glm, nb, knn, rf, nnet)

## collect probs from different models
glm1_cv_pred_df$prob, 
head(enet1_cv_pred_df)
