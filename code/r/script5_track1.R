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



#### decision tree track ####

## get cross validation result
rpart1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart')
rpart2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart', control=rpart.control(cp=0.001))
rpart3_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rpart', control=rpart.control(cp=0.005))

## evaludate average model performance by threshold
rpart1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart1_cv_pred_df)
rpart2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart2_cv_pred_df)
rpart3_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rpart3_cv_pred_df)
rpart1_perf_by_thres_df 
rpart2_perf_by_thres_df 
rpart3_perf_by_thres_df 



#### support vector machine track ####

## takes too long; approach abandoned

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

## evaludate average model performance by threshold
knn5_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn5_cv_pred_df)
knn10_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn10_cv_pred_df)
knn20_perf_by_thres_df <- create_bythres_cv_perf_agg_df(knn20_cv_pred_df)
knn5_perf_by_thres_df 
knn10_perf_by_thres_df 
knn20_perf_by_thres_df 

x <- subset(train_complete, season==2012)
y <- subset(train_complete, season==2013)

pred <- knn(train=scale(x[ , trk1_predictors]), test=scale(y[ , trk1_predictors]),
             cl=x$won, k=5, prob=TRUE)
pred_probs <- attr(pred, 'prob')



#### random forest track ####

## get cross validation result
rf_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rf',
                                         seed=123)
rf_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rf',
                                         seed=123)
rf_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='rf',
                                         seed=123)

## evaludate average model performance by threshold
rf_perf_by_thres_df <- create_bythres_cv_perf_agg_df(rf_cv_pred_df)
rf_perf_by_thres_df 



#### neural net track ####

## get cross validation result
nnet_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk1_orig_formula, method='nnet',
                                           decay=0.001, maxit=500, trace=FALSE, seed=123)

## evaludate average model performance by threshold
nnet_perf_by_thres_df <- create_bythres_cv_perf_agg_df(nnet_cv_pred_df)
nnet_perf_by_thres_df 



#### ridge logistic regression ####


#### lasso logistic regression ####



#### elastic net logistic regression ####
