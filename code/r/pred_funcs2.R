## this function returns vector of prediction probabilities on test dataset
## using logistic regression method
create_pred_probs_glm <- function(train_df, test_df, formula, ...) {
  model <- glm(formula, data=train_df, family='binomial', ...)
  pred_probs <- predict(model, newdata=test_df, type='response')
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using decision tree method
create_pred_probs_rpart <- function(train_df, test_df, formula, ...) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])  # rpart requires prediction variable to be factor
  model <- rpart(formula, data=train_df, ...)
  pred_probs <- predict(model, newdata=test_df, type='prob')[ , 2]
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using linear support vector machine method
create_pred_probs_svmlinear <- function(train_df, test_df, formula, ...) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  train_df[[prediction_var]] <- as.numeric(train_df[[prediction_var]]) - 1  # svm requires numeric dependent variable for regression; converts to 0s and 1s
  model <- svm(formula, data=train_df, ...)
  pred_probs <- predict(model, newdata=test_df)
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using k-nearest neighbor method;
## https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
create_pred_probs_knn <- function(train_df, test_df, formula, k) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  predictors <- get_predictors_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  preds <- knn(train=scale(train_df[ , predictors]), test=scale(test_df[ , predictors]),  # entering normalized train and test datasets
               cl=train_df[ , prediction_var], k=k, prob=TRUE)
  pred_probs <- attr(preds, 'prob')  # win/loss pred prob (mixed and mashed)
  pred_probs[preds=='FALSE'] <- 1 - pred_probs[preds=='FALSE']  # win pred prob
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using naive bayes method
create_pred_probs_nb <- function(train_df, test_df, formula, ...) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  predictors <- get_predictors_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  #model <- NaiveBayes(formula, data=train_df, userkernel=TRUE)
  model <- naiveBayes(formula, data=train_df, ...)
  pred_probs <- predict(model, newdata=test_df[ , predictors], type='raw')[ , 2]
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using random forest method
create_pred_probs_rf <- function(train_df, test_df, formula, seed=123, ...) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  set.seed(seed)
  model <- randomForest(formula, data=train_df, ...)
  pred_probs <- predict(model, newdata=test_df, type='prob')[ , 2]
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using neural network method
create_pred_probs_nnet <- function(train_df, test_df, formula, n_hidden_nodes=NULL, seed=123, trace=TRUE, maxit=500, ...) {
  
  prediction_var <- get_prediction_var_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  
  # get number of hidden nodes to use if not set: https://stats.stackexchange.com/a/180052
  if (is.null(n_hidden_nodes)) { n_hidden_nodes <- round(length(predictors) * 2/3 + 1) }
  
  set.seed(seed)  
  model <- nnet(formula, data=train_df, size=n_hidden_nodes, trace=trace, maxit=maxit, ...)
  pred_probs <- predict(model, newdata=test_df, type='raw')[ , 1]
  return(pred_probs)
}



## this function returns vector of prediction probabilities on test dataset
## using ridge, lasso, or elastic net regression method;
## - ridge regression settings: alpha = 0, lambda = non-zero
## - lasso regression settings: alpha = 1, lambda = 0
## - elastic net settings: alpha = 0 ~ 1, lambda = non-zero
## http://machinelearningmastery.com/penalized-regression-in-r/
## https://drsimonj.svbtle.com/ridge-regression-with-glmnet
create_pred_probs_glmnet <- function(train_df, test_df, formula, lambda, alpha, ...) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  predictors <- get_predictors_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  model <- glmnet(x=as.matrix(train_df[ , predictors]), 
                  y=train_df[ , prediction_var], 
                  family='binomial', alpha=alpha, lambda=lambda)
  pred_probs <- predict(model, newx=as.matrix(test_df[ , predictors]), type='response')
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using machine learning method specified
create_pred_probs <- function(train_df, test_df, formula, method, ...) {
  
  if (method=='glm') {
    pred_probs <- create_pred_probs_glm(train_df, test_df, formula, ...)
  } else if (method=='rpart') {
    pred_probs <- create_pred_probs_rpart(train_df, test_df, formula, ...)
  } else if (method=='svmlinear') {
    pred_probs <- create_pred_probs_svmlinear(train_df, test_df, formula, ...)
  } else if (method=='knn') {
    pred_probs <- create_pred_probs_knn(train_df, test_df, formula, ...)
  } else if (method=='nb') {
    pred_probs <- create_pred_probs_nb(train_df, test_df, formula, ...)
  } else if (method=='rf') {
    pred_probs <- create_pred_probs_rf(train_df, test_df, formula, ...)
  } else if (method=='nnet') {
    pred_probs <- create_pred_probs_nnet(train_df, test_df, formula, ...)
  } else if (method=='glmnet') {
    pred_probs <- create_pred_probs_glmnet(train_df, test_df, formula, ...)
  } else {
    stop(paste0("Unable to handle method: ", method))
  }
  return(pred_probs)
}


## this function takes data, performs leave-one-season-out
## cross validation, and returns df that contains model results
## by season
create_byssn_cv_pred_df <- function(data_df, formula, method, ...) {
  
  ## extract prediction variable from formula
  prediction_var <- get_prediction_var_fr_formula(formula)
  predictors <- get_predictors_fr_formula(formula)

  ## get number of CV partititions based on season
  seasons <- unique(data_df$season)
  n_folds <- length(seasons)
  
  ## create and initialize a progress bar
  progress_bar <- create_progress_bar("text")
  progress_bar$init(n_folds)
  
  ## initialize vectors
  target_pred_season_vec <- c()
  actual_outcome_vec <- c()
  pred_probs_vec <- c()
  
  ## leave one test season out, train mode on rest of the seasons, 
  ## and test model on test season
  for (i in 1:n_folds) {
    
    ## get season to exclude from training (test season)
    test_season <- seasons[i]
    
    ## split to train and test datasets
    cv_train_data_df <- subset(data_df, season!=test_season)
    cv_test_data_df <- subset(data_df, season==test_season)
    
    ## create model using (k-1) folds and get prediction probs
    pred_probs <- create_pred_probs(train_df=cv_train_data_df, 
                                    test_df=cv_test_data_df, 
                                    formula=formula, method=method, ...)
    
    ## save results by appending to vectors
    pred_probs_vec <- c(pred_probs_vec, pred_probs)
    target_pred_season_vec <- c(target_pred_season_vec, cv_test_data_df[['season']])
    actual_outcome_vec <- c(actual_outcome_vec, as.character(cv_test_data_df[[prediction_var]]))
    
    ## run progress bar  
    progress_bar$step()
  }
  
  ## construct df
  ret_df <- data.frame(season=target_pred_season_vec,
                       actual=actual_outcome_vec,
                       prob=pred_probs_vec)
  
  ## add model prediction by threshold
  ret_df$pred_thres.5 <- ifelse(ret_df$prob > 0.5, TRUE, 
                                ifelse(ret_df$prob < 0.5, FALSE, NA))
  ret_df$pred_thres.6 <- ifelse(ret_df$prob > 0.6, TRUE,
                                ifelse(ret_df$prob < 0.4, FALSE, NA))
  ret_df$pred_thres.7 <- ifelse(ret_df$prob > 0.7, TRUE,
                                ifelse(ret_df$prob < 0.3, FALSE, NA))
  ret_df$pred_thres.8 <- ifelse(ret_df$prob > 0.8, TRUE,
                                ifelse(ret_df$prob < 0.2, FALSE, NA))
  ret_df$pred_thres.9 <- ifelse(ret_df$prob > 0.9, TRUE,
                                ifelse(ret_df$prob < 0.1, FALSE, NA))
  
  ## return
  return(ret_df)
}


## this function takes by-season CV perf df and 
## performs by-season-and-threshold-combo aggregation
create_byssnthres_cv_perf_agg_df <- function(cv_pred_df) {
  
  ## generate model prediction cols
  model_prediction_cols <- gsub('0\\.', '.', paste0('pred_thres', seq(0.5, 0.9, by=0.1)))
  
  ## melt cv_pred_df
  mltd_cv_pred_df <- melt(data=cv_pred_df,
                          id.vars=c('season', 'actual', 'prob'),
                          measure.vars=model_prediction_cols,
                          variable.name='thres', value.name='pred')
  mltd_cv_pred_df$thres <- as.numeric(gsub('^pred_thres', '', mltd_cv_pred_df$thres))
  
  ## for each season-threshold combo
  agg_df <- ddply(mltd_cv_pred_df, c('season', 'thres'), function(x) {
    cnf_mtx <- table(x$actual, x$pred)
    acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
    n_preds <- sum(cnf_mtx)
    data.frame(acc, n_preds)
  })
  
  ## return
  return(agg_df)
}


## this function takes in by-season-and-threshold CV perf aggregation df
## and returns df of average performance, number of predictions, or 
## winning bet amount earned by threshold;
## note: later, include bet earn amount as a column
create_bythres_cv_perf_agg_df <- function(cv_pred_df) {
  
  ## create by-season-threshold cv perf agg df
  byssnthres_cv_perf_agg_df <- create_byssnthres_cv_perf_agg_df(cv_pred_df)

  ## aggregate by threshold
  agg_df <- ddply(byssnthres_cv_perf_agg_df, 'thres', function(x) {
    data.frame(mean(x$acc, na.rm=TRUE), mean(x$n_pred, na.rm=TRUE))
  })

  ## name agg df
  names(agg_df) <- c('thres', 'mean_acc', 'mean_n_pred')
  
  ## replae NaN with NA
  agg_df$mean_acc[is.nan(agg_df$mean_acc)] <- NA
  
  ## rounding
  agg_df$mean_acc <- round(agg_df$mean_acc, 3)
  agg_df$mean_n_pred <- round(agg_df$mean_n_pred)
  
  ## return
  return(agg_df)
}


## this function takes in linear regression model and 
## returns a vector of predictor variables that are deemed
## statistically significant
get_statsig_predictors_fr_lm <- function(model, p_val=0.05) {
  statsig_predictors <- summary(model)$coefficients[ , 4] < p_val
  statsig_predictors <- names(statsig_predictors[statsig_predictors])
  statsig_predictors <- setdiff(statsig_predictors, '(Intercept)')
  return(statsig_predictors)
}
