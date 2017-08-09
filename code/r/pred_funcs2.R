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
  train_df[[prediction_var]] <- as.numeric(train_df[[prediction_var]])  # svm requires numeric dependent variable for regression
  model <- svm(formula, data=train_df, ...)
  pred_probs <- predict(model, newdata=test_df)
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using k-nearest neighbor method;
## note: consider including normalization step
## https://www.analyticsvidhya.com/blog/2015/08/learning-concept-knn-algorithms-programming/
create_pred_probs_knn <- function(train_df, test_df, formula, k) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  predictor_vars <- get_predictors_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  preds <- knn(train=train_df[ , predictor_vars], test=test_df[ , predictor_vars], 
               cl=train_df[ , prediction_var], k=k, prob=TRUE)
  pred_probs <- attr(preds, 'prob')
  return(pred_probs)
}


## this function returns vector of prediction probabilities on test dataset
## using naive bayes method
create_pred_probs_nb <- function(train_df, test_df, formula, ...) {
  prediction_var <- get_prediction_var_fr_formula(formula)
  predictor_vars <- get_predictors_fr_formula(formula)
  train_df[[prediction_var]] <- as.factor(train_df[[prediction_var]])
  #model <- NaiveBayes(formula, data=train_df, userkernel=TRUE)
  model <- naiveBayes(formula, data=train_df, ...)
  pred_probs <- predict(model, newdata=test_df[ , predictor_vars], type='raw')[ , 2]
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
  if (is.null(n_hidden_nodes)) { n_hidden_nodes <- round(length(predictor_vars) * 2/3 + 1) }
  
  set.seed(seed)  
  model <- nnet(formula, data=train_df, size=n_hidden_nodes, trace=trace, maxit=maxit, ...)
  pred_probs <- predict(model, newdata=test_df, type='raw')[ , 1]
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
  prediction_var <- all.vars(formula[[2]])
  predictor_vars <- all.vars(formula[[3]])

  ## get number of CV partititions based on season
  seasons <- unique(data_df$season)
  k <- length(seasons)
  
  ## create and initialize a progress bar
  progress_bar <- create_progress_bar("text")
  progress_bar$init(k)
  
  ## initialize vectors
  pred_probs_vec <- c()
  actual_outcome_vec <- c()
  target_pred_season_vec <- c()
  
  ## leave one test season out, train mode on rest of the seasons, 
  ## and test model on test season
  for (i in 1:k) {
    
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
    actual_outcome_vec <- c(actual_outcome_vec, cv_test_data_df[[prediction_var]])
    
    ## run progress bar  
    progress_bar$step()
  }
  
  
  ## construct df
  ret_df <- data.frame(prob=pred_probs_vec, 
                       actual=actual_outcome_vec, 
                       targ_season=target_pred_season_vec)
  
  
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