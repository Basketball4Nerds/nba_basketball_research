
## get list of model candidates
names(getModelInfo())


## this function returns vector of prediction probabilities on test dataset
create_pred_probs_glm(train_df, test_df, formula) {
  
  ## create model using data from k-1 folds 
  model <- glm(formula, data=, family='binomial')      
  
  ## get prediction probability on the cv test set
  pred_probs <- predict(model, newdata=, type='response')
  
  ## return
  return(pred_probs)
}



## this function takes data, performs leave-one-season-out
## cross validation, and returns df that contains model results
## by season
create_byssn_cv_pred_df <- function(data_df, formula) {

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
    
    ## create model here and create prediction probs here
    cv_train_data_df
    cv_test_data_df
    if (method=='glm') {
      create_pred_probs_glm()
    }
    else if (method=='rpart') {
      create_pred_probs_rpart()
    }
    
    else if (method=='svm_linear') {
      create_pred_probs_svmlinear()
    }
    
    
    
    
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

cv_pred_df <- create_by_ssn_cv_pred_df(train_complete, trk1_formula_glm1)
range(cv_pred_df$prob)
hist(cv_pred_df$prob)




## for naive bayes
class(x$won)
head(x$won)
head(y$won)
sapply(x, class)


create_pred_probs_nb <- function() {
  
  naive
  e1071::naiveBayes
  ?klaR::NaiveBayes
  
  model <- NaiveBayes(trk1_formula_orig, data=x)
  pred <- predict(model, newdata=y, type='class')  
}

## for knn
create_pred_knn <- function() {
  
  predictor_vars <- XXXX
  
  # need for normalization??
  x$won <- as.logical(x$won)
  pred <- knn(train=x[ , predictor_vars], test=y[ , predictor_vars], cl=x[ , prediction_var], k=10)
  
  ## return 
  return(pred)
}

create_pred_probs_rf <- function(train_df, test_df, formula, seed=123) {
  
  ## for random forest (must set seed)
  set.seed(seed)
  
  x$won <- as.factor(x$won)
  model <- randomForest(trk1_formula_orig, data=x)
  pred <- predict(model, newdata=y, type='prob')[ , 2]

  ## return
  return(pred_probs)
}



## for neural network
create_pred_prob_nnet(train_df, test_df, formula, n_hid_nds=NULL, seed=123) {

  ## set seed
  set.seed(seed)
  
  x$won <- as.factor(x$won)
  ## get number of hidden nodes to use
  # https://stats.stackexchange.com/a/180052
  if (is.null(n_hid_nds)) {
    n_hid_nds <- round(length(predictor_vars) * 2/3 + 1)    
  }

  
  model <- nnet(trk1_formula_orig, data=x, size=n_hid_nds, decay=0.001, maxit=500, trace=FALSE)
  pred_probs <- predict(model, newdata=y, type='raw')[ , 1]
  
  ## return
  return(pred_probs)
}







model_prediction_cols <- paste0('pred_thres', seq(0.5, 0.9, by=0.1))
# for (col %in% model_prediction_cols) {
#   cnf_mtx
# }
cnf_mtx <- table(cv_pred_df$actual, cv_pred_df$pred_thres.6)
calc_acc_fr_cnf_mtx(cnf_mtx)






create_pred_probs_rpart(train_df, test_df, formula) {
  
  ## for rpart (need the prediction variable to be a factor)
  x$won <- as.factor(x$won)
  model <- rpart(trk1_formula_orig, data=x)
  pred <- predict(model, newdata=y, type='prob')[ , 2]
  
}

create_pred_probs_svmlinear() {
  ## for svm  (need numeric dependent variable for regression)
  x$won <- as.numeric(x$won)
  model <- svm(trk1_formula_orig, data=x)
  pred <- predict(model, newdata=y)
  
}





