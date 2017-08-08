
## this function takes data, performs leave-one-season-out
## cross validation, and returns df that contains model results
## by season
create_byssn_cv_pred_df <- function(data_df, formula) {

  ## extract prediction variable from formula
  prediction_var <- all.vars(formula[[2]])
  
  
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
    
    ## create model using data from k-1 folds 
    model <- glm(formula, data=cv_train_data_df, family='binomial')
    
    ## get prediction probability on the cv test set
    pred_probs <- predict(model, newdata=cv_test_data_df, type='response')
    
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


model_prediction_cols <- paste0('pred_thres', seq(0.5, 0.9, by=0.1))
# for (col %in% model_prediction_cols) {
#   cnf_mtx
# }
cnf_mtx <- table(cv_pred_df$actual, cv_pred_df$pred_thres.6)
calc_acc_fr_cnf_mtx(cnf_mtx)


## testing
x <- subset(train_complete, season %in% 1995)
y <- subset(train_complete, season %in% 1996)

## for rpart (need the prediction variable to be a factor)
model <- rpart(trk1_formula_orig, data=x)
prp(model)
pred <- predict(model, newdata=y, type='prob')[ , 2]
cnf_mtx <- table(pred, y$won)
calc_acc_fr_cnf_mtx(cnf_mtx)

## for svm 

## for naive bayes

## for knn

#trk1_rf_model$results$Accuracy
#trk1_gbm_model$results$Accuracy

## 




x <- subset(train_complete, season %in% 1995)
y <- subset(train_complete, season %in% 1996)
model <- rpart(as.factor(won) ~ line, data=x)
trk1_formula_orig
prp(model)




