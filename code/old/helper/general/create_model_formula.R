## this function takes in a vector of predictor variables and prediction variable
## and creates a formula to be used to create models
create_model_formula <- function(predictors, prediction_var) {
  formula <- as.formula(paste0(prediction_var, ' ~ ', paste(predictors, collapse=' + ')))
  return(formula)
}


