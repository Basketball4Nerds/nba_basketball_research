## this function returns predictor variables from formula
get_predictors_fr_formula <- function (formula) {
  return(all.vars(formula[[3]]))
}


