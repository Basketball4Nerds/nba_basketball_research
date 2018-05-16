## this function returns prediction variable from formula
get_prediction_var_fr_formula <- function(formula) {
  return(all.vars(formula[[2]]))
}


