calc_pred_acc <- function(cnf_mtx) {
  return(sum(diag(cnf_mtx))/sum(cnf_mtx))
}

