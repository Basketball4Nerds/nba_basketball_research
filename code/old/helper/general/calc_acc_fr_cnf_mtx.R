
## this function calculates accuracy from confusion matrix
calc_acc_fr_cnf_mtx <- function(cnf_mtx, rnd_dgt=3) {
  cnf_mtx <- as.matrix(cnf_mtx)
  acc_pc <- sum(diag(cnf_mtx)) / sum(cnf_mtx)
  acc_pc <- round(acc_pc, rnd_dgt)
  return(acc_pc)
}


