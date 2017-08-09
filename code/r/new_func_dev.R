
## get list of model candidates
names(getModelInfo())



## 
pred <- create_pred_probs(x, y, trk1_formula_orig, method='nnet', n_hidden_nodes=10)

## neural net
# seed
#decay=0.001, maxit=500, trace=FALSE
#model <- nnet(trk1_formula_orig, data=train-df, size=n_hidden_nodes, decay=0.001, maxit=500, trace=FALSE)











# model_prediction_cols <- paste0('pred_thres', seq(0.5, 0.9, by=0.1))
# # for (col %in% model_prediction_cols) {
# #   cnf_mtx
# # }
# cnf_mtx <- table(cv_pred_df$actual, cv_pred_df$pred_thres.6)
# calc_acc_fr_cnf_mtx(cnf_mtx)











