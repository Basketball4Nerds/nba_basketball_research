## create indices for data points with minimum n_cnf
ind_min_n5_cnf <- rs_games$n_cnf >= 5 & rs_games$o_n_cnf >= 5
ind_min_n10_cnf <- rs_games$n_cnf >= 10 & rs_games$o_n_cnf >= 10


## add averaged rqP projections
rs_games$pred_rqP_rollmean5_cnf <- (rs_games$rqP_rollmean5_cnf + rs_games$o_rqPA_rollmean5_cnf) / 2
rs_games$pred_rqP_rollmean10_cnf <- (rs_games$rqP_rollmean10_cnf + rs_games$o_rqPA_rollmean10_cnf) / 2
rs_games$pred_rqP_cummean_cnf <- (rs_games$rqP_cummean_cnf + rs_games$o_rqPA_cummean_cnf) / 2

rs_games$o_pred_rqP_rollmean5_cnf <- (rs_games$o_rqP_rollmean5_cnf + rs_games$rqPA_rollmean5_cnf) / 2
rs_games$o_pred_rqP_rollmean10_cnf <- (rs_games$o_rqP_rollmean10_cnf + rs_games$rqPA_rollmean10_cnf) / 2
rs_games$o_pred_rqP_cummean_cnf <- (rs_games$o_rqP_cummean_cnf + rs_games$rqPA_cummean_cnf) / 2


## correlation between prediction var and predictor vars
cor(rs_games$rqP, rs_games$rqP_rollmean5_cnf, use='pairwise.complete.obs')
cor(rs_games$rqP, rs_games$rqP_rollmean10_cnf, use='pairwise.complete.obs')
cor(rs_games$rqP[ind_min_n5_cnf], rs_games$rqP_cummean_cnf[ind_min_n5_cnf], use='pairwise.complete.obs')
cor(rs_games$rqP[ind_min_n10_cnf], rs_games$rqP_cummean_cnf[ind_min_n10_cnf], use='pairwise.complete.obs')

cor(rs_games$rqP, rs_games$o_rqPA_rollmean5_cnf, use='pairwise.complete.obs')
cor(rs_games$rqP, rs_games$o_rqPA_rollmean10_cnf, use='pairwise.complete.obs')
cor(rs_games$rqP[ind_min_n5_cnf], rs_games$o_rqPA_cummean_cnf[ind_min_n5_cnf], use='pairwise.complete.obs')
cor(rs_games$rqP[ind_min_n10_cnf], rs_games$o_rqPA_cummean_cnf[ind_min_n10_cnf], use='pairwise.complete.obs')

cor(rs_games$rqP, rs_games$pred_rqP_rollmean5_cnf, use='pairwise.complete.obs')
cor(rs_games$rqP, rs_games$pred_rqP_rollmean10_cnf, use='pairwise.complete.obs')
cor(rs_games$rqP[ind_min_n5_cnf], rs_games$pred_rqP_cummean_cnf[ind_min_n5_cnf], use='pairwise.complete.obs')
cor(rs_games$rqP[ind_min_n10_cnf], rs_games$pred_rqP_cummean_cnf[ind_min_n10_cnf], use='pairwise.complete.obs')


## mean absolute error
mean(abs(rs_games$rqP - rs_games$rqP_rollmean5_cnf), na.rm=TRUE)
mean(abs(rs_games$rqP - rs_games$rqP_rollmean10_cnf), na.rm=TRUE)
mean(abs(rs_games$rqP[ind_min_n5_cnf] - rs_games$rqP_cummean_cnf[ind_min_n5_cnf]), na.rm=TRUE)
mean(abs(rs_games$rqP[ind_min_n10_cnf] - rs_games$rqP_cummean_cnf[ind_min_n10_cnf]), na.rm=TRUE)

mean(abs(rs_games$rqP - rs_games$o_rqPA_rollmean5_cnf), na.rm=TRUE)
mean(abs(rs_games$rqP - rs_games$o_rqPA_rollmean10_cnf), na.rm=TRUE)
mean(abs(rs_games$rqP[ind_min_n5_cnf] - rs_games$o_rqPA_cummean_cnf[ind_min_n5_cnf]), na.rm=TRUE)
mean(abs(rs_games$rqP[ind_min_n10_cnf] - rs_games$o_rqPA_cummean_cnf[ind_min_n10_cnf]), na.rm=TRUE)

mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean5_cnf), na.rm=TRUE)
mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean10_cnf), na.rm=TRUE)
mean(abs(rs_games$rqP[ind_min_n5_cnf] - rs_games$pred_rqP_cummean_cnf[ind_min_n5_cnf]), na.rm=TRUE)
mean(abs(rs_games$rqP[ind_min_n10_cnf] - rs_games$pred_rqP_cummean_cnf[ind_min_n10_cnf]), na.rm=TRUE)
