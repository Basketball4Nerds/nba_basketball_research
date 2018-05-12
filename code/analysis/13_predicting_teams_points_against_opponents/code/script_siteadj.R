## calculate averaged rqP projections (site adjusted)
rs_games$pred_rqP_rollmean5_gen_siteadj <- (rs_games$rqP_rollmean5_gen_siteadj + rs_games$o_rqPA_rollmean5_gen_siteadj) / 2
rs_games$pred_rqP_rollmean10_gen_siteadj <- (rs_games$rqP_rollmean10_gen_siteadj + rs_games$o_rqPA_rollmean10_gen_siteadj) / 2
rs_games$pred_rqP_cummean_gen_siteadj <- (rs_games$rqP_cummean_gen_siteadj + rs_games$o_rqPA_cummean_gen_siteadj) / 2

rs_games$pred_rqP_rollmean5_cnf_siteadj <- (rs_games$rqP_rollmean5_cnf_siteadj + rs_games$o_rqPA_rollmean5_cnf_siteadj) / 2
rs_games$pred_rqP_rollmean10_cnf_siteadj <- (rs_games$rqP_rollmean10_cnf_siteadj + rs_games$o_rqPA_rollmean10_cnf_siteadj) / 2
rs_games$pred_rqP_cummean_cnf_siteadj <- (rs_games$rqP_cummean_cnf_siteadj + rs_games$o_rqPA_cummean_cnf_siteadj) / 2



## correlation coefficient
round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean5_gen_siteadj, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean10_gen_siteadj, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_gen], rs_games$pred_rqP_cummean_gen_siteadj[ind_min_n5_gen], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_gen], rs_games$pred_rqP_cummean_gen_siteadj[ind_min_n10_gen], use='pairwise.complete.obs'), 3)

round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean5_cnf_siteadj, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean10_cnf_siteadj, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_gen], rs_games$pred_rqP_cummean_cnf_siteadj[ind_min_n5_gen], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_gen], rs_games$pred_rqP_cummean_cnf_siteadj[ind_min_n10_gen], use='pairwise.complete.obs'), 3)



## mean absolute error
round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean5_gen_siteadj), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean10_gen_siteadj), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_gen] - rs_games$pred_rqP_cummean_gen_siteadj[ind_min_n5_gen]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_gen] - rs_games$pred_rqP_cummean_gen_siteadj[ind_min_n10_gen]), na.rm=TRUE), 3)

round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean5_cnf_siteadj), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean10_cnf_siteadj), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_cnf] - rs_games$pred_rqP_cummean_cnf_siteadj[ind_min_n5_cnf]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_cnf] - rs_games$pred_rqP_cummean_cnf_siteadj[ind_min_n10_cnf]), na.rm=TRUE), 3)
