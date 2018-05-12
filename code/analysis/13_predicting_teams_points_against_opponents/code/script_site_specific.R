## create indices for data points with minimum n_site
ind_min_n5_site <- rs_games$n_site >= 5 & rs_games$o_n_site >= 5
ind_min_n10_site <- rs_games$n_site >= 10 & rs_games$o_n_site >= 10


## add averaged rqP projections
rs_games$pred_rqP_rollmean5_site <- (rs_games$rqP_rollmean5_site + rs_games$o_rqPA_rollmean5_site) / 2
rs_games$pred_rqP_rollmean10_site <- (rs_games$rqP_rollmean10_site + rs_games$o_rqPA_rollmean10_site) / 2
rs_games$pred_rqP_cummean_site <- (rs_games$rqP_cummean_site + rs_games$o_rqPA_cummean_site) / 2

rs_games$o_pred_rqP_rollmean5_site <- (rs_games$o_rqP_rollmean5_site + rs_games$rqPA_rollmean5_site) / 2
rs_games$o_pred_rqP_rollmean10_site <- (rs_games$o_rqP_rollmean10_site + rs_games$rqPA_rollmean10_site) / 2
rs_games$o_pred_rqP_cummean_site <- (rs_games$o_rqP_cummean_site + rs_games$rqPA_cummean_site) / 2



## correlation between prediction var and predictor vars
round(cor(rs_games$rqP, rs_games$rqP_rollmean5_site, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$rqP_rollmean10_site, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_site], rs_games$rqP_cummean_site[ind_min_n5_site], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_site], rs_games$rqP_cummean_site[ind_min_n10_site], use='pairwise.complete.obs'), 3)

round(cor(rs_games$rqP, rs_games$o_rqPA_rollmean5_site, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$o_rqPA_rollmean10_site, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_site], rs_games$o_rqPA_cummean_site[ind_min_n5_site], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_site], rs_games$o_rqPA_cummean_site[ind_min_n10_site], use='pairwise.complete.obs'), 3)

round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean5_site, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP, rs_games$pred_rqP_rollmean10_site, use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n5_site], rs_games$pred_rqP_cummean_site[ind_min_n5_site], use='pairwise.complete.obs'), 3)
round(cor(rs_games$rqP[ind_min_n10_site], rs_games$pred_rqP_cummean_site[ind_min_n10_site], use='pairwise.complete.obs'), 3)


## mean absolute error
round(mean(abs(rs_games$rqP - rs_games$rqP_rollmean5_site), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$rqP_rollmean10_site), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_site] - rs_games$rqP_cummean_site[ind_min_n5_site]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_site] - rs_games$rqP_cummean_site[ind_min_n10_site]), na.rm=TRUE), 3)

round(mean(abs(rs_games$rqP - rs_games$o_rqPA_rollmean5_site), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$o_rqPA_rollmean10_site), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_site] - rs_games$o_rqPA_cummean_site[ind_min_n5_site]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_site] - rs_games$o_rqPA_cummean_site[ind_min_n10_site]), na.rm=TRUE), 3)

round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean5_site), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP - rs_games$pred_rqP_rollmean10_site), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n5_site] - rs_games$pred_rqP_cummean_site[ind_min_n5_site]), na.rm=TRUE), 3)
round(mean(abs(rs_games$rqP[ind_min_n10_site] - rs_games$pred_rqP_cummean_site[ind_min_n10_site]), na.rm=TRUE), 3)
