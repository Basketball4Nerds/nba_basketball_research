#### establish baseline measure
pred_base <- rs_games$line < 0
cmtx_base <- table(rs_games$outcome, pred_base)
calc_acc_fr_cnf_mtx(cmtx_base) * 100
length(pred_base)



#### prediction based on general mean points

## 5-games rolling mean points
pred_rqP_rollmean5_gen <- (rs_games$rqP_rollmean5_gen + rs_games$o_rqPA_rollmean5_gen) / 2
pred_o_rqP_rollmean5_gen <- (rs_games$rqPA_rollmean5_gen + rs_games$o_rqP_rollmean5_gen) / 2
win_pred_rqP_rollmean5_gen <- pred_rqP_rollmean5_gen > pred_o_rqP_rollmean5_gen
cmtx_rqP_rollmean5_gen <- table(rs_games$outcome, win_pred_rqP_rollmean5_gen)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_gen) * 100
sum(!is.na(win_pred_rqP_rollmean5_gen))

## 5-games rolling mean points (site adjusted)
pm <- 2
pred_rqP_rollmean5_gen_pm <- ifelse(rs_games$site=='home',
                                     pred_rqP_rollmean5_gen + pm,
                                     pred_rqP_rollmean5_gen - pm)
pred_o_rqP_rollmean5_gen_pm <- ifelse(rs_games$site=='home',
                                       pred_o_rqP_rollmean5_gen - pm,
                                       pred_o_rqP_rollmean5_gen + pm)
win_pred_rqP_rollmean5_gen_pm <- pred_rqP_rollmean5_gen_pm > pred_o_rqP_rollmean5_gen_pm
cmtx_rqP_rollmean5_gen_pm <- table(rs_games$outcome, win_pred_rqP_rollmean5_gen_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_gen_pm) * 100
sum(!is.na(win_pred_rqP_rollmean5_gen_pm))

## 5-games rolling mean points (site adjusted)
win_pred_rqP_rollmean5_gen_pms <- ifelse(pred_rqP_rollmean5_gen_pm > pred_o_rqP_rollmean5_gen_pm & rs_games$site=='home', TRUE,
                                         ifelse(pred_rqP_rollmean5_gen_pm < pred_o_rqP_rollmean5_gen_pm & rs_games$site=='away', FALSE, NA))
cmtx_rqP_rollmean5_gen_pms <- table(rs_games$outcome, win_pred_rqP_rollmean5_gen_pms)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_gen_pms) * 100
sum(!is.na(win_pred_rqP_rollmean5_gen_pms))



## 10-games rolling mean points
pred_rqP_rollmean10_gen <- (rs_games$rqP_rollmean10_gen + rs_games$o_rqPA_rollmean10_gen) / 2
pred_o_rqP_rollmean10_gen <- (rs_games$rqPA_rollmean10_gen + rs_games$o_rqP_rollmean10_gen) / 2
win_pred_rqP_rollmean10_gen <- pred_rqP_rollmean10_gen > pred_o_rqP_rollmean10_gen
cmtx_rqP_rollmean10_gen <- table(rs_games$outcome, win_pred_rqP_rollmean10_gen)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_gen) * 100
sum(!is.na(win_pred_rqP_rollmean10_gen))

## 10-games rolling mean points (site adjusted)
pred_rqP_rollmean10_gen_pm <- ifelse(rs_games$site=='home',
                                     pred_rqP_rollmean10_gen + pm,
                                     pred_rqP_rollmean10_gen - pm)
pred_o_rqP_rollmean10_gen_pm <- ifelse(rs_games$site=='home',
                                       pred_o_rqP_rollmean10_gen - pm,
                                       pred_o_rqP_rollmean10_gen + pm)
win_pred_rqP_rollmean10_gen_pm <- pred_rqP_rollmean10_gen_pm > pred_o_rqP_rollmean10_gen_pm
cmtx_rqP_rollmean10_gen_pm <- table(rs_games$outcome, win_pred_rqP_rollmean10_gen_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_gen_pm) * 100
sum(!is.na(win_pred_rqP_rollmean10_gen_pm))

## 10-games rolling mean points (site adjusted)
win_pred_rqP_rollmean10_gen_pms <- ifelse(pred_rqP_rollmean10_gen_pm > pred_o_rqP_rollmean10_gen_pm & rs_games$site=='home', TRUE,
                                         ifelse(pred_rqP_rollmean10_gen_pm < pred_o_rqP_rollmean10_gen_pm & rs_games$site=='away', FALSE, NA))
cmtx_rqP_rollmean10_gen_pms <- table(rs_games$outcome, win_pred_rqP_rollmean10_gen_pms)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_gen_pms) * 100
sum(!is.na(win_pred_rqP_rollmean10_gen_pms))



## cumulative mean points
pred_rqP_cummean_gen <- (rs_games$rqP_cummean_gen + rs_games$o_rqPA_cummean_gen) / 2
pred_o_rqP_cummean_gen <- (rs_games$rqPA_cummean_gen + rs_games$o_rqP_cummean_gen) / 2
win_pred_rqP_cummean_gen <- pred_rqP_cummean_gen > pred_o_rqP_cummean_gen
cmtx_rqP_cummean_gen <- table(rs_games$outcome, win_pred_rqP_cummean_gen)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_gen) * 100
sum(!is.na(win_pred_rqP_cummean_gen))

## cumulative mean points (site adjusted)
pred_rqP_cummean_gen_pm <- ifelse(rs_games$site=='home',
                                     pred_rqP_cummean_gen + pm,
                                     pred_rqP_cummean_gen - pm)
pred_o_rqP_cummean_gen_pm <- ifelse(rs_games$site=='home',
                                       pred_o_rqP_cummean_gen - pm,
                                       pred_o_rqP_cummean_gen + pm)
win_pred_rqP_cummean_gen_pm <- pred_rqP_cummean_gen_pm > pred_o_rqP_cummean_gen_pm
cmtx_rqP_cummean_gen_pm <- table(rs_games$outcome, win_pred_rqP_cummean_gen_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_gen_pm) * 100
sum(!is.na(win_pred_rqP_cummean_gen_pm))

## cumulative mean points (site adjusted)
win_pred_rqP_cummean_gen_pms <- ifelse(pred_rqP_cummean_gen_pm > pred_o_rqP_cummean_gen_pm & rs_games$site=='home', TRUE,
                                          ifelse(pred_rqP_cummean_gen_pm < pred_o_rqP_cummean_gen_pm & rs_games$site=='away', FALSE, NA))
cmtx_rqP_cummean_gen_pms <- table(rs_games$outcome, win_pred_rqP_cummean_gen_pms)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_gen_pms) * 100
sum(!is.na(win_pred_rqP_cummean_gen_pms))





#### prediction based on site-specific mean points

## 5-games rolling mean points
pred_rqP_rollmean5_site <- (rs_games$rqP_rollmean5_site + rs_games$o_rqPA_rollmean5_site) / 2
pred_o_rqP_rollmean5_site <- (rs_games$rqPA_rollmean5_site + rs_games$o_rqP_rollmean5_site) / 2
win_pred_rqP_rollmean5_site <- pred_rqP_rollmean5_site > pred_o_rqP_rollmean5_site
cmtx_rqP_rollmean5_site <- table(rs_games$outcome, win_pred_rqP_rollmean5_site)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_site) * 100
sum(!is.na(win_pred_rqP_rollmean5_site))

## 5-games rolling mean points (site adjusted)
pred_rqP_rollmean5_site_pm <- ifelse(rs_games$site=='home',
                                     pred_rqP_rollmean5_site + pm,
                                     pred_rqP_rollmean5_site - pm)
pred_o_rqP_rollmean5_site_pm <- ifelse(rs_games$site=='home',
                                       pred_o_rqP_rollmean5_site - pm,
                                       pred_o_rqP_rollmean5_site + pm)
win_pred_rqP_rollmean5_site_pm <- pred_rqP_rollmean5_site_pm > pred_o_rqP_rollmean5_site_pm
cmtx_rqP_rollmean5_site_pm <- table(rs_games$outcome, win_pred_rqP_rollmean5_site_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_site_pm) * 100
sum(!is.na(win_pred_rqP_rollmean5_site_pm))


## 10-games rolling mean points
pred_rqP_rollmean10_site <- (rs_games$rqP_rollmean10_site + rs_games$o_rqPA_rollmean10_site) / 2
pred_o_rqP_rollmean10_site <- (rs_games$rqPA_rollmean10_site + rs_games$o_rqP_rollmean10_site) / 2
win_pred_rqP_rollmean10_site <- pred_rqP_rollmean10_site > pred_o_rqP_rollmean10_site
cmtx_rqP_rollmean10_site <- table(rs_games$outcome, win_pred_rqP_rollmean10_site)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_site) * 100
sum(!is.na(win_pred_rqP_rollmean10_site))

## 10-games rolling mean points (site adjusted)
pred_rqP_rollmean10_site_pm <- ifelse(rs_games$site=='home',
                                      pred_rqP_rollmean10_site + pm,
                                      pred_rqP_rollmean10_site - pm)
pred_o_rqP_rollmean10_site_pm <- ifelse(rs_games$site=='home',
                                        pred_o_rqP_rollmean10_site - pm,
                                        pred_o_rqP_rollmean10_site + pm)
win_pred_rqP_rollmean10_site_pm <- pred_rqP_rollmean10_site_pm > pred_o_rqP_rollmean10_site_pm
cmtx_rqP_rollmean10_site_pm <- table(rs_games$outcome, win_pred_rqP_rollmean10_site_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_site_pm) * 100
sum(!is.na(win_pred_rqP_rollmean10_site_pm))


## cumulative mean points
pred_rqP_cummean_site <- (rs_games$rqP_cummean_site + rs_games$o_rqPA_cummean_site) / 2
pred_o_rqP_cummean_site <- (rs_games$rqPA_cummean_site + rs_games$o_rqP_cummean_site) / 2
win_pred_rqP_cummean_site <- pred_rqP_cummean_site > pred_o_rqP_cummean_site
cmtx_rqP_cummean_site <- table(rs_games$outcome, win_pred_rqP_cummean_site)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_site) * 100
sum(!is.na(win_pred_rqP_cummean_site))

## cumulative mean points (site adjusted)
pred_rqP_cummean_site_pm <- ifelse(rs_games$site=='home',
                                      pred_rqP_cummean_site + pm,
                                      pred_rqP_cummean_site - pm)
pred_o_rqP_cummean_site_pm <- ifelse(rs_games$site=='home',
                                        pred_o_rqP_cummean_site - pm,
                                        pred_o_rqP_cummean_site + pm)
win_pred_rqP_cummean_site_pm <- pred_rqP_cummean_site_pm > pred_o_rqP_cummean_site_pm
cmtx_rqP_cummean_site_pm <- table(rs_games$outcome, win_pred_rqP_cummean_site_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_site_pm) * 100
sum(!is.na(win_pred_rqP_cummean_site_pm))




#### prediction based on conference-specific rolling mean points

## 5-games rolling mean points
pred_rqP_rollmean5_cnf <- (rs_games$rqP_rollmean5_cnf + rs_games$o_rqPA_rollmean5_cnf) / 2
pred_o_rqP_rollmean5_cnf <- (rs_games$rqPA_rollmean5_cnf + rs_games$o_rqP_rollmean5_cnf) / 2
win_pred_rqP_rollmean5_cnf <- pred_rqP_rollmean5_cnf > pred_o_rqP_rollmean5_cnf
cmtx_rqP_rollmean5_cnf <- table(rs_games$outcome, win_pred_rqP_rollmean5_cnf)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_cnf) * 100
sum(!is.na(win_pred_rqP_rollmean5_cnf))

## 5-games rolling mean points (site adjusted)
pred_rqP_rollmean5_cnf_pm <- ifelse(rs_games$site=='home',
                                      pred_rqP_rollmean5_cnf + pm,
                                      pred_rqP_rollmean5_cnf - pm)
pred_o_rqP_rollmean5_cnf_pm <- ifelse(rs_games$site=='home',
                                        pred_o_rqP_rollmean5_cnf - pm,
                                        pred_o_rqP_rollmean5_cnf + pm)
win_pred_rqP_rollmean5_cnf_pm <- pred_rqP_rollmean5_cnf_pm > pred_o_rqP_rollmean5_cnf_pm
cmtx_rqP_rollmean5_cnf_pm <- table(rs_games$outcome, win_pred_rqP_rollmean5_cnf_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean5_cnf_pm) * 100
sum(!is.na(win_pred_rqP_rollmean5_cnf_pm))


## 10-games rolling mean points
pred_rqP_rollmean10_cnf <- (rs_games$rqP_rollmean10_cnf + rs_games$o_rqPA_rollmean10_cnf) / 2
pred_o_rqP_rollmean10_cnf <- (rs_games$rqPA_rollmean10_cnf + rs_games$o_rqP_rollmean10_cnf) / 2
win_pred_rqP_rollmean10_cnf <- pred_rqP_rollmean10_cnf > pred_o_rqP_rollmean10_cnf
cmtx_rqP_rollmean10_cnf <- table(rs_games$outcome, win_pred_rqP_rollmean10_cnf)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_cnf) * 100
sum(!is.na(win_pred_rqP_rollmean10_cnf))

## 10-games rolling mean points (site adjusted)
pred_rqP_rollmean10_cnf_pm <- ifelse(rs_games$site=='home',
                                       pred_rqP_rollmean10_cnf + pm,
                                       pred_rqP_rollmean10_cnf - pm)
pred_o_rqP_rollmean10_cnf_pm <- ifelse(rs_games$site=='home',
                                         pred_o_rqP_rollmean10_cnf - pm,
                                         pred_o_rqP_rollmean10_cnf + pm)
win_pred_rqP_rollmean10_cnf_pm <- pred_rqP_rollmean10_cnf_pm > pred_o_rqP_rollmean10_cnf_pm
cmtx_rqP_rollmean10_cnf_pm <- table(rs_games$outcome, win_pred_rqP_rollmean10_cnf_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_rollmean10_cnf_pm) * 100
sum(!is.na(win_pred_rqP_rollmean10_cnf_pm))


## cumulative mean points
pred_rqP_cummean_cnf <- (rs_games$rqP_cummean_cnf + rs_games$o_rqPA_cummean_cnf) / 2
pred_o_rqP_cummean_cnf <- (rs_games$rqPA_cummean_cnf + rs_games$o_rqP_cummean_cnf) / 2
win_pred_rqP_cummean_cnf <- pred_rqP_cummean_cnf > pred_o_rqP_cummean_cnf
cmtx_rqP_cummean_cnf <- table(rs_games$outcome, win_pred_rqP_cummean_cnf)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_cnf) * 100
sum(!is.na(win_pred_rqP_cummean_cnf))

## cumulative mean points (site adjusted)
pred_rqP_cummean_cnf_pm <- ifelse(rs_games$site=='home',
                                    pred_rqP_cummean_cnf + pm,
                                    pred_rqP_cummean_cnf - pm)
pred_o_rqP_cummean_cnf_pm <- ifelse(rs_games$site=='home',
                                      pred_o_rqP_cummean_cnf - pm,
                                      pred_o_rqP_cummean_cnf + pm)
win_pred_rqP_cummean_cnf_pm <- pred_rqP_cummean_cnf_pm > pred_o_rqP_cummean_cnf_pm
cmtx_rqP_cummean_cnf_pm <- table(rs_games$outcome, win_pred_rqP_cummean_cnf_pm)
calc_acc_fr_cnf_mtx(cmtx_rqP_cummean_cnf_pm) * 100
sum(!is.na(win_pred_rqP_cummean_cnf_pm))



