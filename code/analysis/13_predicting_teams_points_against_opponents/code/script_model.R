#### modeling using raw rolling and cumulative averages

## general 
model_rollmean5_gen <- lm(rqP ~ rqP_rollmean5_gen + o_rqPA_rollmean5_gen + site + cnf + o_cnf, data=rs_games)
model_rollmean10_gen <- lm(rqP ~ rqP_rollmean10_gen + o_rqPA_rollmean10_gen + site + cnf + o_cnf, data=rs_games)
model_cummean_gen <- lm(rqP ~ rqP_cummean_gen + o_rqPA_cummean_gen + site + cnf + o_cnf, data=rs_games[ind_min_n10_gen, ])

## site-specific
model_rollmean5_site <- lm(rqP ~ rqP_rollmean5_site + o_rqPA_rollmean5_site + site + cnf + o_cnf, data=rs_games)
model_rollmean10_site <- lm(rqP ~ rqP_rollmean10_site + o_rqPA_rollmean10_site + site + cnf + o_cnf, data=rs_games)
model_cummean_site <- lm(rqP ~ rqP_cummean_site + o_rqPA_cummean_site + site + cnf + o_cnf, data=rs_games[ind_min_n10_gen, ])

## conference-specific
model_rollmean5_cnf <- lm(rqP ~ rqP_rollmean5_cnf + o_rqPA_rollmean5_cnf + site + cnf + o_cnf, data=rs_games)
model_rollmean10_cnf <- lm(rqP ~ rqP_rollmean10_cnf + o_rqPA_rollmean10_cnf + site + cnf + o_cnf, data=rs_games)
model_cummean_cnf <- lm(rqP ~ rqP_cummean_cnf + o_rqPA_cummean_cnf + site + cnf + o_cnf, data=rs_games[ind_min_n10_gen, ])



#### modeling using averaged rolling and cumulative averages of scored and allowed

## general
model_pred_rollmean5_gen <- lm(rqP ~ pred_rqP_rollmean5_gen + site + cnf + o_cnf, data=rs_games)
model_pred_rollmean10_gen <- lm(rqP ~ pred_rqP_rollmean10_gen + site + cnf + o_cnf, data=rs_games)
model_pred_cummean_gen <- lm(rqP ~ pred_rqP_cummean_gen + site + cnf + o_cnf, data=rs_games[ind_min_n10_gen, ])

## site-specific
model_pred_rollmean5_site <- lm(rqP ~ pred_rqP_rollmean5_site + site + cnf + o_cnf, data=rs_games)
model_pred_rollmean10_site <- lm(rqP ~ pred_rqP_rollmean10_site + site + cnf + o_cnf, data=rs_games)
model_pred_cummean_site <- lm(rqP ~ pred_rqP_cummean_site + site + cnf + o_cnf, data=rs_games[ind_min_n10_gen, ])

## conference-specific
model_pred_rollmean5_cnf <- lm(rqP ~ pred_rqP_rollmean5_cnf + site + cnf + o_cnf, data=rs_games)
model_pred_rollmean10_cnf <- lm(rqP ~ pred_rqP_rollmean10_cnf + site + cnf + o_cnf, data=rs_games)
model_pred_cummean_cnf <- lm(rqP ~ pred_rqP_cummean_cnf + site + cnf + o_cnf, data=rs_games[ind_min_n10_gen, ])



#### 
mean(abs(model_rollmean5_gen$residuals))
mean(abs(model_rollmean10_gen$residuals))
mean(abs(model_cummean_gen$residuals))

mean(abs(model_rollmean5_site$residuals))
mean(abs(model_rollmean10_site$residuals))
mean(abs(model_cummean_site$residuals))

mean(abs(model_rollmean5_cnf$residuals))
mean(abs(model_rollmean10_cnf$residuals))
mean(abs(model_cummean_cnf$residuals))


####
mean(abs(model_pred_rollmean5_gen$residual))
mean(abs(model_pred_rollmean10_gen$residual))
mean(abs(model_pred_cummean_gen$residual))

mean(abs(model_pred_rollmean5_site$residual))
mean(abs(model_pred_rollmean10_site$residual))
mean(abs(model_pred_cummean_site$residual))

mean(abs(model_pred_rollmean5_cnf$residual))
mean(abs(model_pred_rollmean10_cnf$residual))
mean(abs(model_pred_cummean_cnf$residual))






####
model2_rollmean5_gen <- lm(rqP ~ 
                            rqP_rollmean5_gen + o_rqPA_rollmean5_gen + 
                            rqPA_rollmean5_gen + o_rqP_rollmean5_gen + 
                            site + cnf + o_cnf, data=rs_games)
summary(model2_rollmean5_gen)

model2_rollmean10_gen <- lm(rqP ~ 
                            rqP_rollmean10_gen + o_rqPA_rollmean10_gen + 
                            rqPA_rollmean10_gen + o_rqP_rollmean10_gen + 
                            site + cnf + o_cnf, data=rs_games)
summary(model2_rollmean10_gen)

model2_cummean_gen <- lm(rqP ~ 
                              rqP_cummean_gen + o_rqPA_cummean_gen + 
                              rqPA_cummean_gen + o_rqP_cummean_gen + 
                              site + cnf + o_cnf, data=rs_games)
summary(model2_cummean_gen)


