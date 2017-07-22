## subset df
master_df <- subset(master, season==2012)


## create and view a simple retro win prediction accuracy df
create_smpl_retro_win_pred_acc_df(master_df)


## create prediction df
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))


## 
won_df <- subset(predictive_df, won)
lost_df <- subset(predictive_df, !won)



varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=cumperf_cols, normalize=TRUE)
varimp_df <- sortByCol(varimp_df, 'spread')
varimp_df
plot_varimp(varimp_df)




## CONTINUE HERE!!!!!

library(corrplot)
wpc_cols <- names(predictive_df)[grepl('^wpc_', names(predictive_df))]
cumperf_cols <- names(predictive_df)[grepl('_cumperf_', names(predictive_df))]
oeff_cumperf_cols <- names(predictive_df)[grepl("^oeff_cumperf_", names(predictive_df), perl = TRUE)]
oeffA_cumperf_cols <- names(predictive_df)[grepl("^oeffA_cumperf_", names(predictive_df), perl = TRUE)]
FGP_cumperf_cols <- names(predictive_df)[grepl("^FGP_cumperf_", names(predictive_df), perl = TRUE)]
FGPA_cumperf_cols <- names(predictive_df)[grepl("^FGPA_cumperf_", names(predictive_df), perl = TRUE)]
rqP_cumperf_cols <- names(predictive_df)[grepl("^rqP_cumperf_", names(predictive_df), perl = TRUE)]
rqPA_cumperf_cols <- names(predictive_df)[grepl("^rqPA_cumperf_", names(predictive_df), perl = TRUE)]
pos_cumperf_cols <- names(predictive_df)[grepl("^pos_cumperf_", names(predictive_df), perl = TRUE)]
posA_cumperf_cols <- names(predictive_df)[grepl("^posA_cumperf_", names(predictive_df), perl = TRUE)]
j_cols <- names(predictive_df)[grepl("^j", names(predictive_df), perl = TRUE)]



## correlation plot
# https://stackoverflow.com/a/19115003
cor_mtx <- round(cor(predictive_df[cumperf_cols], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')

names(predictive_df)
gen_cols <- names(predictive_df)[grepl('_gen', names(predictive_df))]
gen_cols <- setdiff(gen_cols, c('n_gen', 'o_n_gen'))
cor_mtx <- round(cor(predictive_df[gen_cols], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')




