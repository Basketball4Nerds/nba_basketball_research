## In this code track, we will throw in all predictor variables into the 
## high-VIF removal function (without handling any variables first)
## to remove collinear variables. We will then use cross validation to compare
## different model performances and pick the best one.


## remove highly correlated variables via VIF calc
trk2_predictors <- vif_func(in_frame=train_complete[ , predictors], 
                                  thresh=5, trace=TRUE)



## examine predictive power of the selected vars
a_trk2 <- get_pred_perf_rnk_plcmnt_lst(train_complete, 
                                  predictors=trk2_predictors, 
                                  rank_method='pred_acc', min_n=5)
lapply(a_trk2, table)


## picking top-performing predictors from top 5 rank populations
rev(sort(table(c(a_trk2[[1]], a_trk2[[2]], a_trk2[[3]], a_trk2[[4]], a_trk2[[5]]))))


## hand-picking the top performing variables (see above)
trk2_top_predictors <- c('wpc_cnf', 'j15', 'FGPA_cumperf_site', 
                             'FGP_cumperf_site', 'FGPA_cumperf_oeffQntlRnk')


## create track 2 formula
trk2_formula <- create_model_formula(trk2_top_predictors, 'won')



#### logistic regression track ####

## get cross validation result 
trk2_glm_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='glm') 

## evaludate average model performance by threshold
trk2_glm_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_glm_cv_pred_df)
trk2_glm_perf_by_thres_df



#### decision tree track ####

## get cross validation result
trk2_rpart1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='rpart')
trk2_rpart2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='rpart', control=rpart.control(cp=0.001))
trk2_rpart3_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='rpart', control=rpart.control(cp=0.003))
trk2_rpart4_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='rpart', control=rpart.control(cp=0.005))

## evaludate average model performance by threshold
trk2_rpart1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_rpart1_cv_pred_df)
trk2_rpart2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_rpart2_cv_pred_df)
trk2_rpart3_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_rpart3_cv_pred_df)
trk2_rpart4_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_rpart4_cv_pred_df)
trk2_rpart1_perf_by_thres_df
trk2_rpart2_perf_by_thres_df
trk2_rpart3_perf_by_thres_df
trk2_rpart4_perf_by_thres_df

## rpart3 selected



#### naive bayes track ####

## get cross validation result
trk2_nb_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='nb')

## evaludate average model performance by threshold
trk2_nb_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_nb_cv_pred_df)
trk2_nb_perf_by_thres_df 




#### k-nearest neighbor track ####

## get cross validation result
trk2_knn5_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='knn', k=5)
trk2_knn10_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='knn', k=10)
trk2_knn20_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='knn', k=20)
trk2_knn50_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='knn', k=50)
trk2_knn100_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='knn', k=100)

## evaludate average model performance by threshold
trk2_knn5_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_knn5_cv_pred_df)
trk2_knn10_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_knn10_cv_pred_df)
trk2_knn20_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_knn20_cv_pred_df)
trk2_knn50_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_knn50_cv_pred_df)
trk2_knn100_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_knn100_cv_pred_df)
trk2_knn5_perf_by_thres_df 
trk2_knn10_perf_by_thres_df 
trk2_knn20_perf_by_thres_df 
trk2_knn50_perf_by_thres_df 
trk2_knn100_perf_by_thres_df  

## knn-100 picked



#### random forest track ####

## get cross validation result
trk2_rf_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='rf',
                                         seed=123)

## evaludate average model performance by threshold
trk2_rf_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_rf_cv_pred_df)
trk2_rf_perf_by_thres_df 



#### neural net track ####

## calculate n hidden nodes
trk2_n_hidden_nodes <- round(length(trk2_top_predictors) * 2/3 + 1)
trk2_n_hidden_nodes

## get cross validation result
trk2_nnet1_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='nnet',
                                            decay=0.001, maxit=500, trace=FALSE, 
                                            n_hidden_nodes=n_hidden_nodes, seed=123)
trk2_nnet2_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='nnet',
                                            decay=0.003, maxit=500, trace=FALSE, 
                                            n_hidden_nodes=n_hidden_nodes, seed=123)
trk2_nnet3_cv_pred_df <- create_byssn_cv_pred_df(data_df=train_complete, formula=trk2_formula, method='nnet',
                                            decay=0.005, maxit=500, trace=FALSE, 
                                            n_hidden_nodes=n_hidden_nodes, seed=123)

## evaludate average model performance by threshold
trk2_nnet1_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_nnet1_cv_pred_df)
trk2_nnet2_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_nnet2_cv_pred_df)
trk2_nnet3_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_nnet3_cv_pred_df)
trk2_nnet1_perf_by_thres_df 
trk2_nnet2_perf_by_thres_df 
trk2_nnet3_perf_by_thres_df 

## nnet3 selected





#### model ensemble (glm, nb, knn, rf, nnet)

## collect probs from different models
trk2_ens_cv_pred_df <- data.frame(
  glm_prob = trk2_glm_cv_pred_df$prob, 
  nb_prob = trk2_nb_cv_pred_df$prob, 
  knn_prob = trk2_knn100_cv_pred_df$prob,
  rf_prob = trk2_rf_cv_pred_df$prob,
  nnet_prob = trk2_nnet3_cv_pred_df$prob
)

## check that the values in the "actual" column match
## across all cv pred perf dfs
check_equal_vectors(trk2_glm_cv_pred_df$actual,
                    trk2_nb_cv_pred_df$actual,
                    trk2_knn100_cv_pred_df$actual,
                    trk2_rf_cv_pred_df$actual,
                    trk2_nnet3_cv_pred_df$actual)

## append the actual outcome variable as a column
trk2_ens_cv_pred_df$actual <- trk2_glm_cv_pred_df$actual
trk2_ens_cv_pred_df$season <- trk2_glm_cv_pred_df$season

## average the different models' probs
trk2_ens_cv_pred_df$prob <- 
  (trk2_ens_cv_pred_df$glm_prob + 
     trk2_ens_cv_pred_df$nb_prob +
     trk2_ens_cv_pred_df$knn_prob +
     trk2_ens_cv_pred_df$rf_prob +
     trk2_ens_cv_pred_df$nnet_prob) / 5

## add prediction by threshold
trk2_ens_cv_pred_df <- add_pred_by_thres(trk2_ens_cv_pred_df, 
                                         prob_col='prob', 
                                         thres_vec=seq(0.5, 0.9, by=0.1))
head(trk2_ens_cv_pred_df)

## evaludate average performance by threshold
trk2_ens_perf_by_thres_df <- create_bythres_cv_perf_agg_df(trk2_ens_cv_pred_df)
trk2_ens_perf_by_thres_df



