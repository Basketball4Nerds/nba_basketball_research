## this function finds best predictor variable by 




c('A', 'B', 'C', 'D', 'E')
c('')

## this function takes in a list of vectors and returns
first_place <- c(a, b, a, c, a, a, c, d)
second_place <- c()

higher_val_bttr_perf=TRUE


rnkd_metrics <- predictor_vars <- c('a', 'b', 'c')






find_top_perf_predictor <- function(predictive_df, predictor_vars, ...) {
  
  wpa_df <- create_win_pred_acc_df(predictive_df, metric_cols=wpc_cols, min_n=5)
  vars <- wpa_df$metric 
  acc <- wpa_df$acc
  
  
  
}
