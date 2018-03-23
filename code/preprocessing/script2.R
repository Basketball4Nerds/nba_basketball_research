
##
source('../helper/round_df.R')
source('../helper/create_new_cum_colnm.R')
source('../helper/camelCase.R')
source('helper/moving_avg_funcs/add_cumsum_cols.R')

source('helper/moving_avg_funcs/add_cumperf_cols.R')
source('../helper/fill_in_opp_cols.R')
source('helper/ranking/rnk_grp_funcs.R')
library(reshape2)
source('../helper/create_empty_df_copy.R')
source('helper/moving_avg_funcs/add_cumcnt_cols.R')
source('../helper/assure_correct_vary_by_vars.R')





## exclude playoff games (for now since playoff data is not available for 1995 - 2001)
master <- subset(master, playoffs==0)  # flaw in data; does not filter out all playoff games
x <- ddply(master, c('team', 'season'), nrow)
unique(x$V1)


## add juice propagation columns (j and o_j)
source('helper/classes.R')
source('helper/addJCols.R')
master <- addJCols(master, init_j=100, dist_wgts=c(0.05, 0.1, 0.15))


## round digits to 3 decimal places
master <- round_df(master, 3)


## set metrics to calculate their cumulative sums
cols_to_cumsum <- c('p', 'pA', 'pos', 'posA', 'FGM', 'FGMA', 'FGA', 'FGAA', 'rqP', 'rqPA')


## add general cumsum columns
master <- add_cumsum_cols(master, 
                          cols=cols_to_cumsum,
                          vary_by=NULL,
                          add_opp_cols=FALSE)


## add general cumulative performance columns
master <- add_cumperf_cols(master, add_opp_cols=TRUE)


## 
# x <- create_rnkd_tm_std_by_date_df(master, 'oeff_cumperf_gen', TRUE, method='qntl')
# 
# ## create df with relevant columns
# df <- master[, c('date', 'team', 'p')]
# 
# ## create an initial standing-by-date df that contains NA "holes"
# x <- dcast(df, date ~ team, value.var='p')
# head(df)
# dim(df)
# dim(master)

## add columns for offensive and defensive rankings
master <- add_rnk_cols(master,
                       metric=c('oeff_cumperf_gen', 'oeffA_cumperf_gen'),
                       higher_num_bttr_perf=c(TRUE, FALSE),
                       method='qntl',
                       add_opp_cols=TRUE)


## set vary-by variable for variable-specific counts, sums, and performances
vary_by <- c('site', 'o_cnf', 'o_oeff_qntl_rnk', 'o_oeffA_qntl_rnk')


## add variable-specific win cumcnt
master <- add_cumcnt_cols(master, 
                          cols=c('w', 'n'), 
                          vary_by=vary_by, 
                          add_opp_cols=TRUE)


## add variable-specific cumsum cols 
master <- add_cumsum_cols(master, 
                          cols=cols_to_cumsum,
                          vary_by=vary_by,
                          add_opp_cols=FALSE)


## add variable-specific performance
master <- add_cumperf_cols(master, add_opp_cols=TRUE)


## add win percentages (both general and variable-specific)
source('../helper/is_nan_data_frame.R')
source('helper/add_wpc_cols.R')
master <- add_wpc_cols(master, add_opp_cols=TRUE)


## list completely empty columns
source('../helper/list_empty_cols.R')
list_empty_cols(master)


## list cols that contain Inf or NaN vals
source('../helper/list_cols_w_inf_or_nan.R')
list_cols_w_inf_or_nan(master)


## make a backup
write.csv(master, '../../data/regular_season_games_master_2.csv', row.names=FALSE)
# master <- read.csv('../../data/regular_season_games_master_2.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)



