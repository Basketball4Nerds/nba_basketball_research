# ## master df derivative that contains only the following:
# ## - base columns
# ## - previous cumulative count columns
# ## - previous cumulative sum columns 
# base_cols <- c('season', 'date', 'site', 'playoffs', 'team', 'o_team', 'cnf', 'o_cnf', 'gid',
#                'n_cumcnt_gen', 'w_cumcnt_gen', 'l_cumcnt_gen',
#                'o_n_cumcnt_gen', 'o_w_cumcnt_gen', 'o_l_cumcnt_gen',
#                'rst', 'o_rst', 'mtchmrgn', 'line', 'won')
# master2 <- master[ , base_cols]


## add juice propagation columns (j and o_j)
master <- addJCols(master, init_j=100, dist_wgts=c(0.05, 0.1, 0.15))


## round digits to 3 decimal places
master <- round_df(master, 3)


## create backup
write.csv(master, './data/master_backup2.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup2.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)


## temporary subsetting (for faster code execution during testing)
# master <- subset(master, season==2012)
# master <- master[ , 1:140]


## set metrics to calculate their cumulative sums
cols_to_cumsum <- c('p', 'pA', 'pos', 'posA', 'FGM', 'FGMA', 'FGA', 'FGAA', 'rqP', 'rqPA')


## add various general cumsum columns
master <- add_cumsum_cols(master, 
                          cols=cols_to_cumsum,
                          vary_by=NULL,
                          add_opp_cols=FALSE)


## add various general cumulative performance columns
master <- add_cumperf_cols(master, add_opp_cols=TRUE)


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
                          add_opp_cols=FALSE)


## make a backup
write.csv(master, './data/master_backup3.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup3.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)


## add variable-specific cumsum cols 
master <- add_cumsum_cols(master, 
                          cols=cols_to_cumsum,
                          vary_by=vary_by,
                          add_opp_cols=FALSE)

## make a backup
write.csv(master, './data/master_backup4.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup4.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)


## add variable-specific performance
master <- add_cumperf_cols(master, add_opp_cols=TRUE)


## add win percentages (both general and variable-specific)
master <- add_wpc_cols(master, add_opp_cols=TRUE)


## list completely empty columns
list_empty_cols(master)


## list cols that contain Inf or NaN vals
list_cols_w_inf_or_nan(master)


## make a backup
write.csv(master, './data/master_backup5.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup5.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)





