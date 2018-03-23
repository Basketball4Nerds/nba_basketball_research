#### load regular season games
games <- read.csv("../../../data/processed/regular_season_games_master.csv", stringsAsFactors=FALSE)
games$outcome <- ifelse(games$won, 'won', 'lost')
games$site <- ifelse(games$site=='H', 'home', 'away')



#### load libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(plyr)
library(dplyr)
library(BBmisc)
library(reshape2)
library(tidyr)  # tidyr is successor to reshape2
library(data.table)  # for setDT()
library(knitr)
library(kableExtra)



#### get necessary functions
source("../../helper/calc_acc_fr_cnf_mtx.R")

## this function creates df of simple retrospective win prediction strengths (RWPS)
## by using raw performance metrics
create_smpl_retro_win_pred_acc_df <- function(master_df, raw_perf_cols=NULL) {
  
  ## if cols not set, assign values to cols
  if (is.null(raw_perf_cols)) {
    raw_perf_cols <- c('p', 'p2x', 'p3x', 'pPnt', 'pFb', 
                       'stl', 'ast', 'blk', 'dRb', 'oRb', 'rb', 'trnovrFcd', 'flFcd', 'pos',
                       'FGA', 'FGA2x', 'FGA3x', 'FTA',
                       'FGM', 'FGM2x', 'FGM3x', 'FTM', 
                       'p2xShr', 'p3xShr', 'pFtShr', 'pPntShr', 'pFbShr',
                       'FGP', 'FGP2x', 'FGP3x', 'FTP',
                       'oRbShr', 'dRbShr', 'oRbPc', 'dRbPc', 'ODRR', 'ODRSR',
                       'oeff', 'toPcFcd', 'FTA_p_FGA')
  }
  
  ## initialize empty vectors to store values
  col_vec <- acc_vec <- n_pred_vec <- c()
  
  ## calculate retrospective win prediction strength (RWPS) for each raw performance metric
  for (col in raw_perf_cols) {
    
    ## construct o_col (opponent's metric) based on a given col
    if (grepl('Fcd', col)) {
      o_col <- gsub('Fcd', '', col)
    }
    else if (grepl('[A-Za-z]_p_[A-Za-z]', col)) {
      o_col <- unlist(strsplit(col, split='_p_'))
      o_col <- paste0(o_col, 'A')
      o_col <- paste0(o_col, collapse='_p_')
    }
    else {
      o_col <- paste0(col, 'A')
    }
    
    ## if o_col is not found in dataset, skip to the next metric
    if (!(o_col %in% names(master_df))) {
      print(paste('Unable to locate the following metric:', col))
      next
    }
    
    ## make a simple retrospective prediction
    pred <- ifelse(master_df[ , col] > master_df[ , o_col], TRUE, 
                   ifelse(master_df[ , col] < master_df[ , o_col], FALSE, NA))
    
    ## create confusion matrix
    cnf_mtx <- table(master_df$won, pred)
    
    ## calculate prediction accuracy
    acc <- calc_acc_fr_cnf_mtx(cnf_mtx)
    
    ## calculate the number of data points to calculate retro pred acc
    n_pred <- sum(cnf_mtx)
    
    ## add vals to vectors
    col_vec <- c(col_vec, col)
    acc_vec <- c(acc_vec, acc)
    n_pred_vec <- c(n_pred_vec, n_pred)
  }
  
  ## create return df
  ret_df <- cbind.data.frame(metric = col_vec, 
                             SRWPS = acc_vec, 
                             n_pred = n_pred_vec)
  
  ## sort return df
  ret_df <- sortByCol(ret_df, col='SRWPS', asc=FALSE)
  
  ## return
  return(ret_df)
}




## 
games$ast_p_FGM <- games$ast / games$FGM
games$astA_p_FGMA <- games$astA / games$FGMA

raw_perf_cols <- c('p', 'p2x', 'p3x', 'pPnt', 'pFb', 
                   'stl', 'ast', 'blk', 'dRb', 'oRb', 'rb', 'trnovrFcd', 'flFcd', 'pos',
                   'FGA', 'FGA2x', 'FGA3x', 'FTA',
                   'FGM', 'FGM2x', 'FGM3x', 'FTM', 
                   'p2xShr', 'p3xShr', 'pFtShr', 'pPntShr', 'pFbShr',
                   'FGP', 'FGP2x', 'FGP3x', 'FTP',
                   'oRbShr', 'dRbShr', 'oRbPc', 'dRbPc', 'ODRR', 'ODRSR',
                   'oeff', 'toPcFcd', 'FTA_p_FGA', 'ast_p_FGM')



## create and view a simple retro win prediction accuracy df
x <- create_smpl_retro_win_pred_acc_df(games, raw_perf_cols)
row.names(x) <- NULL
kable(x, 'html')
x
