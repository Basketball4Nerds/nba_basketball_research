## load libraries
library(shiny)
library(ggplot2)
library(corrplot)
library(TTR)
library(plyr)
library(BBmisc)
library(rpart.plot)
library(data.table)
library(DBI)
library(RMySQL)
library(RCurl)
library(rjson)
library(dplyr)
library(rvest)
library(car)  # for leveneTest()
library(lsr)  # for cohensD()
library(gridExtra)  # for expand.grid()
library(reshape2)  # for dcast()
library(mRMRe)  # for redundant feature reduction
library(fmsb)  # for VIF calculation
library(rpart)  # for decision tree
library(caret)  
library(klaR)  # for naive bayes
library(e1071)  # for svm, naive bayes
library(kernlab)  # for svm
library(class)  # for knn
library(randomForest)
library(gbm)  # gbm modeling
library(nnet)  # for neural network
#library(neuralnet)  # for neural network
library(glmnet)  # for lasso and ridge regression



## load functions and classes
## * future plan: these functions will be wrapped in a package to avoid overcrowding the global env
source('./code/_analysis_scripts/analysis_funcs.R')
source('./code/_analysis_scripts/functions.R')
source('./code/_analysis_scripts/pred_funcs.R')
source('./code/_analysis_scripts/pred_funcs2.R')
source('./code/_analysis_scripts/supervised_summarizer_funcs.R')
source('./code/_analysis_scripts/feature_selection_funcs.R')

source('./code/_preprocess_scripts/mov_cnt_sum_avg_funcs.R')
source('./code/_preprocess_scripts/preprocess_funcs.R')
source('./code/_preprocess_scripts/rnk_grp_funcs.R')

source('./code/_other_scripts/classes.R')
source('./code/_other_scripts/helper_funcs.R')




# ## load DB login keys
# source('../credentials/keys.R')
# 
# 
# ## connect to db again
# mydb <- dbConnect(MySQL(), 
#                   user=DB_USER, 
#                   password=DB_PASS, 
#                   dbname=DB_NAME,
#                   host=DB_HOST)
# 
# 
# ## load complete data from db
# games <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM games;'), n=-1))
# spreads <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM spreads;'), n=-1))
# totals <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM totals;'), n=-1))
# moneylines <- suppressWarnings(fetch(dbSendQuery(mydb, 'SELECT * FROM moneylines;'), n=-1))
#
#
# ## close database connections
# close_all_db_cons()
