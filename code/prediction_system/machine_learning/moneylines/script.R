## set database credentials
source('../../credentials/aws_db_credentials.R')


## load libraries
library(RPostgreSQL)
library(tidyverse)
library(lubridate)
library(zoo)
library(glue)
library(rpart)
library(rpart.plot)
library(randomForest)


## import custom functions
source('../helper/calc_pred_acc.R')


## load PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## creates a connection to database
con <- dbConnect(drv, host=host, port=port, dbname=db_name, user=db_user, password=db_pass)

## remove password from environment
#rm(db_pass) 

## list available tables
dbListTables(con)

## get pregame performance data
pregame_perf_master_df <- dbReadTable(conn=con, name='pregame_perf_master_df')


## define irrelevant columns to remove for modeling
rm_cols <- c('team_id', 'game_id', 'game_date', 'team_abbr', 'opponent_abbr', 'ptsmrgn')
rm_cols_regex <- '^(o_)?[nwl]_(gen|site)$'

## define metrics to take difference against opponent's metric
metric_cols <- names(x) %>% 
  grep(pattern='cummean|rollmean|^wp_', x=., value=TRUE) %>%
  gsub(pattern='^o_', replacement='', x=.)


## define function that can perform the following: (metric) - (opponent metric)
take_diff <- function(df, cols) {
  for (col in cols) {
    o_col <- glue("o_{col}")  
    diff_col <- glue("{col}_diff")
    df[[diff_col]] <- df[[col]] - df[[o_col]]
  }
  return(df)    
}


##
x <- pregame_perf_master_df %>%
  
  ## remove rollmean10 columns
  select(-contains('rollmean10')) %>%
  
  ## change game location to home 
  rename(home = game_location) %>%
  mutate(home = as.integer(home=='H')) %>% 

  ## take difference against opponent's metric
  take_diff(cols=metric_cols) %>%

  ## select relevant columns for modeling
  #select(game_outcome, home, season, game_date, team_abbr, opponent_abbr, matches('_diff$')) %>%
  select(game_outcome, home, season, matches('_diff$')) %>%
  
  ## filter by complete cases
  filter(complete.cases(.)) 


#### split training and test dataset
train <- x %>% filter(season <= '2014-15') %>% select(-season)
test <- x %>% filter(season >= '2015-16') %>% select(-season)



#### data normalization: scaling 

## obtain min and max values for each column (for 0 to 1 scaling)
maxs <- apply(train[ , -1], 2, max)
mins <- apply(train[ , -1], 2, min)

## train dataset's scaled feature matrix
train_features_scaled_mtx <- train %>%
  select(-game_outcome) %>%
  scale(center = mins, scale = maxs - mins)

## test dataset's scaled feature matrix
test_features_scaled_mtx <- test %>%
  select(-game_outcome) %>%
  scale(center=attr(train_features_scaled_mtx, 'scaled:center'),
        scale=attr(train_features_scaled_mtx, 'scaled:scale'))

## scaled train and test datasets with prediction variable joined back
train_scaled <- train %>%
  select(game_outcome) %>%
  cbind.data.frame(train_features_scaled_mtx) %>%
  as.data.frame()

test_scaled <- test %>%
  select(game_outcome) %>%
  cbind.data.frame(test_features_scaled_mtx) %>%
  as.data.frame()


#### analysis of variable importance

## create variable importance df
varimp_df <- train_scaled %>%
  group_by(game_outcome) %>%
  summarise_all(
    funs(mean)
  ) %>%
  select(-game_outcome) %>%
  t() %>% 
  as.data.frame() %>%
  set_names(c('L_mean', 'W_mean')) %>%
  mutate(
    metric = rownames(.),
    abs_diff = abs(W_mean - L_mean)
  ) %>% 
  select(metric, abs_diff) %>%
  arrange(-abs_diff)

## review variables
head(varimp_df, 30)
tail(varimp_df, 30)

## top percentile
quantile(varimp_df$abs_diff)
quantile(varimp_df$abs_diff)[4]


## select top variables
top_vars <- varimp_df$metric[varimp_df$abs_diff > quantile(varimp_df$abs_diff)[4]]
length(top_vars)


#### principal component analysis

## create prcomp object with scaled training features
train_pca <- train_scaled %>%
  select(top_vars) %>%
  prcomp()

## plot share of variance explained by top 10 components
plot(train_pca, type = "l")  

## view pca summary
summary(train_pca)

## create principal components for test dataset
test_pca <- predict(train_pca, newdata=test_features_scaled_mtx)

## create train dataset with outcome variable and PCs
train_pc <- train %>%
  select(game_outcome) %>%
  cbind.data.frame(train_pca$x[ , 1:10]) %>%
  as.data.frame() %>%
  mutate(game_outcome = as.factor(game_outcome))

## create test dataset with outcome variable and PCs
test_pc <- test %>%
  select(game_outcome) %>%
  cbind.data.frame(test_pca[ , 1:10]) %>%
  as.data.frame() %>%
  mutate(game_outcome = as.factor(game_outcome))




#### modeling

## very basic logistic regression model
logit_model <- glm(game_outcome ~ ., family="binomial", data=train_pc)
summary(logit_model)
logit_pred_prob <- predict(logit_model, newdata=test_pc, type='response')
logit_pred <- ifelse(logit_pred_prob > 0.5, 1, 
                     ifelse(logit_pred_prob < 0.5, 0, NA))
logit_cnf_mtx <- table(logit_pred, test_pc$game_outcome)
calc_pred_acc(logit_cnf_mtx)


## very basic decision tree model
tree_model <- rpart(game_outcome ~ ., data=train_pc)
summary(tree_model)
prp(tree_model)
tree_pred_prob <- predict(tree_model, newdata=test_pc, type='prob')[, 2]
tree_pred <- ifelse(tree_pred_prob > 0.5, 1, 
                    ifelse(tree_pred_prob < 0.5, 0, NA))
tree_cnf_mtx <- table(tree_pred, test_pc$game_outcome)
calc_pred_acc(tree_cnf_mtx)


## random forest model
rf_model <- randomForest(game_outcome ~ ., data=train_pc)
summary(rf_model)
rf_pred_prob <- predict(rf_model, newdata=test_pc, type='prob')[, 2]
rf_pred <- ifelse(rf_pred_prob > 0.5, 1, 
                  ifelse(rf_pred_prob < 0.5, 0, NA))
rf_cnf_mtx <- table(rf_pred, test_pc$game_outcome)
calc_pred_acc(rf_cnf_mtx)



