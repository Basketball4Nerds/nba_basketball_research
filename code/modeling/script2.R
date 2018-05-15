## load libraries
library(rpart)
library(rpart.plot)
library(randomForest)


## list available tables
dbListTables(con)


## remove irrelevant column for modeling
rm_cols <- c('team_id', 'game_id', 'game_date', 'team_abbr', 'opponent_abbr', 'ptsmrgn')
rm_cols_regex <- '^(o_)?[nwl](_gen|_site)$'

x <- pregame_perf_master_df %>%
  
  ## filter by complete cases
  #filter(n_site >= 5 & o_n_site >= 5) %>%
  filter(complete.cases(.)) %>%
  
  ## remove non-pre-game-performance metrics
  select(-one_of(rm_cols)) %>%
  
  ## remove pre-game win, loss count columns
  select(-matches(rm_cols_regex)) %>%
  
  ## change game location to home 
  rename(home = game_location) %>%
  mutate(home = as.integer(home=='H'))



#### split training and test dataset
train <- x %>% filter(season <= '2014-15') %>% select(-season)
test <- x %>% filter(season >= '2015-16') %>% select(-season)



#### data normalization: scaling 

## dfs of pre-game features
train_features <- train[ , -1]
test_features <- test[ , -1]

## dfs of scaled pre-game features
train_features_scaled <- scale(train_features)
test_features_scaled <- scale(test_features,
                              center=attr(train_features_scaled, 'scaled:center'),
                              scale=attr(train_features_scaled, 'scaled:scale'))


## principal component analysis
train_pca <- prcomp(train_features, center=TRUE, scale=TRUE)

## share of variance explained by component
plot(train_pca, type = "l")  

## increase print limit option to print more
getOption("max.print")
options(max.print=3000)

## view pca summary
summary(train_pca)

## 
test_pca <- predict(train_pca, newdata=test_features_scaled)

train_pc <- as.data.frame(cbind.data.frame(game_outcome=train$game_outcome, train_pca$x[ , 1:83]))
test_pc <- as.data.frame(cbind.data.frame(game_outcome=test$game_outcome, test_pca[ , 1:83]))


## very basic logistic regression model
logit_model <- glm(game_outcome ~ ., family="binomial", data=train_pc)
summary(logit_model)
logit_pred_prob <- predict(logit_model, newdata=test_pc, type='response')
logit_pred <- ifelse(logit_pred_prob > 0.85, 1, 
                     ifelse(logit_pred_prob < 0.15, 0, NA))
logit_cnf_mtx <- table(logit_pred, test_pc$game_outcome)
calc_pred_acc(logit_cnf_mtx)
calc_pred_acc <- function(cnf_mtx) {
  return(sum(diag(cnf_mtx))/sum(cnf_mtx))
}

## very basic decision tree model
tree_model <- rpart(game_outcome ~ ., data=train_pc)
summary(tree_model)
prp(tree_model)
tree_pred_prob <- predict(tree_model, newdata=test_pc, type='prob')[, 2]
tree_pred <- ifelse(tree_pred_prob > 0.5, 1, 0)
tree_cnf_mtx <- table(tree_pred, test_pc$game_outcome)
tree_cnf_mtx
1757 / (1757 + 953)

## random forest model
rf_model <- randomForest(game_outcome ~ ., data=train_pc)
summary(rf_model)
rf_pred_prob <- predict(rf_model, newdata=test_pc, type='prob')[, 2]
rf_pred <- ifelse(rf_pred_prob > 0.5, 1, 0)
rf_cnf_mtx <- table(rf_pred, test_pc$game_outcome)
rf_cnf_mtx
table(rf_pred)

head(rf_pred_prob)




