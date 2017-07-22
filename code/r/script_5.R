
## CONTINUE HERE!!!!!

library(corrplot)


## correlation plot
# https://stackoverflow.com/a/19115003
cor_mtx <- round(cor(predictive_df[cumperf_cols], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')

names(predictive_df)
gen_cols <- names(predictive_df)[grepl('_gen', names(predictive_df))]
gen_cols <- setdiff(gen_cols, c('n_gen', 'o_n_gen'))
cor_mtx <- round(cor(predictive_df[gen_cols], use='pairwise.complete.obs'), 3)
corrplot(cor_mtx, method='ellipse', type='lower')




## selecting important variables
## http://amunategui.github.io/variable-importance-shuffler/index.html

## cannot proceed;
## error in package download;
## try again later;

# bonus - great package for fast variable importance
install.packages('mRMRe')
library(mRMRe)
ind <- sapply(titanicDF, is.integer)
titanicDF[ind] <- lapply(titanicDF[ind], as.numeric)
dd <- mRMR.data(data = titanicDF)
feats <- mRMR.classic(data = dd, target_indices = c(ncol(titanicDF)), feature_count = 10)
variableImportance <-data.frame('importance'=feats@mi_matrix[nrow(feats@mi_matrix),])
variableImportance$feature <- rownames(variableImportance)
row.names(variableImportance) <- NULL
variableImportance <- na.omit(variableImportance)
variableImportance <- variableImportance[order(variableImportance$importance, decreasing=TRUE),]
print(variableImportance)