
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