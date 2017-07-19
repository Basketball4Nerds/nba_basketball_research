## subset df
master_df <- subset(master, season==2012)


## create prediction df
predictive_df <- create_predictive_df(master_df)
predictive_df <- sortByCol(predictive_df, c('season', 'date'))



## 
won_df <- subset(predictive_df, won)
lost_df <- subset(predictive_df, !won)
cumperf_cols <- names(won_df)[grepl('_cumperf_', names(won_df))]
varimp_df <- create_varimp_df(won_df, lost_df, predictor_vars=cumperf_cols)
varimp_df <- sortByCol(varimp_df, 'spread')


results <- varimp_df
names(results) <- c('variableName', 'Weight')
# display variable importance on a +/- scale 
results <- results[order(results$Weight),]
results <- results[(results$Weight != 0),]

par(mar=c(2.5,7.5,2,1)) # increase y-axis margin. 
xx <- barplot(results$Weight, width = 0.85, 
              main = paste("Variable Importance - Titanic"), horiz = T, 
              xlab = "< (-) importance >  < neutral >  < importance (+) >", axes = FALSE, 
              col = ifelse((results$Weight > 0), 'blue', 'red')) 
axis(2, at=xx, labels=results$VariableName, tick=FALSE, las=2, line=-0.3, cex.axis=0.6)  

varimp_df

