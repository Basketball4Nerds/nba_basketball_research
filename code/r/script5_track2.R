## In this code track, we will throw in all predictor variables into the 
## high-VIF removal function (without handling any variables first)
## and remove collinear variables. We will then use cross validation to compare
## different model peroformances and pick the best one.




## remove highly correlated variables via VIF calc
trk2_predictor_vars <- vif_func(in_frame=train_trk1[ , ], 
                                  thresh=5, trace=TRUE)