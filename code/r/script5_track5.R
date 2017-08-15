## Other modeling track ideas to explore if interested.

## Idea 1
## Use random forest to pick the best predictor variables.
## https://github.com/feelosophy13/kaggle_show_of_hands

## Idea 2
## average out the best performing cumperf metrics in each category;
## for example, average the best oeff_cumperf metrics (e.g. oeff_cumperf_gen and oeff_cumperf_oeffaQntlRnk)
## also do the same for the best oeffA_cumperf metrics, FGP_cumperf metrics, FGPA_cumperf metrics, etc.;
## then use these average cumperf metrics to include into the models;

## Idea 3
## Throw in all predictor variables in logistic regression model and 
## use ridge, lasso, or elastic net methods to prevent overfitting