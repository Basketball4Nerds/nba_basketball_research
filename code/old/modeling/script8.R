#### calculating the earnings for spreads
y <- head(spreads, 50)

## add wonATS column
y <- add_wonATS_col(y, p_mrgn_col='pMrgn', line_col='bookmaker_line')

## add prediction columns to dfs
y$predATS <- sample(c(TRUE,FALSE), 50, TRUE)

## add wager amount column
y$bet_amt <- 100
y$bet_amt[!y$predATS] <- 0

## add earning column
y <- add_earning_col(y, pred_col='predATS', bet_amt_col='bet_amt', payout_col='bookmaker_payout')

## calculate total bet amount and total earning
sum(y$bet_amt)
sum(y$earning)




#### always betting on underdogs
#### always betting on away underdogs
#### always betting on home underdogs
#### always betting on favorites
#### always betting on away favorites
#### always betting on home favorites
