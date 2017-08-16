
## In this script, we will use historical moneylines data to 
## see how we would have fared if we had bet on games where 
## we expected to see positive EVs.

head(moneylines)
x <- moneylines

## add win probability based on model ensemble (maybe later)


## add win probability based on majority
head(x)
head(trk2_pvotes_df)






#### calculating the earnings for moneylines
# x <- head(moneylines, 50)
x <- moneylines

## add prediction columns to dfs
set.seed(123)
x$pred <- sample(c(TRUE,FALSE), 50, TRUE)

## add wager amount column
x$bet_amt <- 100
x$bet_amt[!x$pred] <- 0

## add earning column
x <- add_earning_col(x, pred_col='pred', bet_amt_col='bet_amt', payout_col='bookmaker_ml')

## calculate total bet amount and total earning
sum(x$bet_amt)
sum(x$earning)



#### calculating the earnings for moneylines
# x <- head(moneylines, 50)
x <- moneylines

## add implied win prob col
#x$implied_wprob <- calc_implied_win_prob_fr_line(x$bookmaker_ml)

## calculate min win prob required to reach breakeven (expected value of 0)
# x$min_wprob_for_brkevn <- calc_win_prob_for_zero_EV(x$bookmaker_ml, rnd_dgt=3)

## add our own win probability
set.seed(123)
x$wprob <- round(runif(n=nrow(moneylines), min=0, max=1), 3)

## calculate expected value (favorable to bet if EV > 0)
x$expval <- calc_exp_val(wgr_amt=100, moneyline_odds=x$bookmaker_ml, w_prob=x$wprob)

## place bet if EV is above certain threshold
x$pred <- ifelse(x$expval >= 100, TRUE, FALSE)

## add wager amount
x$bet_amt[!is.na(x$pred)] <- 100
x$bet_amt[!x$pred] <- 0

## add earning column
x <- add_earning_col(x, pred_col='pred', bet_amt_col='bet_amt', payout_col='bookmaker_ml')

## calculate total bet amount and total earning
sum(x$bet_amt)
sum(x$earning)





