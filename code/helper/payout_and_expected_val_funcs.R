## this function calculates moneyline payout profit in case of met bet; 
## does not include the initial amount wagered;
## https://www.gamblingsites.org/sports-betting/beginners-guide/odds/moneyline/
##
## payout may look like this: -110 or 100
calc_payout_amount <- function(wgr_amt=100, payout) {
  
  payout_amt <- ifelse(payout > 0, 
                       wgr_amt * (payout / 100),  # payout profit for bet met on underdog
                       wgr_amt / (abs(payout) / 100))  # payout profit for bet met on favorite
  
  ## round down at 2 decimal places (very miniscule underestimation of payout profit)
  payout_amt <- floor(payout_amt * 100) / 100
  
  ## return
  return(payout_amt)
}


## this function calculates expected return based on
## prob of win, payout for win, and money wagered;
## EV does NOT include the initially wagered bet amount;
##
## Sport Bet Expected Value Formula: 
# Expected Value = 
# (Probability of Positive Outcome) x (Amount Won per Bet) – 
# (Probability of Negative Outcome) x (Amount Lost per Bet)
calc_expected_return <- function(wgr_amt=100, payout, positive_outcome_prob) {
  
  ## calculate probability of negative outcome
  negative_outcome_prob <- 1 - positive_outcome_prob
  
  ## calculate win payout profit in case of correct bet,
  ## not including the initially wagered bet amount
  positive_outcome_payout_amt <- calc_payout_amount(wgr_amt=wgr_amt, payout=payout)
  
  ## calculate expected value
  expected_return <- (positive_outcome_payout_amt * positive_outcome_prob) - (wgr_amt * negative_outcome_prob)
  
  ## return
  return(expected_return)
}

