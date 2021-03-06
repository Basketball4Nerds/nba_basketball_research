
## this function takes in moneyline odds (which reflects win payout info) 
## and returns win probability required to achieve an expected value of 0;
## this is equivalent to calculating implied win probability of a team based
## on its moneyline odds; see calc_implied_win_prob_fr_line();
calc_win_prob_for_zero_EV <- function(moneyline_odds, rnd_dgt=5) {
  
  #### formula derivation
  # EV = win_prob * payout_profit - loss_prob * wgr_amt
  # 0 = win_prob * payout_profit - loss_prob * wgr_amt
  # 0 = win_prob * payout_profit - (1 - win_prob) * wgr_amt
  # 0 = win_prob * payout_profit + win_prob * wgr_amt - wgr_amt
  # 0 = win_prob * (payout_profit + wgr_amt) - wgr_amt
  # wgr_amt = win_prob * (payout_profit + wgr_amt)
  # win_prob = wgr_amt / (payout_profit + wgr_amt)
  
  ## for underdogs
  # payout_profit = wgr_amt * (moneyline_odds / 100)
  # win_prob = wgr_amt / (payout_profit + wgr_amt)
  # win_prob = wgr_amt / [ wgr_amt * (moneyline_odds / 100) + wgr_amt ]
  # win_prob = 1 / [ (moneyline_odds / 100) + 1 ]
  # win_prob = 100 / (moneyline_odds + 100)
  
  ## for favorites
  # payout_profit <- wgr_amt / (abs(moneyline_odds) / 100)
  # win_prob = wgr_amt / (payout_profit + wgr_amt)
  # win_prob = wgr_amt / [ wgr_amt / (abs(moneyline_odds) / 100) + wgr_amt ]
  # win_prob = 1 / [ 1 / (abs(moneyline_odds) / 100) + 1 ]
  # win_prob = 1 / [ 1 / (abs(moneyline_odds) / 100) + 1 ]
  # win_prob = abs(moneyline_odds) / [ 100 + abs(moneyline_odds) ]
  
  ## calculate win prob necessary to achieve expected value of 0
  win_prob <- ifelse(moneyline_odds > 0,
                     100 / (moneyline_odds + 100),  # calculate win prob with underdog moneyline odds
                     abs(moneyline_odds) / (100 + abs(moneyline_odds)))  # calculate win prob with favorite moneyline odds
  
  ## round probability
  win_prob <- round(win_prob, rnd_dgt)
  
  ## return 
  return(win_prob)
}


## this function add earning column to df
add_earning_col <- function(df, pred_col, bet_amt_col, payout_col) {
  
  ## initialize earning col
  df$earning <- NA
  
  ## net winning/loss when when no bet was placed (0) 
  no_bet_index <- is.false(df[[pred_col]])
  no_bet_index[is.na(no_bet_index)] <- FALSE
  df$earning[no_bet_index] <- 0
  
  ## bet winning when win correctly predicted
  correct_bet_index <- df$won & df[[pred_col]]
  correct_bet_index[is.na(correct_bet_index)] <- FALSE
  df$earning[correct_bet_index] <- calc_moneyline_payout_profit(wgr_amt=df[[bet_amt_col]], 
                                                                moneyline_odds=df[[payout_col]])[correct_bet_index]
  
  ## bet loss when win incorrectly predicted
  incorrect_bet_index <- !df$won & df[[pred_col]]
  incorrect_bet_index[is.na(incorrect_bet_index)] <- FALSE
  df$earning[incorrect_bet_index] <- -df[[bet_amt_col]][incorrect_bet_index]
  
  ## return
  return(df)
}


## this function takes in df and adds ATS (against the spread) win column
add_wonATS_col <- function(df, p_mrgn_col, line_col) {
  
  ## favorite examples
  # favorite (-3) wins by 7: spread covered; won ATS
  # favorete (-3) wins by 3: "push" (tie)
  # favorite (-3) wins by 1: spread not covered; lost ATS
  # favorite (-3) loses by 3: spread not covered; lost ATS
  
  ## underdog examples
  # underdog (+5) wins by 7: won ATS
  # underdog (+5) loses by -3: won ATS
  # underdog (+5) loses by -5: "push" (tie)
  # underdog (+5) loses by -7: lost ATS
  
  df$wonATS <- NA
  df$wonATS[df[[line_col]] + df[[p_mrgn_col]] > 0] <- TRUE  # covered
  df$wonATS[df[[line_col]] + df[[p_mrgn_col]] < 0] <- FALSE  # not covered
  df$wonATS[df[[line_col]] + df[[p_mrgn_col]] == 0] <- NA  # neither covered nor not covered (tie, or "push")
  return(df)
}


## this function calculates implied win probability of a team
## from its given moneyline odds; 
## https://sportsprofit.wordpress.com/2010/06/29/how-to-convert-a-money-line-into-a-percentage-and-vice-versa/
##
## this is equivalent to calculating the minimum win probability required to
## achieve zero expected value (and reach breakeven point) given its moneyline odds; 
## see calc_win_prob_for_zero_EV();
calc_implied_win_prob_fr_line <- function(moneyline_odds, rnd_dgt=3) {
  implied_win_prob <- ifelse(moneyline_odds < 0, 
                             moneyline_odds / (moneyline_odds - 100),  # implied win prob for favorite
                             ifelse(moneyline_odds > 0, 
                                    100 / (moneyline_odds + 100),   # implied win prob for underdog
                                    0.5))  # when moneyline is 0, implied win probability is 0.5
  implied_win_prob <- round(implied_win_prob, rnd_dgt)
  return(implied_win_prob)
}
