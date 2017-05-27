
#### simple methods to predict winners (regular season only)


makeSimplePreds <- function(master) {
  ## predict that home team will win
  pred <- rs$site=='H'
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
  
  ## predict that favored team will win
  ## - use the tier system to see how it perform
  hist(rs$line)
  pred <- ifelse(rs$line < 0, TRUE,
                 ifelse(rs$line > 0, FALSE, NA))
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
  
  ## predict that team with higher win percentage will win
  pred <- ifelse(rs$wPc > rs$o_wPc, TRUE,
                 ifelse(rs$wPc < rs$o_wPc, FALSE, NA))
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
  
  
  ## predict that team with more matchup wins will win
  ## use tier system to see how it performs
  rs$mtch_mrgn <- rs$mtch_w - rs$mtch_l
  pred <- ifelse(rs$mtch_mrgn > 0, TRUE,
                 ifelse(rs$mtch_mrgn < 0, FALSE, NA))
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
  pred <- ifelse(rs$mtch_mrgn > 1, TRUE,
                 ifelse(rs$mtch_mrgn < -1, FALSE, NA))
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
  pred <- ifelse(rs$mtch_mrgn > 2, TRUE,
                 ifelse(rs$mtch_mrgn < -2, FALSE, NA))
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  x
  
  
  ## predict that team with higher propagation juice will win
  pred <- rs$j > rs$o_j
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
  ## predict that whichever team that had more rest will win the game
  pred <- ifelse(rs$rst > rs$o_rst, TRUE, 
                 ifelse(rs$rst == rs$o_rst, NA, FALSE))
  x <- table(pred, rs$won)
  calcAccFrConfMtx(x)
  
}



## MODIFY THIS
# ## predict game outcome based on team's win perc at home/away
# pred <- (rs$site=='H' & rs$wPcH > 0.5) | (rs$site=='A' & rs$wPcA > 0.5)
# x <- table(pred, rs$won)
# calcAccFrConfMtx(x)
# 
# 
# ## predict game outcome based on team's win perc at home/away
# pred <- (rs$o_cnf=='E' & rs$wPcVsE > 0.5) | (rs$o_cnf=='W' & rs$wPcVsW > 0.5)
# x <- table(pred, rs$won)
# calcAccFrConfMtx(x)
# 
# 
# ## predict game outcome based on team's win perc against opposing team conf
# pred <- (rs$o_cnf=='E' & rs$wPcVsE > 0.5) | (rs$o_cnf=='W' & rs$wPcVsW > 0.5)
# x <- table(pred, rs$won)
# calcAccFrConfMtx(x)










## predict that team with higher points SMA will win

## predict that team with lower points-allowed SMA will win

## predict that team with higher points margin SMA will win

## predict that team with higher PPP SMA will win

## predict that team with higher FG percentage SMA will win

## predict that team with higher 2-pointer FG percentage SMA will win

## predict that team with higher 3-pointer FG percentage SMA will win

## predict that team with higher FGM SMA will win

## predict that team with higher dRb SMA will win

## predict that team with higher ast SMA will win

## predict that team with higher rb SMA will win

## predict that team with higher blk SMA will win

## predict that team with higher FTA SMA will win

## predict that team with lower mean age will win





# site-sp
# cnf-sp
# 



