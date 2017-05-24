#### examine metrics' retrospective win prediction strength
cols <- c('p', 'p2x', 'p3x', 'pPnt', 'pFb', 
          'stl', 'ast', 'blk', 'dRb', 'oRb', 'rb', 'trnovrFcd', 'flFcd', 'pos',
          'FGA', 'FGA2x', 'FGA3x', 'FTA',
          'FGM', 'FGM2x', 'FGM3x', 'FTM', 
          'p2xShr', 'p3xShr', 'pFtShr', 'pPntShr', 'pFbShr',
          'FGP', 'FGP2x', 'FGP3x', 'FTP',
          'oRbShr', 'dRbShr', 'oRbPc', 'dRbPc', 'ODRR', 'ODRSR',
          'PPP', 'toPcFcd', 'FTA_p_FGA')
SRWPA_df <- createSimpleRetroWinPredAccDf(master, cols)
SRWPA_df$SRWPS <- round(SRWPA_df$SRWPS, 3)
SRWPA_df <- SRWPA_df[order(-SRWPA_df$SRWPS), ]
SRWPA_df



#### simple methods to predict winners (regular season only)
rs <- subset(master, playoffs==0)
rs10 <- subset(rs, n >= 10 & o_n >= 10)


## predict that home team will win
pred <- rs$site=='home'
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

pred <- ifelse(rs10$wPc > rs10$o_wPc, TRUE,
               ifelse(rs10$wPc < rs10$o_wPc, FALSE, NA))
x <- table(pred, rs10$won)
calcAccFrConfMtx(x)


## predict game outcome based on team's win perc at home/away
pred <- (rs$site=='home' & rs$wPcH > 0.5) | (rs$site=='away' & rs$wPcA > 0.5)
x <- table(pred, rs$won)
calcAccFrConfMtx(x)


## predict game outcome based on team's win perc at home/away
pred <- (rs$o_cnf=='E' & rs$wPcVsE > 0.5) | (rs$o_cnf=='W' & rs$wPcVsW > 0.5)
x <- table(pred, rs$won)
calcAccFrConfMtx(x)


## predict that team with higher site-sp win percentage will win
pred <- ifelse(rs$site=='home' & rs$wPcH > rs$o_wPcA, TRUE,
               ifelse(rs$site=='away' & rs$wPcA > rs$o_wPcH, TRUE, FALSE))
x <- table(pred, rs$won)
calcAccFrConfMtx(x)


## predict game outcome based on team's win perc against opposing team conf
pred <- (rs$o_cnf=='E' & rs$wPcVsE > 0.5) | (rs$o_cnf=='W' & rs$wPcVsW > 0.5)
x <- table(pred, rs$won)
calcAccFrConfMtx(x)


## predict that team with higher conf-sp win percentage will win
# scenarios:
# E vs. E
# E vs. W
# W vs. E
# W vs. W
pred <- ifelse(rs$cnf=='E' & rs$o_cnf=='E' & (rs$wPcVsE > rs$o_wPcVsE), TRUE,
               ifelse(rs$cnf=='E' & rs$o_cnf=='W' & (rs$wPcVsW > rs$o_wPcVsE), TRUE, 
                      ifelse(rs$cnf=='W' & rs$o_cnf=='E' & (rs$wPcVsE > rs$o_wPcVsW), TRUE, 
                             ifelse(rs$cnf=='W' & rs$o_cnf=='W' & (rs$wPcVsW > rs$o_wPcVsW), TRUE, FALSE))))
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

pred <- rs10$j > rs10$o_j
x <- table(pred, rs10$won)
calcAccFrConfMtx(x)


## predict that whichever team that had more rest will win the game
pred <- ifelse(rs$rst > rs$o_rst, TRUE, 
               ifelse(rs$rst == rs$o_rst, NA, FALSE))
x <- table(pred, rs$won)
calcAccFrConfMtx(x)


## predict that team with higher off-rank-grp-sp win percentage will win
## predict that team with higher def-rank-grp-sp win percentage will win


## see how 
ddply(rs, 'season', function(x) {
  table(x$p + x$line > x$o_p)
})
master$won_after_adj <- ifelse(master$p + master$adjustor > master$o_p, TRUE,
                               ifelse(master$p + master$adjustor < master$o_p, FALSE, NA))




# 100 + (+4.5) > 103
# 120 + (+4.5) > 103
# 100 + (-4.5) > 103
# 105 + (-4.5) > 103
# 120 + (-4.5) > 103


cols <- c('p', 'p2x', 'p3x', 'pPnt', 'pFb', 
          'stl', 'ast', 'blk', 'dRb', 'oRb', 'rb', 'trnovrFcd', 'flFcd', 'pos',
          'FGA', 'FGA2x', 'FGA3x', 'FTA',
          'FGM', 'FGM2x', 'FGM3x', 'FTM', 
          'p2xShr', 'p3xShr', 'pFtShr', 'pPntShr', 'pFbShr',
          'FGP', 'FGP2x', 'FGP3x', 'FTP',
          'oRbShr', 'dRbShr', 'oRbPc', 'dRbPc', 'ODRR', 'ODRSR',
          'PPP', 'toPcFcd', 'FTA_p_FGA')


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



