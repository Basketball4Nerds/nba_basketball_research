master_df <- df

# MODIFY THIS
## predict game outcome based on team's win perc at home/away
pred <- (master_df$site=='H' & master_df$wPcH > 0.5) | (master_df$site=='A' & master_df$wPcA > 0.5)
x <- table(pred, master_df$won)
calcAccFrConfMtx(x)


## predict game outcome based on team's win perc against opposing team conf
pred <- (master_df$o_cnf=='E' & master_df$wPcVsE > 0.5) | (master_df$o_cnf=='W' & master_df$wPcVsW > 0.5)
x <- table(pred, master_df$won)
calcAccFrConfMtx(x)










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



