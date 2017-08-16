#### analysis of game pace distribution
range(master$pos)
quantile(master$pos)
tapply(master$pos, master$season, quantile)

mean(master$pos)
sd(master$pos)
tapply(master$pos, master$season, mean)




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





#### exploratory analysis

## relationship between points and game outcome
ggplot(games) + 
  geom_density(aes(x=pts, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$pts, games$won, mean)
leveneTest(games$pts, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$pts[games$won], games$pts[!games$won], paired=FALSE, var.equal=TRUE)
cohensD(games$pts[games$won], games$pts[!games$won])


## relationship between FG percentage and game outcome
ggplot(master) + 
  geom_density(aes(x=FGP, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$fg_perc, games$won, mean)
leveneTest(games$fg_perc, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$fg_perc[games$won], games$fg_perc[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$fg_perc[games$won], games$fg_perc[!games$won])


## relationship between assists and game outcome
ggplot(master) + 
  geom_density(aes(x=ast, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$assists, games$won, mean)
leveneTest(games$assists, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$assists[games$won], games$assists[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$assists[games$won], games$assists[!games$won])


## relationship between steals and game outcome
ggplot(master) + 
  geom_density(aes(x=stl, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(master$stl, master$won, mean)
leveneTest(games$steals, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$steals[games$won], games$steals[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$steals[games$won], games$steals[!games$won])


## relationship between blocks and game outcome
ggplot(games) + 
  geom_density(aes(x=blocks, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$blocks, games$won, mean)
leveneTest(games$blocks, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$blocks[games$won], games$blocks[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$blocks[games$won], games$blocks[!games$won])


## relationship between turnovers and game outcome
ggplot(games) + 
  geom_density(aes(x=turnovers, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$turnovers, games$won, mean)
leveneTest(games$turnovers, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$turnovers[games$won], games$turnovers[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$turnovers[games$won], games$turnovers[!games$won])


## relationship between fouls and game outcome:
## - What is the distribution of number of fouls? 
## - Do teams who commit a lot of fouls more likely to win or lose? 
ggplot(games) + 
  geom_density(aes(x=fouls, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$fouls, games$won, mean)
leveneTest(games$fouls, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$fouls[games$won], games$fouls[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$fouls[games$won], games$fouls[!games$won])


## relationship between percentage of points from 3-pointers and game outcome
## - Do teams with high percentage of points from behind the 3-point line more or less likely to win?
ggplot(games) + 
  geom_density(aes(x=perc_pts_fr_3ptr, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$perc_pts_fr_3ptr, games$won, mean)
leveneTest(games$perc_pts_fr_3ptr, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$perc_pts_fr_3ptr[games$won], games$perc_pts_fr_3ptr[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$perc_pts_fr_3ptr[games$won], games$perc_pts_fr_3ptr[!games$won])


## relationship between percentage of points from 2-pointers and game outcome
ggplot(games) + 
  geom_density(aes(x=perc_pts_fr_2ptr, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$perc_pts_fr_2ptr, games$won, mean)
leveneTest(games$perc_pts_fr_2ptr, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$perc_pts_fr_2ptr[games$won], games$perc_pts_fr_2ptr[!games$won], paired=FALSE, var.equal=TRUE)
cohensD(games$perc_pts_fr_2ptr[games$won], games$perc_pts_fr_2ptr[!games$won])


## relationship between percentage of points from free throws and game outcome
ggplot(games) + 
  geom_density(aes(x=perc_pts_fr_ft, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$perc_pts_fr_ft, games$won, mean)
leveneTest(games$perc_pts_fr_ft, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$perc_pts_fr_ft[games$won], games$perc_pts_fr_ft[!games$won], paired=FALSE, var.equal=TRUE)
cohensD(games$perc_pts_fr_ft[games$won], games$perc_pts_fr_ft[!games$won])


## relationship between percentage of fast break points and game outcome
ggplot(games) + 
  geom_density(aes(x=perc_pts_fr_fb, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$perc_pts_fr_fb, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$perc_pts_fr_fb, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$perc_pts_fr_fb[games$won], games$perc_pts_fr_fb[!games$won], paired=FALSE, var.equal=TRUE)
cohensD(games$perc_pts_fr_fb[games$won], games$perc_pts_fr_fb[!games$won])


## relationship between percentage of points in paint and game outcome
## - Do teams with high percentage of points in the paint more or less likely to win?
ggplot(games) + 
  geom_density(aes(x=perc_pts_in_paint, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$perc_pts_in_paint, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$perc_pts_in_paint, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$perc_pts_in_paint[games$won], games$perc_pts_in_paint[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$perc_pts_in_paint[games$won], games$perc_pts_in_paint[!games$won])


## relationship between biggest lead and game outcome
ggplot(games) + 
  geom_density(aes(x=biggest_lead, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$biggest_lead, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$biggest_lead, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$biggest_lead[games$won], games$biggest_lead[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$biggest_lead[games$won], games$biggest_lead[!games$won])


## relationship between biggest lead and points margin
ggplot(games) + 
  geom_point(aes(x=biggest_lead, y=pts_margin)) 


## relationship between percentage of rebounds while on defense and game outcome
ggplot(games) + 
  geom_density(aes(x=perc_rbnds_on_def, fill=won), alpha=0.5) +
  facet_grid(season ~ .)
tapply(games$perc_rbnds_on_def, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$perc_rbnds_on_def, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$perc_rbnds_on_def[games$won], games$perc_rbnds_on_def[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$perc_rbnds_on_def[games$won], games$perc_rbnds_on_def[!games$won])


## relationship between percentage of rebounds while on offense and game outcome
ggplot(games) + 
  geom_density(aes(x=perc_rbnds_on_off, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$perc_rbnds_on_off, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$perc_rbnds_on_off, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$perc_rbnds_on_off[games$won], games$perc_rbnds_on_off[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$perc_rbnds_on_off[games$won], games$perc_rbnds_on_off[!games$won])


## relationship between matchup margin and point spreads
ggplot(games, aes(x=matchup_margin, y=pts_margin)) + 
  geom_point() + 
  geom_smooth(method='lm') + 
  facet_grid(season ~ .)
tapply(games$matchup_margin, games$won, function(x) { mean(x, na.rm=TRUE) })
t.test(games$matchup_margin[games$won], games$matchup_margin[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$matchup_margin[games$won], games$matchup_margin[!games$won])


## relationship between rest and game outcome
ggplot(games) + 
  geom_density(aes(x=rest, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$rest, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$rest, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$rest[games$won], games$rest[!games$won], paired=FALSE, var.equal=TRUE)
cohensD(games$rest[games$won], games$rest[!games$won])


## relationship between rest margin and game outcome
ggplot(games) + 
  geom_density(aes(x=rest_margin, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$rest_margin, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$rest_margin, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$rest_margin[games$won], games$rest_margin[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$rest_margin[games$won], games$rest_margin[!games$won])


## relationship between rest margin and point spread
ggplot(games, aes(x=rest_margin, y=pts_margin)) + 
  geom_point() + 
  geom_smooth(method='lm') 


## relationship between win percentage and game outcome
ggplot(games) + 
  geom_density(aes(x=w_perc, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$w_perc, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$w_perc, as.factor(games$won), center='mean')  # homogeneity of variance assumption violated (since p-value < 0.05)
t.test(games$w_perc[games$won], games$w_perc[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$w_perc[games$won], games$w_perc[!games$won])


## relationship between win percentage margin and game outcome
ggplot(games) + 
  geom_density(aes(x=w_perc_margin, fill=won), alpha=0.5) + 
  facet_grid(season ~ .)
tapply(games$w_perc_margin, games$won, function(x) { mean(x, na.rm=TRUE) })
leveneTest(games$w_perc_margin, as.factor(games$won), center='mean')  # homogeneity of variance assumption not violated (since p-value > 0.05)
t.test(games$w_perc_margin[games$won], games$w_perc_margin[!games$won], paired=FALSE, var.equal=FALSE)
cohensD(games$w_perc_margin[games$won], games$w_perc_margin[!games$won])


## relationship between win percentage margin and point spread
ggplot(games, aes(x=w_perc_margin, y=pts_margin)) + 
  geom_point() + 
  stat_smooth(method='lm') 


## relationship between lead changes and points margin
ggplot(games) + 
  geom_point(aes(x=lead_changes, y=abs(pts_margin))) + 
  facet_grid(season ~ .)


## What are the "patterns" of per-quarter points and what is their relationship with game outcomes?

