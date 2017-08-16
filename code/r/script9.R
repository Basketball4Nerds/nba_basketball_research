

## implement a function for table shown in the following link:
# - https://www.teamrankings.com/nba/odds-history/results/
# - http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/trends/league/season.html

## What is power ranking? How is it ranked?
# - http://www.covers.com/sports/nba/powerrankings

## check out this page later:
# - http://www.covers.com/pageLoader/pageLoader.aspx?page=/data/nba/statistics/2016-2017/statistics_playoffs.html




#############
## see how 

fav <- subset(master, line < 0)
und <- subset(master, line > 0)

table(fav$p + fav$line > fav$pA)
ddply(fav, 'season', function(x) {
  table(x$p + x$line > x$pA)
})

table(und$p + und$line > und$pA)
ddply(und, 'season', function(x) {
  table(x$p + x$line > x$pA)
})

# 100 + (+4.5) > 103
# 120 + (+4.5) > 103
# 100 + (-4.5) > 103
# 105 + (-4.5) > 103
# 120 + (-4.5) > 103




#### analysis

## find best predictors for points and opponent points allowed (visual)
# x <- master[complete.cases(master), ]
# numVars <- getNumericVarNames(x)
# numVars <- setdiff(numVars, 'season')
# corMtx <- cor(x[ , numVars])
# corrplot(corMtx, method='ellipse')
# 
# best sma predictors of pts: pts_sma, o_pts_alwd_to_o_sma
# best sma predictors of pts_alwd_to_o: pts_alwd_to_o_sma, o_pts_sma

## find best weights to predict pts
# result0 <- optim(par=c(0, 0), 
#                  fn=calcAvgPredErr2,
#                  df=x, 
#                  outcomeVar='pts', 
#                  indVar=c('pts_sma', 'o_pts_alwd_to_o_sma'),
#                  control = list(maxit = 400))
# result0
# calcAvgPredErr(x, outcomeVar='pts', projVar='pts_sma')
# calcAvgPredErr(x, outcomeVar='pts', projVar='o_pts_alwd_to_o_sma')

## find best weights to predict pts_alwd_to_os
# result1 <- optim(par=c(0, 0), 
#                  fn=calcAvgPredErr2,
#                  df=x, 
#                  outcomeVar='pts_alwd_to_o', 
#                  indVar=c('pts_alwd_to_o_sma', 'o_pts_sma'),
#                  control = list(maxit = 400))
# result1
# calcAvgPredErr(x, outcomeVar='pts_alwd_to_o', projVar='pts_alwd_to_o_sma')
# calcAvgPredErr(x, outcomeVar='pts_alwd_to_o', projVar='o_pts_sma')



#### implement first prediction strategy
master$pts_proj <- (master$pts_sma + master$o_pts_alwd_to_o_sma) / 2
master$pts_alwd_to_o_proj <- (master$pts_alwd_to_o_sma + master$o_pts_sma) / 2
master$pts_margin_proj <- master$pts_proj - master$pts_alwd_to_o_proj
master$total_pts_proj <- master$pts_proj + master$pts_alwd_to_o_proj

master$win_proj <- ifelse(master$pts_margin_proj > 0, TRUE,
                          ifelse(master$pts_margin_proj < 0, FALSE, NA))
master$win_proj_after_adj <- ifelse(master$pts_proj + master$adjustor > master$pts_alwd_to_o_proj, TRUE,
                                    ifelse(master$pts_proj + master$adjustor < master$pts_alwd_to_o_proj, FALSE, NA))
master$over_under_proj <- ifelse(master$total_pts_proj < master$total_pts_proj_om, 'under', 
                                 ifelse(master$total_pts_proj > master$total_pts_proj_om, 'over', NA))



#### analysis: evaluate projections on points, opponent points, and points margin

## carve out data for analysis
bt1 <- master[ , c('season', 'date', 'team', 'o_team',
                   'won', 'pts', 'pts_alwd_to_o', 'pts_margin',  # actual
                   'win_proj', 'pts_proj', 'pts_alwd_to_o_proj', 'pts_margin_proj',  # projected by model
                   'win_proj_om', 'pts_proj_om', 'pts_alwd_to_o_proj_om', 'pts_margin_proj_om'  # projected by odds-makers
)]

## subset rows with complete data
bt1 <- bt1[complete.cases(bt1), ]

## break df into a list of dfs
bt1Lst <- split(bt1, f=bt1$season)

## compare accuracy: points projected by the model vs points projected by odds-makers
calcAvgPredErr(bt1, outcomeVar='pts', projVar='pts_proj')
calcAvgPredErr(bt1, outcomeVar='pts', projVar='pts_proj_om')
sum(abs(bt1$pts - bt1$pts_proj), na.rm=TRUE)
sum(abs(bt1$pts - bt1$pts_proj_om), na.rm=TRUE)
a <- bt1[ , c('pts', 'pts_proj', 'pts_proj_om')]
cor(a)

## compare accuracy: points projected by the model vs points projected by odds-makers
calcAvgPredErr(bt1, outcomeVar='pts_alwd_to_o', projVar='pts_alwd_to_o_proj')
calcAvgPredErr(bt1, outcomeVar='pts_alwd_to_o', projVar='pts_alwd_to_o_proj_om')
sum(abs(bt1$pts_alwd_to_o - bt1$pts_alwd_to_o_proj), na.rm=TRUE)
sum(abs(bt1$pts_alwd_to_o - bt1$pts_alwd_to_o_proj_om), na.rm=TRUE)
b <- bt1[ , c('pts_alwd_to_o', 'pts_alwd_to_o_proj', 'pts_alwd_to_o_proj_om')]
cor(b)

## compare accuracy: points projected by the model vs points projected by odds-makers
calcAvgPredErr(bt1, outcomeVar='pts_margin', projVar='pts_margin_proj')
calcAvgPredErr(bt1, outcomeVar='pts_margin', projVar='pts_margin_proj_om')
sum(abs(bt1$pts_margin - bt1$pts_margin_proj), na.rm=TRUE)
sum(abs(bt1$pts_margin - bt1$pts_margin_proj_om), na.rm=TRUE)
c <- bt1[ , c('pts_margin', 'pts_margin_proj', 'pts_margin_proj_om')]
cor(c)



#### analysis: evaluate factors that correlate with game outcomes (with and without adjustments)

## carve out data for analysis
bt2 <- master[ , c('season', 'date', 'team', 'o_team', 'adjustor',
                   'won', 'win_proj_om', 'win_proj',
                   'win_proj_after_adj', 'won_after_adj'
)]

## subset rows with complete data
bt2 <- bt2[complete.cases(bt2), ]

## break df into a list of dfs
bt2Lst <- split(bt2, f=bt2$season)

## see how often the model correctly predict games' outcomes (wins/losses); 
## around 63% of the times, the model correctly predicts the winner
d <- table(bt2$won, bt2$win_proj)
d
sum(diag(d)) / sum(d)
lapply(bt2Lst, function(df) {
  x <- table(df$won, df$win_proj) 
  sum(diag(x)) / sum(x)
})

## see how often odds-makers correctly predict games' outcomes (wins/losses);
## around 68% of the times, odd-makers correctly predict the winner
e <- table(bt2$won, bt2$win_proj_om)
e
sum(diag(e)) / sum(e)
lapply(bt2Lst, function(df) {
  x <- table(df$won, df$win_proj_om) 
  sum(diag(x)) / sum(x)
})

## analyze game outcomes after adjustment vs. game outcomes;
## result: around 80% of the times, winner remains winner and loser remains loser, even after point spread adjustments;
f <- table(bt2$won_after_adj, bt2$won)
f
sum(diag(f)) / sum(f)
lapply(bt2Lst, function(df) {
  x <- table(df$won_after_adj, df$won) 
  sum(diag(x)) / sum(x)
})
## an idea for a better point spreads betting strategy: predict who would win the game 
## and place a bet on the favored team regardless of the handicap

## analyze game outcomes after adjustments vs. projected game outcome by odds-makers;
## favored team, after points adjustments, is equally likely to come out as a winner as a loser; 
## similarly, underdog team, after points adjustments, is equally likely to come out as a winner as a loser;
## poor correlation;
g <- table(bt2$won_after_adj, bt2$win_proj_om)
g
sum(diag(g)) / sum(g)
lapply(bt2Lst, function(df) {
  x <- table(df$won_after_adj, df$win_proj_om) 
  sum(diag(x)) / sum(x)
})

## analyze game outcomes after adjustments vs. projected game outcome by the model;
## favored team, after points adjustments, is equally likely to come out as a winner as a loser; 
## similarly, underdog team, after points adjustments, is equally likely to come out as a winner as a loser;
## poor correlation;
h <- table(bt2$won_after_adj, bt2$win_proj)
h
sum(diag(h)) / sum(h)
lapply(bt2Lst, function(df) {
  x <- table(df$won_after_adj, df$win_proj) 
  sum(diag(x)) / sum(x)
})

## analyze game outcomes after adjustments vs the model's projected game outcomes after adjustments
i <- table(bt2$won_after_adj, bt2$win_proj_after_adj)
i
sum(diag(i)) / sum(i)
lapply(bt2Lst, function(df) {
  x <- table(df$won_after_adj, df$win_proj_after_adj) 
  sum(diag(x)) / sum(x)
})



#### analysis: evaluate over-and-under betting prediction performance
bt3 <- master[ , c('season', 'date', 'team', 'o_team',
                   'total_pts',  # actual
                   'total_pts_proj',  # projected by model
                   'total_pts_proj_om',  # projected by odds-makers
                   'over_under', 'over_under_proj'
)]

## subset rows with complete data
bt3 <- bt3[complete.cases(bt3), ]

## break df into a list of dfs
bt3Lst <- split(bt3, f=bt3$season)

## compare accuracy: total points projected by the model vs total points projected by odds-makers
calcAvgPredErr(bt3, outcomeVar='total_pts', projVar='total_pts_proj')
calcAvgPredErr(bt3, outcomeVar='total_pts', projVar='total_pts_proj_om')
sum(abs(bt3$total_pts - bt3$total_pts_proj), na.rm=TRUE)
sum(abs(bt3$total_pts - bt3$total_pts_proj_om), na.rm=TRUE)
j <- bt3[ , c('total_pts', 'total_pts_proj', 'total_pts_proj_om')]
cor(j)

## analyze over-and-under vs the model's projected over-and-under
## not very accurate;
k <- table(bt3$over_under, bt3$over_under_proj)
k
sum(diag(k)) / sum(k)
lapply(bt3Lst, function(df) {
  x <- table(df$over_under, df$over_under_proj) 
  sum(diag(x)) / sum(x)
})










#### what qualifies as a "decisive" wins?
## decisive wins
hist(master$pMrgn)
qplot(master$pMrgn, geom="histogram") 
mean(master$pMrgn)
sd(master$pMrgn)
