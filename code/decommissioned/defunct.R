#### snippet 1
x$won <- as.factor(x$won)
rfModel1 <- randomForest(won ~ wins + losses + o.team + o.wins + o.losses + wl.ratio + o.wl.ratio + wl.ratio.diff, data=x)
rfModel2 <- randomForest(won ~ o.team + wl.ratio + o.wl.ratio + wl.ratio.diff, data=x)
varImpPlot(rfModel1)
varImpPlot(rfModel2)

predRf1 <- predict(rfModel1)
predRf2 <- predict(rfModel2)
table(predRf1, x$won)
table(predRf2, x$won)

t <- cavsDfLst[[7]]
predRf3 <- predict(rfModel1, newdata=t)
predRf4 <- predict(rfModel2, newdata=t)
a <- table(predRf3, t$won)
b <- table(predRf4, t$won)

calcAccFrConfMtx(a)
calcAccFrConfMtx(b)




#### snippet 2
won <- subset(x, won)
lost <- subset(x, !won)

outcomeVar <- 'won'
numPredVars <- c('points', 'wins', 'losses', 'o.points', 'o.wins', 'o.losses', 
                 'wl.ratio', 'o.wl.ratio', 'wl.ratio.diff', 'pts.margin', 
                 'wins.margin', 'o.wins.margin', 
                 'wins.diff', 'losses.diff')
misVars <- c('season', 'date', 'team', 'o.team')

# Temporarily remove the outcome variable before scaling the data set
outcomeVals <- x[[outcomeVar]]
# scale returns a matrix so we need to tranform it back to a data frame
x_scaled <- as.data.frame(scale(x[ , numPredVars]))
x_scaled[[outcomeVar]] <- outcomeVals

# split your data sets
x_scaled_won <- x_scaled[x_scaled[[outcomeVar]], ]
x_scaled_lost <- x_scaled[!x_scaled[[outcomeVar]], ]


GetSummaryPlot <- function(objdfscaled0, objdfscaled1, predictorName, plotit=TRUE) {
  stats0 <- (summary(objdfscaled0[,predictorName]))
  stats0 <- c(stats0[1:6])
  stats1 <- (summary(objdfscaled1[,predictorName]))
  stats1 <- c(stats1[1:6])
  stats <- data.frame('ind'=c(1:6), 'stats1'=stats1,'stats0'=stats0)
  
  spread <- ((stats1[[1]] - stats0[[1]]) +
               (stats1[[2]] - stats0[[2]]) +
               (stats1[[3]] - stats0[[3]]) +
               (stats1[[4]] - stats0[[4]]) +
               (stats1[[5]] - stats0[[5]]) +
               (stats1[[6]] - stats0[[6]]))
  
  if (plotit) {
    print(paste('Scaled spread:',spread))
    p <- ggplot(data=stats, aes(ind)) +
      geom_line(aes(y = stats1, colour = "stats1")) +
      geom_line(aes(y = stats0, colour = "stats0")) +
      scale_x_discrete(breaks = 1:6,
                       labels=c("min","1q","median","mean","3q","max")) +
      ylab(predictorName) + xlab(paste('Spread:',spread))
    return (p)
  } else {
    return (spread)
  }
}

predictorNames <- numPredVars
for (predictorName in predictorNames)
  print(paste(predictorName,':',GetSummaryPlot(x_scaled_won,
                                               x_scaled_lost, predictorName, plotit=FALSE)))



#### snippet 3
#impVars <- c('wl.ratio', 'o.wl.ratio', 'wl.ratio.diff', 'wins.diff', 'losses.diff')
x <- cavsDfLst[[15]]
numPredVars <- c('points', 'wins', 'losses', 'o.points', 'o.wins', 'o.losses', 
                 'wl.ratio', 'o.wl.ratio', 'wl.ratio.diff', 'pts.margin', 
                 'wins.margin', 'o.wins.margin', 
                 'wins.diff', 'losses.diff')
y <- x[ , numPredVars]
corMtx <- cor(y)
corrplot(corMtx, method='ellipse')



#### snippet 4
ggplot(x, aes(x=points, y=points.avg.n5)) + 
  geom_point(aes(color=won)) + 
  stat_smooth(method='lm')

cor.test(x$pts.margin, x$pts.margin.proj)
a <- ggplot(x, aes(x=pts.margin, y=pts.margin.proj)) + 
  geom_point(aes(color=won)) + 
  stat_smooth(method='lm')
b <- ggplot(x, aes(x=pts.margin, y=pts.margin.proj.2)) + 
  geom_point(aes(color=won)) + 
  stat_smooth(method='lm')
grid.arrange(a, b, ncol=2)



#### snippet 5
x <- tmsDf[complete.cases(tmsDf), ]
xLst <- split(x, f=x$season)
yLst <- lapply(xLst, function(df) {
  optim(par=c(0.5, 0.5), 
        fn=calcAvgPredErr2,
        df=df, 
        outcomeVar='pts', 
        indVar=c('pts.avg.n8', 'o.o.pts.alwd.avg.n8'),
        control = list(maxit = 400))
})

for (i in yLst) {
  print(i$par)
  print(i$value)
  print('---')
}



#### snippet 6
# c(1, 1, 1, 1)
# c('W', 'L', 'W', 'L')
# 100, -100, 100, -100
# 
# c(0.5, 1, 0.5, 1)
# c('W', 'L', 'W', 'L')
# 50, -100, 50, -100
# 
# c(0.9, 1, 0.9, 1)
# c('W', 'L', 'W', 'L')
# 90, -100, 90, -100
# 
# 100 / 110
# x + y = 1
# wins: 100 * x * 0.90
# losses: 100 * y * 1 
# 
# 100 * x * 1 - 100 * y * 1 >= 0
# 100 * x * 0.91 - 100 * y * 1 >= 0
# 100 * x * 0.91 - 100 * (1 - x) * 1 >= 0
# 91 * x - 100 + 100 * x >= 0
# 191 * x >= 100
# 100 / (100 + 100/110*100)
#
# 100 / (100 + 100/105*100)
#
# 100 / (100 + 100/104*100)



#### snippet 7
t <- odds[seq(1, nrow(odds), 2), ]
v <- odds[seq(2, nrow(odds), 2), ]
s <- abs(t$pts.margin.bet) != abs(v$pts.margin.bet)
x <- t[s, ]
y <- v[s, ]
tail(x)
tail(y)



#### snippet 8
bt2 <- master[ , c('season', 'date', 'team', 'o_team', 'adjustor', 
                   'won', 'won_after_adj',
                   'win_proj', 'win_proj_after_adj'
)]

bt2 <- bt2[complete.cases(bt2), ]
names(bt2) <- c('season', 'date', 'team', 'o_team', 'adjustor', 'outcome', 'outcome_after_adj', 'proj_outcome', 'proj_outcome_after_adj')
bt2$outcome <- ifelse(bt2$outcome, 'won', 'lost')
bt2$outcome_after_adj <- ifelse(bt2$outcome_after_adj, 'won', 'lost')
bt2$proj_outcome <- ifelse(bt2$proj_outcome, 'won', 'lost')
bt2$proj_outcome_after_adj <- ifelse(bt2$proj_outcome_after_adj, 'won', 'lost')

bt2 <- subset(bt2, season==2014)
tail(bt2, 30)

x <- table(bt2$outcome_after_adj, bt2$proj_outcome)
sum(diag(x)) / sum(x)

y <- table(bt2$outcome_after_adj, bt2$proj_outcome_after_adj)
sum(diag(y)) / sum(y)



#### snippet 9
#' SDQL <- sprintf('season, date, site, overtime, playoffs, 
#'                     
#'                     team, points, wins, losses, steaks, rest, 
#'                     steals, assists, blocks, defensive rebounds, offensive rebounds, rebounds, turnovers, fouls,
#'                     field goals attempted, field goals made, three pointers attempted, three pointers made,
#'                     quarter scores, biggest lead, lead changes, 
#' 
#'                     o:team, o:points, o:wins, o:losses, o:rest,
#'                     o:steals, o:assists, o:blocks, o:defensive rebounds, o:offensive rebounds, o:rebounds, o:turnovers, o:fouls,
#'                     o:field goals attempted, o:field goals made, o:three pointers attempted, o:three pointers made,
#'                     o:quarter scores, o:biggest lead, o:lead changes,
#'                     
#'                     matchup wins, matchup losses, 
#'                     total, line
#'                     @ date=%s',
#'                 format(as.Date(date), '%Y%m%d'))



## example of 503 page upon too many API requests
# webpage <- "<html>\r\n<head><title>503 Service Temporarily Unavailable</title></head>\r\n<body bgcolor=\"white\">\r\n<center><h1>503 Service Temporarily Unavailable</h1></center>\r\n<hr><center>nginx/1.4.6 (Ubuntu)</center>\r\n</body>\r\n</html>\r\n"

## example of 599 timeout error page
# webpage <-"json_callback({'html': 'HTTP 599: Timeout'});"

## 
a <- getRawGamesDataViaApi(date='2016/11/20')
reqUrl <- 'http://api.sportsdatabase.com/nba/query.json?sdql=season,%20date,%20site,%20playoffs,%20team,%20points,%20wins,%20losses,%20rest,%20quarter%20scores,%20biggest%20lead,%20o:team,%20o:points,%20o:wins,%20o:losses,%20o:rest,%20o:steals,%20o:assists,%20o:blocks,%20o:defensive%20rebounds,%20o:offensive%20rebounds,%20o:rebounds,%20o:turnovers,%20o:fouls,%20o:field%20goals%20attempted,%20o:field%20goals%20made,%20o:three%20pointers%20attempted,%20o:three%20pointers%20made,%20o:free%20throws%20attempted,%20o:free%20throws%20made,%20o:points%20in%20the%20paint,%20o:fast%20break%20points,%20o:quarter%20scores,%20o:biggest%20lead,%20matchup%20wins,%20matchup%20losses,%20lead%20changes,%20margin%20after%20the%20first,%20margin%20at%20the%20half,%20margin%20after%20the%20third,%20total,%20line%20@%20date=20161120&output=json&api_key=guest'
webpage <- getURL(reqUrl)
webpage <- gsub('\t', '', webpage)
webpage <- gsub('^json_callback\\(', '', webpage)
webpage <- gsub('\\);\n$', '', webpage)
webpage <- gsub("'", '"', webpage)


## QA tests
table(zzz$wVsE + zzz$wVsW + zzz$lVsE + zzz$lVsW == zzz$n)
table(zzz$wVsE + zzz$wVsW == zzz$w)
table(zzz$lVsE + zzz$lVsW == zzz$l)
table(zzz$wH + zzz$wA == zzz$w)
table(zzz$lH + zzz$lA == zzz$l)

table(zzz$o_wVsE + zzz$o_wVsW + zzz$o_lVsE + zzz$o_lVsW == zzz$o_n)
table(zzz$o_wVsE + zzz$o_wVsW == zzz$o_w)
table(zzz$o_lVsE + zzz$o_lVsW == zzz$o_l)
table(zzz$o_wH + zzz$o_wA == zzz$o_w)
table(zzz$o_lH + zzz$o_lA == zzz$o_l)




## checking out pQtr column
# master[nchar(test)==0, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==9, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==10, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==11, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==12, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==13, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==14, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==15, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==16, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==17, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==18, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==19, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==20, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==21, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]
# master[nchar(test)==22, c('season', 'date', 'team', 'o_team', 'pQtr')][1:10, ]



## offense/defense rank algorithm 
## methodology outlined here: https://pdfs.semanticscholar.org/da05/3c70773944765ff16a1a6a3da956d8e329f6.pdf
x <- master[master$season=='1995', c('season', 'date', 'team', 'o_team', 'p', 'pA')]
y <- subset(x, date=='1995-11-03')
teams <- unique(y$team)

n <- length(teams)
A <- matrix(0, nrow=n, ncol=n)
colnames(A) <- teams
rownames(A) <- teams
d <- matrix(rep(1, n), nrow=n, ncol=1)

for (k in 1:nrow(y)) {
  # k <- 1
  i <- which(x$o_team[k]==teams)
  j <- which(x$team[k]==teams)
  A[i, j] <- x$p[k]
}

o <- t(A) %*% (1 / d)
d <- A %*% (1 / o)



## check out PPP distribution
mean <- mean(master$PPP)
sd <- sd(master$PPP)
qplot(master$PPP, geom="histogram") +
  geom_vline(xintercept=mean, color='red', size=1) + 
  geom_vline(xintercept=mean + sd, color='red', size=1) + 
  geom_vline(xintercept=mean + 2 * sd, color='red', size=1) +
  geom_vline(xintercept=mean - sd, color='red', size=1) + 
  geom_vline(xintercept=mean - 2 * sd, color='red', size=1)



#### offense/defense rank algorithm 
df <- master[master$season==2015 & master$playoffs==0, 
             c('season', 'date', 'team', 'o_team', 'PPP', 'PPPA', 'won')]

## create PPP matrix
pppM <- createPppMatrix(df)

## create rank matrix
dRnkM <- createDefRnkMatrix(pppM)
oRnkM <- createOffRnkMatrix(pppM)

## exploratory
apply(dRnkM, 2, function(x) round(mean(x, na.rm=TRUE), 0))
apply(dRnkM, 2, function(x) round(median(x, na.rm=TRUE), 0))
hist(apply(dRnkM, 2, function(x) round(mean(x, na.rm=TRUE), 0)))

apply(oRnkM, 2, function(x) round(mean(x, na.rm=TRUE), 0))
apply(oRnkM, 2, function(x) round(median(x, na.rm=TRUE), 0))
hist(apply(oRnkM, 2, function(x) round(mean(x, na.rm=TRUE), 0)))

## create graded rank matrix
gDRnkM <- createGradedRnkM(dRnkM, k=3)
gORnkM <- createGradedRnkM(oRnkM, k=3)

## frequency and percentage distribution of d rank votes
for (i in 1:30) {
  print('--------')
  x <- table(gDRnkM[ , i])
  y <- round(prop.table(table(gDRnkM[ , i])), 3)
  z <- rbind(x, y)
  print(z)
  print(chisq.test(x)$p.value)
  print(paste(colnames(gDRnkM)[i], i))
}

## create vectors "true" grades
tOffGrades <- returnCollectiveGrading(gORnkM, pValThres=0.1)
tDefGrades <- returnCollectiveGrading(gDRnkM, pValThres=0.1)

tOffGrades[tOffGrades %in% c('AB', 'BC')] <- 'B'
tDefGrades[tDefGrades %in% c('AB', 'BC')] <- 'B'

x <- tapply(df$PPP, df$team, mean)
y <- tapply(df$PPPA, df$team, mean)

sort(x)
rev(sort(tOffGrades))

sort(y)
sort(tDefGrades)



#### create offensive/defensive rank group columns 
df <- master[master$season %in% 1995:2017 & master$playoffs==0, 
             c('season', 'date', 'team', 'o_team', 'PPP', 'PPPA', 'won')]
df <- addSmaCols(df=df, n=10, cols=c('PPP', 'PPPA'), aggVars=c('team', 'season'), orderCol='date')
df <- addABCGradeCol(df=df, metric='PPP_sma', higherNumBetterPerf=TRUE, eff=FALSE)
df2 <- addABCGradeCol(df=df, metric='PPP_sma', higherNumBetterPerf=TRUE, eff=TRUE)






#### SMA FUNCTIONS

## this function calculates simple moving average
# calcSmaVals <- function(vals, n) {
#   len <- length(vals)
#   if (len==1) return(NA)
#   if (len <= n) n <- len
#   
#   runVals <- SMA(vals, n)
#   runVals <- c(NA, runVals[-length(runVals)])
#   
#   runVals[2] <- vals[1]
#   if (n > 2) {
#     for (i in 3:n) {
#       pRunVals <- SMA(vals[1:i-1], i-1)
#       pRunVals <- pRunVals[length(pRunVals)]
#       runVals[i] <- pRunVals
#     }
#   }
#   return(runVals)
# }
calcSmaVals <- function(vals, n) {
  
  ## condition whether a vector's vaues end in NA
  vecValsEndInNA <- is.na(vals[length(vals)])
  
  if (vecValsEndInNA) {
    
    ## temporarily remove the last value (which is NA)
    vals <- vals[-length(vals)]    
  }
  
  len <- length(vals)
  if (len==1) return(NA)
  if (len <= n) n <- len
  
  runVals <- SMA(vals, n)
  
  for (i in 1:n) {
    runVals[i] <- mean(vals[1:i])
  }
  
  if (vecValsEndInNA)
    runVals <- c(NA, runVals)
  else 
    runVals <- c(NA, runVals[-length(runVals)])
  
  return(runVals)
}

## this function adds new columns for running values 
## (e.g. simple moving averages or running standard deviations)
addSmaCols <- function(df, n, cols,  aggVars=NULL, orderCol=NULL, rndDgt=3) {
  
  ## base case
  if (is.null(aggVars)) {
    
    ## order the base subsets
    if (!is.null(orderCol))
      df <- df[order(df[[orderCol]]), ]
    
    ## in case n is a vector of integers
    #     for (i in n) {
    #       for (col in cols) {
    #         runVals <- calcSmaVals(vals=df[[col]], n=i)
    #         runValsColNm <- paste0(col, '_avg_n', i)
    #         df[[runValsColNm]] <- runVals
    #       }
    #     }
    for (col in cols) {
      runVals <- calcSmaVals(vals=df[[col]], n=n)
      runValsColNm <- paste0(col, '_sma')
      runVals <- round(runVals, rndDgt)
      df[[runValsColNm]] <- runVals
    }
    
    return(df)
  }
  
  df <- sortByCol(df, aggVars, asc=TRUE)
  outputDF <- ddply(df, aggVars, function(x) {
    addSmaCols(df=x, n=n, cols=cols, aggVars=NULL, orderCol=orderCol, rndDgt=rndDgt)
  })
  return(outputDF)
}



## this function gets a moving average of the n previous games
## for a given team in a given metric
getMvAvg <- function(gmsRawDf, var, tm, focDate, n) {
  
  ## get season
  sn <- gmsRawDf$season[gmsRawDf$date==focDate][1]
  
  ## get team's current season data up to a given focus date
  df <- subset(gmsRawDf, team==tm & season==sn & date < focDate)
  vals <- df[[var]]
  
  ## get start and end indices to subset values for averaging
  length <- length(vals)
  strtIndex <- length - n + 1
  
  ## subset values for averaging
  if (strtIndex > 1)
    vals <- vals[strtIndex:length]
  
  ## obtain moving average
  mvAvg <- mean(vals, na.rm=TRUE)
  
  ## return
  return(mvAvg)
}

## up-to-date average; decommission to use cummean() function instead
UTDA <- function(x) {
  
  ## calculate the number of values in vector
  len <- length(x)
  
  ## initialize vector to return with NAs to fill in its values
  runVals <- rep(NA, len)
  
  ## for each indices from 2nd index, calculate SMA with ever-increasing value of n
  for (i in 2:len) {
    runVal <- SMA(x[1:i], i)[i]
    runVals[i] <- runVal  
  }
  
  ## return
  return(runVals)
}


## QA for calcMovAvgVals()
calcMovAvgVals(1:10, type='cummean')
calcMovAvgVals(1:10, type='SMA', n=5, coverLessThanN=FALSE)
calcMovAvgVals(1:10, type='SMA', n=21, coverLessThanN=FALSE)
calcMovAvgVals(1:10, type='SMA', n=5, coverLessThanN=TRUE)
calcMovAvgVals(1:10, type='SMA', n=21, coverLessThanN=TRUE)




#### OLD WAY OF COMPUTER WIN PERCENTAGES
#### (REPLACED BY FUNCTION)
## win percentage
master$wPc <- master$w / master$n
master$o_wPc <- master$o_w / master$o_n

## win percentage at home/away
master$wPcH <- master$wH / master$nH
master$wPcA <- master$wA / master$nA
master$o_wPcH <- master$o_wH / master$o_nH
master$o_wPcA <- master$o_wA / master$o_nA

## win percentage against E/W teams
master$wPcVsE <- master$wVsE / master$nVsE
master$wPcVsW <- master$wVsW / master$nVsW
master$o_wPcVsE <- master$o_wVsE / master$o_nVsE
master$o_wPcVsW <- master$o_wVsW / master$o_nVsW

## win percentage against A/B/C offense teams
master$wPcVsOGA <- master$wVsOGA / master$nVsOGA
master$wPcVsOGB <- master$wVsOGB / master$nVsOGB
master$wPcVsOGC <- master$wVsOGC / master$nVsOGC
master$o_wPcVsOGA <- master$o_wVsOGA / master$o_nVsOGA
master$o_wPcVsOGB <- master$o_wVsOGB / master$o_nVsOGB
master$o_wPcVsOGC <- master$o_wVsOGC / master$o_nVsOGC

## win percentage against A/B/C defense teams
master$wPcVsOGA <- master$wVsOGA / master$nVsOGA
master$wPcVsOGB <- master$wVsOGB / master$nVsOGB
master$wPcVsOGC <- master$wVsOGC / master$nVsOGC
master$o_wPcVsOGA <- master$o_wVsOGA / master$o_nVsOGA
master$o_wPcVsOGB <- master$o_wVsOGB / master$o_nVsOGB
master$o_wPcVsOGC <- master$o_wVsOGC / master$o_nVsOGC



#### rearrange columns for better grouping, readability, and organization




## this function adds running count columns of wins/losses 
## by home/away, opponent def/off rank, opponent conf,
## and combinations of those
# addRunWinLossGmCntCols <- function(df) {
#   
#   ## original cols
#   origCols <- colnames(df)
#   
#   ## running tallies should contain only per-season-per-team counts
#   df <- ddply(df, c('season', 'team'), function(x) {
#     n <- nrow(x)
#     x <- sortByCol(x, col='date')
#     
#     ## counts at home/away
#     x$wH <- c(0, cumsum(x$won & x$site=='home')[-n])
#     x$wA <- c(0, cumsum(x$won & x$site=='away')[-n])
#     x$lH <- c(0, cumsum(!x$won & x$site=='home')[-n])
#     x$lA <- c(0, cumsum(!x$won & x$site=='away')[-n])
#     
#     ## counts vs. E/W
#     x$wVsE <- c(0, cumsum(x$won & x$o_cnf=='E')[-n])
#     x$wVsW <- c(0, cumsum(x$won & x$o_cnf=='W')[-n])
#     x$lVsE <- c(0, cumsum(!x$won & x$o_cnf=='E')[-n])
#     x$lVsW <- c(0, cumsum(!x$won & x$o_cnf=='W')[-n])
#     
#     ## counts against A/B/C offense team
#     x$wVsOGA <- c(0, cumsum(x$won & x$o_OG=='A')[-n])
#     x$wVsOGB <- c(0, cumsum(x$won & x$o_OG=='B')[-n])
#     x$wVsOGC <- c(0, cumsum(x$won & x$o_OG=='C')[-n])
#     x$lVsOGA <- c(0, cumsum(!x$won & x$o_OG=='A')[-n])
#     x$lVsOGB <- c(0, cumsum(!x$won & x$o_OG=='B')[-n])
#     x$lVsOGC <- c(0, cumsum(!x$won & x$o_OG=='C')[-n])
#     
#     ## counts against A/B/C defense team    
#     x$wVsDGA <- c(0, cumsum(x$won & x$o_DG=='A')[-n])
#     x$wVsDGB <- c(0, cumsum(x$won & x$o_DG=='B')[-n])
#     x$wVsDGC <- c(0, cumsum(x$won & x$o_DG=='C')[-n])
#     x$lVsDGA <- c(0, cumsum(!x$won & x$o_DG=='A')[-n])
#     x$lVsDGB <- c(0, cumsum(!x$won & x$o_DG=='B')[-n])
#     x$lVsDGC <- c(0, cumsum(!x$won & x$o_DG=='C')[-n])
#     
#     ## counts against E/W at home/away
#     x$wHVsE <- c(0, cumsum(x$won & x$site=='home' & x$o_cnf=='E')[-n])
#     x$wHVsW <- c(0, cumsum(x$won & x$site=='home' & x$o_cnf=='W')[-n])
#     x$wAVsE <- c(0, cumsum(x$won & x$site=='away' & x$o_cnf=='E')[-n])
#     x$wAVsW <- c(0, cumsum(x$won & x$site=='away' & x$o_cnf=='W')[-n])
#     x$lHVsE <- c(0, cumsum(!x$won & x$site=='home' & x$o_cnf=='E')[-n])
#     x$lHVsW <- c(0, cumsum(!x$won & x$site=='home' & x$o_cnf=='W')[-n])
#     x$lAVsE <- c(0, cumsum(!x$won & x$site=='away' & x$o_cnf=='E')[-n])
#     x$lAVsW <- c(0, cumsum(!x$won & x$site=='away' & x$o_cnf=='W')[-n])
#     
#     ## counts against OGA/OGB/OGC at home/away
#     x$wHVsOGA <- c(0, cumsum(x$won & x$site=='home' & x$o_OG=='A')[-n])
#     x$wHVsOGB <- c(0, cumsum(x$won & x$site=='home' & x$o_OG=='B')[-n])
#     x$wHVsOGC <- c(0, cumsum(x$won & x$site=='home' & x$o_OG=='C')[-n])
#     x$wAVsOGA <- c(0, cumsum(x$won & x$site=='away' & x$o_OG=='A')[-n])
#     x$wAVsOGB <- c(0, cumsum(x$won & x$site=='away' & x$o_OG=='B')[-n])
#     x$wAVsOGC <- c(0, cumsum(x$won & x$site=='away' & x$o_OG=='C')[-n])
#     x$lHVsOGA <- c(0, cumsum(!x$won & x$site=='home' & x$o_OG=='A')[-n])
#     x$lHVsOGB <- c(0, cumsum(!x$won & x$site=='home' & x$o_OG=='B')[-n])
#     x$lHVsOGC <- c(0, cumsum(!x$won & x$site=='home' & x$o_OG=='C')[-n])
#     x$lAVsOGA <- c(0, cumsum(!x$won & x$site=='away' & x$o_OG=='A')[-n])
#     x$lAVsOGB <- c(0, cumsum(!x$won & x$site=='away' & x$o_OG=='B')[-n])
#     x$lAVsOGC <- c(0, cumsum(!x$won & x$site=='away' & x$o_OG=='C')[-n])
#     
#     ## counts against DGA/DGB/DGC at home/away
#     x$wHVsDGA <- c(0, cumsum(x$won & x$site=='home' & x$o_DG=='A')[-n])
#     x$wHVsDGB <- c(0, cumsum(x$won & x$site=='home' & x$o_DG=='B')[-n])
#     x$wHVsDGC <- c(0, cumsum(x$won & x$site=='home' & x$o_DG=='C')[-n])
#     x$wAVsDGA <- c(0, cumsum(x$won & x$site=='away' & x$o_DG=='A')[-n])
#     x$wAVsDGB <- c(0, cumsum(x$won & x$site=='away' & x$o_DG=='B')[-n])
#     x$wAVsDGC <- c(0, cumsum(x$won & x$site=='away' & x$o_DG=='C')[-n])
#     x$lHVsDGA <- c(0, cumsum(!x$won & x$site=='home' & x$o_DG=='A')[-n])
#     x$lHVsDGB <- c(0, cumsum(!x$won & x$site=='home' & x$o_DG=='B')[-n])
#     x$lHVsDGC <- c(0, cumsum(!x$won & x$site=='home' & x$o_DG=='C')[-n])
#     x$lAVsDGA <- c(0, cumsum(!x$won & x$site=='away' & x$o_DG=='A')[-n])
#     x$lAVsDGB <- c(0, cumsum(!x$won & x$site=='away' & x$o_DG=='B')[-n])
#     x$lAVsDGC <- c(0, cumsum(!x$won & x$site=='away' & x$o_DG=='C')[-n])
#     
#     x
#   })
#   
#   ## total number of games played
#   df$n <- df$w + df$l
#   
#   ## number of games at home/away
#   df$nH <- df$wH + df$lH
#   df$nA <- df$wA + df$lA
#   
#   ## number of games against E/W conf team
#   df$nVsE <- df$wVsE + df$lVsE
#   df$nVsW <- df$wVsW + df$lVsW
#   
#   ## number of games against A/B/C off team
#   df$nVsOGA <- df$wVsOGA + df$lVsOGA
#   df$nVsOGB <- df$wVsOGB + df$lVsOGB
#   df$nVsOGC <- df$wVsOGC + df$lVsOGC
#   
#   ## number of games against A/B/C def team
#   df$nVsDGA <- df$wVsDGA + df$lVsDGA
#   df$nVsDGB <- df$wVsDGB + df$lVsDGB
#   df$nVsDGC <- df$wVsDGC + df$lVsDGC
#   
#   ## number of games at home/away against E/W
#   df$nHVsE <- df$wHVsE + df$lHVsE
#   df$nHVsW <- df$wHVsW + df$lHVsW
#   df$nAVsE <- df$wAVsE + df$lAVsE
#   df$nAVsW <- df$wAVsW + df$lAVsW
#   
#   ## number of games at home/away against A/B/C offense group
#   df$nHVsOGA <- df$wHVsOGA + df$lHVsOGA
#   df$nHVsOGB <- df$wHVsOGB + df$lHVsOGB
#   df$nHVsOGC <- df$wHVsOGC + df$lHVsOGC
#   df$nAVsOGA <- df$wAVsOGA + df$lAVsOGA
#   df$nAVsOGB <- df$wAVsOGB + df$lAVsOGB
#   df$nAVsOGC <- df$wAVsOGC + df$lAVsOGC
#   
#   ## number of games at home/away against A/B/C defense group
#   df$nHVsDGA <- df$wHVsDGA + df$lHVsDGA
#   df$nHVsDGB <- df$wHVsDGB + df$lHVsDGB
#   df$nHVsDGC <- df$wHVsDGC + df$lHVsDGC
#   df$nAVsDGA <- df$wAVsDGA + df$lAVsDGA
#   df$nAVsDGB <- df$wAVsDGB + df$lAVsDGB
#   df$nAVsDGC <- df$wAVsDGC + df$lAVsDGC
#   
#   ## names of new columns
#   newCols <- setdiff(colnames(df), origCols)
#   
#   ## create running tally columns for opponent
#   df <- fillInOpCols(df, cols=newCols)
#   
#   ## return 
#   return(df)
# }



#### create win prediction test
a <- createWinPred(df, metric='site')
b <- createWinPred(df, metric='line')
c <- createWinPred(df, metric='mtch_mrgn')
d <- createWinPred(df, metric='j')
e <- createWinPred(df, metric='rst')
f <- createWinPred(df, metric='wPc')
g <- createWinPred(df, metric='wPc', by=c('site', 'cnf'))
table(a)
table(b)
table(c)
table(d)
table(e)
table(f)
table(g)

a <- createWinPred(master_df, metric='wPc', by='site')
b <- createWinPred(master_df, metric='wPc', by='cnf')
c <- createWinPred(master_df, metric='wPc', by='OG')
d <- createWinPred(master_df, metric='wPc', by='DG')
e <- createWinPred(df, metric='wPc', by=c('site', 'cnf'))
f <- createWinPred(df, metric='wPc', by=c('site', 'OG'))
g <- createWinPred(df, metric='wPc', by=c('site', 'DG'))
table(a)
table(b)
table(c)
table(d)
table(e)
table(f)
table(g)


params_df <- expand.grid(metric=c('j', 'wPc'), by=NA, n_min=c(5, 10, 20), min_diff=NA)
params_lst <- convertParamsDfToLst(params_df)



## this function creates a df of win predictions based on variable-specific metric
## (e.g. conf-specific win percentage, site-specific win percentage, etc.)
createVarSpWinPredDf <- function(master_df, metric, by_lst, n_min=5) {
  
  ## initialize empty list to store vectors of predictions
  pred_lst <- list()
  
  ## make prediction using each metric
  for (by in by_lst) {
    pred <- createWinPred(master_df=master_df, metric=metric, by=by, n_min=n_min)
    pred_lst <- c(pred_lst, list(pred))
  }
  
  ## label the list elements
  names(pred_lst) <- paste0('w_pred_by_', metric, '_', unlist(lapply(by_lst, paste0, collapse='_')), '_sp')
  
  ## convert list of predictions to df
  pred_df <- do.call(cbind.data.frame, pred_lst)
  
  ## return
  return(pred_df)
}



# ## points per possesion and points allowed per possession (for team)
# master$PPP <- master$p / master$pos
# master$PAPP <- master$pA / master$pos
# 
# ## points per possession and points allowed per possession (for opponent)
# master$PPPA <- master$pA / master$posA
# master$PFcdPPA <- master$p / master$posA


# ## add SMA columns for PPP, PPPA
# ## (required for offensive/defense grouping)
# master <- addMaCols(df=master, type='cummean',
#                     cols=c('PPP', 'PPPA', 'FGP', 'FGPA', 'rqP', 'rqPA'),
#                     aggVars=c('team', 'season'), colApndStr='_gen')
# 
# ## using PPP_sma and PPPA_sma, create off/def rank group columns
# master <- addABCGradeCol(df=master, eff=TRUE, minN=10, method='qntl',
#                          metrics=c('PPP_cummean_gen', 'PPPA_cummean_gen',
#                                    'FGP_cummean_gen', 'FGPA_cummean_gen',
#                                    'rqP_cummean_gen', 'rqPA_cummean_gen'),
#                          higherNumBetterPerf=c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
# 
# ## create OG (offense group) and DG (defense group) columns
# ## based on majority vote method using three separate metrics
# master$OG <- retByMajorityVote(master[ , c('gPPP', 'gFGP', 'gRqP')])
# master$DG <- retByMajorityVote(master[ , c('gPPPA', 'gFGPA', 'gRqPA')])
# 
# ## create off/def rank group columns for opponent teams
# master <- fillInOpCols(df=master, cols=c('OG', 'DG'))



# createVarDf <- function(by=c('site', 'cnf', 'OG', 'DG'),
#                         include.opp.cols=TRUE,
#                         metric=c('w_pc')) {
# 
#   ## possible combinations are:
#   # - site-cnf
#   # - site-OG
#   # - site-DG
#   ## not allowed combinations are:
#   # - cnf-OG
#   # - cnf-DG
#   # - OG-DG
# 
#   ## weed out error cases
#   by <- unique(by)
#   if (!all(by %in% c('site', 'cnf', 'OG', 'DG'))) stop('Incorrect variable given. Try again.')
#   if (length(by) > 2) stop('Too many variables given. Please limit to 2.')
#   else if (length(by)==2 && !('site' %in% by)) stop('Incorrect variable combinations provided. With two variable combinations, one must be site.')
# 
#   ## specify variable options
#   site_opts <- c('H', 'A')
#   cnf_opts <- c('E', 'W')
#   OG_opts <- c('A', 'B', 'C')
#   DG_opts <- c('A', 'B', 'C')
# 
#   ## create varDf by expanding options
#   if (length(by)==1) {
#     opts <- get(paste0(by, '_opts'))
#     varDf <- expand.grid(opts, opts)
#     names(varDf) <- c(by, paste0('o_', by))
#   }  else if (length(by)==2) {
#     opts1 <- get(paste0(by[1], '_opts'))
#     opts2 <- get(paste0(by[2], '_opts'))
#     varDf <- expand.grid(opts1, opts2, opts1, opts2)
#     names(varDf) <- c(by[1], by[2], paste0('o_', by[1]), paste0('o_', by[2]))
#   }
# 
#   ## weed out incorrect cases (e.g. two teams both can't have home games)
#   if ('site' %in% names(varDf)) {
#     varDf <- varDf[varDf$site != varDf$o_site, ]
#   }
# 
#   ## initialize values
#   wPcCols <- rep('wPc', nrow(varDf))
#   o_wPcCols <- rep('o_wPc', nrow(varDf))
# 
#   ## append H/A specificity
#   if ('site' %in% names(varDf)) {
#     wPcCols <- paste0(wPcCols, varDf$site)
#     o_wPcCols <- paste0(o_wPcCols, varDf$o_site)
#   }
# 
#   ## append vs E/W specificity
#   if ('cnf' %in% names(varDf)) {
#     wPcCols <- paste0(wPcCols, 'Vs', varDf$o_cnf)
#     o_wPcCols <- paste0(o_wPcCols, 'Vs', varDf$cnf)
#   }
# 
#   ## append vs offense group specificity
#   if ('OG' %in% names(varDf)) {
#     wPcCols <- paste0(wPcCols, 'VsOG', varDf$o_OG)
#     o_wPcCols <- paste0(o_wPcCols, 'VsOG', varDf$OG)
#   }
# 
#   ## append vs defense group specificity
#   if ('DG' %in% names(varDf)) {
#     wPcCols <- paste0(wPcCols, 'VsDG', varDf$o_DG)
#     o_wPcCols <- paste0(o_wPcCols, 'VsDG', varDf$DG)
#   }
# 
#   ## add win and n game columns
#   wCols <- gsub('^wPc', 'w', wPcCols)
#   nCols <- gsub('^wPc', 'n', wPcCols)
#   o_wCols <- gsub('^o_wPc', 'o_w', o_wPcCols)
#   o_nCols <- gsub('^o_wPc', 'o_n', o_wPcCols)
# 
#   ## incorporate the comparable metrics into the variation df
#   varDf <- cbind(varDf,
#                  wPcCol=wPcCols, o_wPcCol=o_wPcCols,
#                  nCol=nCols, o_nCol=o_nCols, wCol=wCols, o_wCol=o_wCols)
# 
#   ## if opponent metrics are not desired
#   if (!include.opp.cols) {
#     varDf <- varDf[ , !grepl('o_', names(varDf))]
#     varDf <- unique(varDf)
#   }
# 
#   ## apply as.character function to each column
#   varDf <- sapply(varDf, as.character)
# 
#   ## turn back into data frame
#   varDf <- as.data.frame(varDf, stringsAsFactors=FALSE)
# 
#   ## return
#   return(varDf)
# }



## this function adds running count columns of wins/losses
## by home/away, opponent def/off rank, opponent conf,
## and combinations of those
addRunWinLossGmCntCols <- function(df) {

  ## original cols
  origCols <- colnames(df)

  ## create variable-by list
  by_lst <- list('site', 'cnf')

  ## create variable-by df list
  var_df_lst <- lapply(by_lst, function(x) {
    createVarDf(by=x)
  })

  ## running tallies should contain only per-season-per-team counts
  df <- ddply(df, c('season', 'team'), function(x) {
    n <- nrow(x)
    x <- sortByCol(x, col='date')

    ## for each "by" variation and combination
    for (i in 1:length(by_lst)) {

      ## select by and var_df
      by <- by_lst[[i]]
      var_df <- var_df_lst[[i]]

      ## if single-variable (e.g. by H/A or by E/W, etc.)
      if (length(by)==1) {

        ## create column selector
        selector <- ifelse(by=='site', by, paste0('o_', by))

        ## add running win count column and n-game column
        for (j in 1:nrow(var_df)) {
          wCntMetric <- var_df[j, 'wCols']
          nGmMetric <- var_df[j, 'nCols']
          x[ , wCntMetric] <- c(0, cumsum(x$won & x[[selector]]==var_df[j, selector])[-n])
          x[ , nGmMetric] <- c(0, cumsum(x[[selector]]==var_df[j, selector])[-n])
        }
      }

      ## if multi-variable (two at most, with one being by "site")
      else if (length(by)==2) {

        ## create second column selector (first selector is always "site" when there are two by variaables)
        selector <- paste0('o_', by[2])

        ## add running win count column and n-game column
        for (j in 1:nrow(var_df)) {
          wCntMetric <- var_df[j, 'wCols']
          nGmMetric <- var_df[j, 'nCols']
          x[ , wCntMetric] <- c(0, cumsum(x$won & x$site==var_df$site[j] & x[[selector]]==var_df[j, selector])[-n])
          x[ , nGmMetric] <- c(0, cumsum(x$site==var_df$site[j] & x[[selector]]==var_df[j, selector])[-n])
        }
      }

    }

    x
  })

  ## names of new columns
  newCols <- setdiff(colnames(df), origCols)

  ## create running tally columns for opponent
  df <- fillInOpCols(df, cols=newCols)

  ## return
  return(df)
}




## this function adds running count columns of wins/losses 
## by home/away, opponent def/off rank, opponent conf,
## and combinations of those
## create variable-by list
addRunCntCols <- function(master_df, 
                          cnt_type=c('w', 'l', 'n'),
                          by=list('site', 'cnf')) {
  
  ## original cols
  orig_cols <- colnames(master_df)
  
  ## sort by date
  master_df <- sortByCol(master_df, col='date')
  
  ## for each season-team combo
  ## (running tallies should contain only per-season-per-team counts)
  master_df <- ddply(master_df, c('season', 'team'), function(x) {
    
    ## calculate total number of games played by team per season
    n <- nrow(x)
    
    ## for each variable-specification, add counts
    for (by_elem in by) {
      
      ## create variable-specific var_df
      var_df <- createVarDf(by=by_elem)
      
      ## for each variability dimension (e.g. by site, by opponent conference, etc.)
      for (j in 1:nrow(var_df)) {
        
        tm_tag <- var_df[j, 'tm_tags']
        o_tag <- var_df[j, 'o_tags']
        var_df_row <- var_df[j, ]
        varsp_ind <- createVarSpIndex(master_df=x, var_df_row=var_df_row, n_min=0)
        
        if ('w' %in% cnt_type)
          x[ , paste0('w', tm_tag)] <- c(0, cumsum(x$won & varsp_ind)[-n])
        if ('l' %in% cnt_type)
          x[ , paste0('l', tm_tag)] <- c(0, cumsum(!x$won & varsp_ind)[-n])
        if ('n' %in% cnt_type)
          x[ , paste0('n', tm_tag)] <- c(0, cumsum(varsp_ind)[-n])
      }
    }
    
    ## return 
    x
  })
  
  ## get new columns created
  new_cols <- setdiff(colnames(master_df), orig_cols)
  
  ## create win percentage columns for opponent
  master_df <- fillInOpCols(master_df, cols=new_cols)
  
  ## return 
  return(master_df)
}





# ## using PPP_sma and PPPA_sma, create off/def rank group columns
# master <- addABCGradeCol(df=master, eff=TRUE, minN=10, method='qntl',
#                          metrics=c('PPP_cummean_gen', 'PPPA_cummean_gen',
#                                    'FGP_cummean_gen', 'FGPA_cummean_gen',
#                                    'rqP_cummean_gen', 'rqPA_cummean_gen'),
#                          higherNumBetterPerf=c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE))
# 
# ## create OG (offense group) and DG (defense group) columns
# ## based on majority vote method using three separate metrics
# master$OG <- retByMajorityVote(master[ , c('gPPP', 'gFGP', 'gRqP')])
# master$DG <- retByMajorityVote(master[ , c('gPPPA', 'gFGPA', 'gRqPA')])
# 
# ## create off/def rank group columns for opponent teams
# master <- fillInOpCols(df=master, cols=c('OG', 'DG'))




####
## add general cumulative sum columns for FGM, FGA, FGMA, FGAA
master <- addCumSumCols(master, cols=c('FGM', 'FGA', 'FGMA', 'FGAA'), 
                        agg_vars=c('team', 'season'), new_colnm_apnd_str='gen')

## general up-to-date FGP
master$FGP_cum_gen <- master$FGM_cumsum_gen / master$FGA_cumsum_gen

## general up-to-date FGPA
master$FGPA_cum_gen <- master$FGMA_cumsum_gen / master$FGAA_cumsum_gen




## this function adds running count columns of wins/losses 
## as well as running sum columns of other numeric values
## by home/away, opponent def/off rank, opponent conf, etc.
add_varsp_runsum_cols <- function(master_df, cols, 
                                  by=list('site', 'cnf'), add_opp_cols) {
  
  ## original cols
  orig_cols <- colnames(master_df)
  
  ## for each season-team combo
  ## (running tallies should contain only per-season-per-team counts)
  master_df <- ddply(master_df, c('season', 'team'), function(x) {
    
    ## sort by date
    x <- sortByCol(x, col='date')
    
    ## calculate total number of games played by team per season
    n <- nrow(x)
    
    ## for each variable-specification, add counts
    for (by_elem in by) {
      
      ## create variable-specific var_df
      var_df <- createVarDf(by=by_elem)
      
      ## for each variability dimension (e.g. by site, by opponent conference, etc.)
      for (j in 1:nrow(var_df)) {
        
        tm_tag <- var_df[j, 'tm_tags']
        o_tag <- var_df[j, 'o_tags']
        var_df_row <- var_df[j, ]
        varsp_ind <- createVarSpIndex(master_df=x, var_df_row=var_df_row, n_min=0)
        
        ## for each column
        for (col in cols) {
          
          ## adding w, l, n counts by variable specificity
          if (col %in% c('w', 'l', 'n')) {
            if (col=='w')
              x[ , paste0('w', tm_tag)] <- c(0, cumsum(x$won & varsp_ind)[-n])
            else if (col=='l')
              x[ , paste0('l', tm_tag)] <- c(0, cumsum(!x$won & varsp_ind)[-n])
            else if (col=='n')
              x[ , paste0('n', tm_tag)] <- c(0, cumsum(varsp_ind)[-n])
          }
          
          ## adding cumulative sum by variable specificity
          else {
            target_vals <- rep(0, n)
            target_vals[varsp_ind] <- x[[col]][varsp_ind]
            x[ , paste0(col, tm_tag)] <- c(0, cumsum(target_vals)[-n])
          }
        }
      }
    }
    
    ## return 
    x
  })
  
  ## add opponent columns
  if (add_opp_cols) {
    
    ## get new columns created
    new_cols <- setdiff(colnames(master_df), orig_cols)
    
    ## create win percentage columns for opponent
    master_df <- fill_in_opp_cols(master_df, cols=new_cols)
  }
  
  ## return 
  return(master_df)
}



## for each aggregation variable
# master_df <- subset(master, season==2012 & team=='Bucks')
# master_df2 <- master_df[ , c('season', 'date', 'team', 'site', 'o_cnf', 'won')]
# x <- add_cum_cnt_cols(master_df2, cols='n', vary_by=c('site'))
# y <- add_cum_cnt_cols(master_df2, cols='n', vary_by=c('cnf'))
# z <- add_cum_cnt_cols(master_df2, cols='n', vary_by=c('site', 'cnf'))



#### various SMA metrics
##
## - point mrgn SMA
## - opponent's point mrgn SMA
##
## - 3-pointer FG percentage SMA
## - opponent's 3-pointer FG percentage allowed SMA
## 
## - 2-pointer FG percentage SMA
## - opponent's 2-pointer FG percentage allowed SMA
##
## - assists SMA
## - steals SMA
## - opponent's blocks SMA
## - opponent's turnovers SMA
## - fouls SMA
## 
## - (percentage of) points from 3-pointers SMA
## - (percentage of) points from 2-pointers SMA
## - (percentage of) points from free throws SMA
## - (percentage of) points in paint SMA
## - (percentage of) points from fast breaks SMA
##
## - (percentage of) rebounds while on defense SMA
## - (percentage of) rebounds while on offense SMA
##
## - lead changes SMA
## - biggest leads SMA
##
## - matchup stats
## - rest
## - opponent's rest
