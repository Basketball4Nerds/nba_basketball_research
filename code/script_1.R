#### feature addition to master df

## copy games df to master
master <- games

## exclude playoff games (for now since playoff data is not available for 1995 - 2001)
ddply(master, 'season', function(x) {table(x$playoffs)})
master <- subset(master, playoffs==0)  # flaw in data; does not filter out all playoff games
master <- subset(master, n < 82)
ddply(master, c('team', 'season'), nrow)

## change home/away to H/A for site variable
master$site <- ifelse(master$site=='home', 'H', 'A')

## un-factor teams (for later, for the left_join() function to work)
master$team <- as.character(master$team)
master$o_team <- as.character(master$o_team)

## create opponent site variable 
## this is redundant info but provides programmatic 
## convenience for createWinPred() much later
master$o_site <- ifelse(master$site=='H', 'A', 'H')

## remove data points for future games
master <- master[!is.na(master$pts), ]

## set to correct data types
# master$season <- as.factor(master$season)
master$playoffs <- as.factor(master$playoffs)

## shorten column names
names(master) <- gsub('pts', 'p', names(master))
names(master) <- gsub('wins', 'w', names(master))
names(master) <- gsub('losses', 'l', names(master))
names(master) <- gsub('rest', 'rst', names(master))
names(master) <- gsub('steals', 'stl', names(master))
names(master) <- gsub('assists', 'ast', names(master))
names(master) <- gsub('blocks', 'blk', names(master))
names(master) <- gsub('offensive_rebounds', 'oRb', names(master))
names(master) <- gsub('defensive_rebounds', 'dRb', names(master))
names(master) <- gsub('rebounds', 'rb', names(master))
names(master) <- gsub('turnovers', 'trnovr', names(master))
names(master) <- gsub('fouls', 'fl', names(master))
names(master) <- gsub('field_goals_attempted', 'FGA', names(master))
names(master) <- gsub('field_goals_made', 'FGM', names(master))
names(master) <- gsub('three_pointers_attempted', 'FGA3x', names(master))
names(master) <- gsub('three_pointers_made', 'FGM3x', names(master))
names(master) <- gsub('free_throws_attempted', 'FTA', names(master))
names(master) <- gsub('free_throws_made', 'FTM', names(master))
names(master) <- gsub('p(ts)?_in_paint', 'pPnt', names(master))
names(master) <- gsub('fast_break_p(ts)?', 'pFb', names(master))
names(master) <- gsub('quarter_scores', 'pQtr', names(master))
names(master) <- gsub('biggest_lead', 'bgstLd', names(master))
names(master) <- gsub('matchup', 'mtch', names(master))
names(master) <- gsub('lead_changes', 'ldChng', names(master))
names(master) <- gsub('margin_after_first', 'pMrgn1q', names(master))
names(master) <- gsub('margin_at_half', 'pMrgnHlf', names(master))
names(master) <- gsub('margin_after_third', 'pMrgn3q', names(master))

## change column names (essential for data processing step later)
names(master)[names(master)=='total'] <- 'om_proj_total'
names(master)[names(master)=='o_p'] <- 'pA'  # opponent points allowed
names(master)[names(master)=='o_stl'] <- 'stlA' # opponent steals allowed
names(master)[names(master)=='o_ast'] <- 'astA' # opponent assists allowed
names(master)[names(master)=='o_blk'] <- 'blkA' # opponent blocks allowed
names(master)[names(master)=='o_dRb'] <- 'dRbA' # opponent def rebs allowed
names(master)[names(master)=='o_oRb'] <- 'oRbA' # opponent off rebs allowed
names(master)[names(master)=='o_rb'] <- 'rbA' # opponent rebs allowed
names(master)[names(master)=='o_trnovr'] <- 'trnovrFcd' # opponent turnovers forced
names(master)[names(master)=='o_fl'] <- 'flFcd' # opponent fouls forced 
names(master)[names(master)=='o_FGA'] <- 'FGAA' # opponent field goal attempts allowed
names(master)[names(master)=='o_FGM'] <- 'FGMA' # opponent field goals made allowed
names(master)[names(master)=='o_FGA3x'] <- 'FGA3xA' # opponent 3-pt shot attempts allowed
names(master)[names(master)=='o_FGM3x'] <- 'FGM3xA' # opponent 3-pt shots made allowed
names(master)[names(master)=='o_FTA'] <- 'FTAA' # opponent free throws attempts allowed
names(master)[names(master)=='o_FTM'] <- 'FTMA' # opponent free throws made allowed
names(master)[names(master)=='o_pPnt'] <- 'pPntA' # opponent points allowed in paint
names(master)[names(master)=='o_pFb'] <- 'pFbA' # opponent fast-break points allowed
names(master)[names(master)=='o_pQtr'] <- 'pQtrA' # opponent quarter points allowed
names(master)[names(master)=='o_bgstLd'] <- 'bgstLdA' # opponent biggest lead allowed

## add game number cols
master$n <- master$w + master$l
master$o_n <- master$o_w + master$o_l

## add conference information
master$cnf <- TeamCityConfDf$Conference[match(master$team, TeamCityConfDf$Team)]
master$o_cnf <- TeamCityConfDf$Conference[match(master$o_team, TeamCityConfDf$Team)]

## remove lead change column (no information in the column due to error in API)
master$ldChng <- NULL

## combined points of both teams
master$pTotal <- master$p + master$pA

## add pts columns for each quarter/overtime and a column for number of overtimes
master <- addQtrOtPtsCols(master, qtrPtsCols=c('pQtr', 'pQtrA'))

## re-do point margin by quarter (due to possible presence of missing data)
master$pMrgn1q <- master$pQ1 - master$pQ1A
master$pMrgnHlf <- (master$pQ1 + master$pQ2) - (master$pQ1A + master$pQ2A)
master$pMrgn3q <- (master$pQ1 + master$pQ2 + master$pQ3) - (master$pQ1A + master$pQ2A + master$pQ3A)

## point margin after game
master$pMrgn <- master$p - master$pA

## add column for regular quarter points scored and allowed 
## (need these columns to calculate more realistic SMAs for P and PA)
master$rqP <- master$pQ1 + master$pQ2 + master$pQ3 + master$pQ4
master$rqPA <- master$pQ1A + master$pQ2A + master$pQ3A + master$pQ4A

## match w-l differential
master$mtch_mrgn <- master$mtch_w - master$mtch_l

## conditional whether games was won
master$won <- master$p > master$pA

## points made from 3-pointers
master$p3x <- master$FGM3x * 3
master$p3xA <- master$FGM3xA * 3

## points made from 2-pointers
master$p2x <- master$p - master$p3x - master$FTM
master$p2xA <- master$pA - master$p3xA - master$FTMA

## number of 2-pointers made
master$FGM2x <- master$FGM - master$FGM3x
master$FGM2xA <- master$FGMA - master$FGM3xA

## number of 2-pointers attempted
master$FGA2x <- master$FGA - master$FGA3x
master$FGA2xA <- master$FGAA - master$FGA3xA

## field goal percentage
master$FGP <- master$FGM / master$FGA
master$FGPA <- master$FGMA / master$FGAA

## 3-pointer FG percentage
master$FGP3x <- master$FGM3x / master$FGA3x
master$FGP3xA <- master$FGM3xA / master$FGA3xA

## 2-pointer FG percentage
master$FGP2x <- master$FGM2x / master$FGA2x
master$FGP2xA <- master$FGM2xA / master$FGA2xA

## free throw percentage
master$FTP <- master$FTM / master$FTA
master$FTPA <- master$FTMA / master$FTAA

## percentage of points from 3-pointers
master$p3xShr <- master$p3x / master$p
master$p3xShrA <- master$p3xA / master$pA

## percentage of points from 2-pointers
master$p2xShr <- master$p2x / master$p
master$p2xShrA <- master$p2xA / master$pA

## percentage of points from free throws
master$pFtShr <- master$FTM / master$p
master$pFtShrA <- master$FTMA / master$pA

## percentage of points in the paint
master$pPntShr <- master$pPnt / master$p
master$pPntShrA <- master$pPntA / master$pA

## percentage of points from fast breaks
master$pFbShr <- master$pFb / master$p
master$pFbShrA <- master$pFbA / master$pA

## rebound metrics 
master$oRbShr <- master$oRb / master$rb
master$dRbShr <- master$dRb / master$rb
master$ODRR <- master$oRb / master$dRb

master$oRbShrA <- master$oRbA / master$rbA
master$dRbShrA <- master$dRbA / master$rbA
master$ODRRA <- master$oRbA / master$dRbA

master$oRbPc <- master$oRb / (master$oRb + master$dRbA)
master$dRbPc <- master$dRb / (master$dRb + master$oRbA)
master$ODRSR <- master$oRbPc / master$dRbPc

master$oRbPcA <- master$oRbA / (master$oRbA + master$dRb)
master$dRbPcA <- master$dRbA / (master$dRbA + master$oRb)
master$ODRSRA <- master$oRbPcA / master$dRbPcA

## estimated possessions
master$pos <- master$FGA + (0.44 * master$FTA) - master$oRb + master$trnovr
master$posA <- master$FGAA + (0.44 * master$FTAA) - master$oRbA + master$trnovrFcd

## offensive efficiency: points per possesion x100
## defensive efficiency: points allowed per possession x100
master$oeff <- master$p / master$pos * 100
master$deff <- master$pA / master$pos * 100

## opponent offensive efficiency: points per possession x100
## opponent defensive efficiency: points allowed per possession x100
master$oeffA <- master$pA / master$posA * 100
master$deffA <- master$p / master$posA * 100

## turnover percentage (turnover per possession)
master$toPc <- master$trnovr / master$pos
master$toPcFcd <- master$trnovrFcd / master$posA

## ability to get to the foul line
master$FTA_p_FGA <- master$FTA / master$FGA
master$FTAA_p_FGAA <- master$FTAA / master$FGAA

## add juice propagation columns (j and o_j)
master <- addJCols(master, init_j=100, dist_wgts=c(0.05, 0.1, 0.15))

## round digits to 3 decimal places
master <- round_df(master, 3)

## create backup
write.csv(x, './data/master_backup.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)

## add various general cumulative performance columns
master <- add_cum_perf_cols(master, 
                            metric=c('oeff', 'oeffA', 
                                     'FGP', 'FGPA', 
                                     'rqP', 'rqPA',
                                     'pos', 'posA'), 
                            vary_by=NULL,
                            add_opp_cols=TRUE)

## create backup 2
write.csv(master, './data/master_backup2.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup2.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)

## add columns for offensive and defensive rankings
master <- add_rnk_cols(master, 
                       metric=c('oeff_cumperf_gen', 'oeffA_cumperf_gen'), 
                       higher_num_bttr_perf=c(TRUE, FALSE),
                       method='qntl',
                       add_opp_cols=TRUE)

## create backup 3
write.csv(master, './data/master_backup3.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup3.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)


## add general win percentage
master <- add_wpc_cols(master, 
                       vary_by=NULL, 
                       rm_n_cnt_cols=FALSE,
                       add_opp_cols=TRUE)

## add variable-specific (e.g. site-specific) win percentage
# x <- add_wpc_cols(master, 
#                        vary_by=c('site', 'cnf', 
#                                  'oeff_qntl_rnk', 
#                                  'oeffA_qntl_rnk'),
#                        rm_n_cnt_cols=FALSE,
#                        add_opp_cols=TRUE)
## FIX HERE; SEE tail(x)



x <- master[ , c('season', 'date', 
                 'team', 'o_team', 
                 'site', 'o_cnf', 
                 'o_oeff_qntl_rnk', 'o_oeffA_qntl_rnk', 
                 'won')]
x2012 <- subset(x, season==2012)
bucks2012 <- subset(x2012, team=='Bucks')

bucks2012_1 <- add_wpc_cols(bucks2012, vary_by=c('site', 'cnf'))
bucks2012_1 <- sortByCol(bucks2012_1, col=c('team', 'o_cnf'))
head(bucks2012_1)





## create backup 4
write.csv(master, './data/master_backup4.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup4.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)

## variable-specific (e.g. conference-specific) performance


## define columns for SMA calculations
cols <- c('rqP', 'rqPA', 'FGP', 'FGPA', 'PPP', 'PPPA')
smaCols <- c('p')

## add SMA columns 

## site specific
## opponent conference specific
## opponent rank specific

y <- master[master$season==1995, c('season', 'date', 'team', 'o_team', 'won', 'site', 'o_cnf', smaCols)]

## general
y0 <- addMaCols(df=y, type='sma', n=10, cols='p', aggVars=c('team', 'season'), colApndStr='_gen')

## site-specific
y1 <- addMaCols(df=y, type='sma', n=10, cols=smaCols, aggVars=c('team', 'season', 'site'), colApndStr='_ssp')

## cnf-specific
y2 <- addMaCols(df=y, type='sma', n=10, cols=smaCols, aggVars=c('team', 'season', 'o_cnf'), colApndStr='_cfsp')

## off-grp-specific
y3 <- addMaCols(df=y, type='sma', n=10, cols=smaCols, aggVars=c('team', 'season', 'gPPP'), colApndStr='_ogsp')

## def-grp-specific
y4 <- addMaCols(df=y, type='sma', n=10, cols=smaCols, aggVars=c('team', 'season', 'gPPPA'), colApndStr='_dgsp')

## site-cnf-specific
table(master_df$OG)
table(master_df$DG)

## site



y <- sortByCol(y, col=c('date', 'team'))
y0 <- sortByCol(y0, col=c('date', 'team'))
y1 <- sortByCol(y1, col=c('date', 'team'))
y2 <- sortByCol(y2, col=c('date', 'team'))
# y3 <- sortByCol(y3, col=c('date', 'team'))
# y4 <- sortByCol(y4, col=c('date', 'team'))

z <- cbind(y, y0[ , 14:19], y1[ , 14:19], y2[ , 14:19])
head(z)

 

#### various SMA metrics
## - PPP SMA
## - PPPA SMA
##
## - regular quarter points SMA
## - opponent's regular quarter points allowed SMA
##
## - point mrgn SMA
## - opponent's point mrgn SMA
##
## - FG percentage SMA
## - opponent's FG percentage allowed SMA
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
