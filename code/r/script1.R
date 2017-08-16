## read games data cache
# games <- read.csv('./data/games.csv', stringsAsFactors=FALSE)
# games$date <- as.Date(games$date)


#### feature addition to master df

## copy games df to master
master <- games

## exclude playoff games (for now since playoff data is not available for 1995 - 2001)
master <- subset(master, playoffs==0)  # flaw in data; does not filter out all playoff games
x <- ddply(master, c('team', 'season'), nrow)
unique(x$V1)

## change home/away to H/A for site variable
master$site <- ifelse(master$site=='home', 'H', 'A')

# ## create opponent site variable
# ## this is redundant info but provides programmatic
# ## convenience for createWinPred() much later
# master$o_site <- ifelse(master$site=='H', 'A', 'H')

## un-factor teams (for later, for the left_join() function to work)
master$team <- as.character(master$team)
master$o_team <- as.character(master$o_team)

## remove data points for future games
master <- master[!is.na(master$pts), ]

## set to correct data types
# master$season <- as.factor(master$season)
# master$playoffs <- as.factor(master$playoffs)

## shorten column names
names(master) <- gsub('pts', 'p', names(master))
names(master) <- gsub('wins', 'w_cumcnt_gen', names(master))
names(master) <- gsub('losses', 'l_cumcnt_gen', names(master))
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
names(master) <- gsub('quarter_scores', 'qtrpts', names(master))
names(master) <- gsub('biggest_lead', 'bgstLd', names(master))
names(master) <- gsub('matchup', 'mtch', names(master))
names(master) <- gsub('margin_after_first', 'pMrgn1q', names(master))
names(master) <- gsub('margin_at_half', 'pMrgnHlf', names(master))
names(master) <- gsub('margin_after_third', 'pMrgn3q', names(master))

## change column names (essential for data processing step later)
# names(master)[names(master)=='total'] <- 'om_proj_total'
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
names(master)[names(master)=='o_qtrpts'] <- 'qtrptsA' # opponent quarter points allowed
names(master)[names(master)=='o_bgstLd'] <- 'bgstLdA' # opponent biggest lead allowed

## add game number cols
master$n_cumcnt_gen <- master$w_cumcnt_gen + master$l_cumcnt_gen
master$o_n_cumcnt_gen <- master$o_w_cumcnt_gen + master$o_l_cumcnt_gen

## add conference information
master$cnf <- TeamCityConfDf$Conference[match(master$team, TeamCityConfDf$Team)]
master$o_cnf <- TeamCityConfDf$Conference[match(master$o_team, TeamCityConfDf$Team)]

## combined points of both teams
master$pTotal <- master$p + master$pA

## add pts columns for each quarter/overtime and a column for number of overtimes
master <- addQtrOtPtsCols(master, qtrPtsCols=c('qtrpts', 'qtrptsA'))

## add column for regular quarter points scored and allowed 
## (need these columns to calculate more realistic SMAs for P and PA)
master$rqP <- master$pQ1 + master$pQ2 + master$pQ3 + master$pQ4
master$rqPA <- master$pQ1A + master$pQ2A + master$pQ3A + master$pQ4A

## re-do point margin by quarter (due to possible presence of missing data)
master$pMrgn1q <- master$pQ1 - master$pQ1A
master$pMrgnHlf <- (master$pQ1 + master$pQ2) - (master$pQ1A + master$pQ2A)
master$pMrgn3q <- (master$pQ1 + master$pQ2 + master$pQ3) - (master$pQ1A + master$pQ2A + master$pQ3A)

## point margin after game
master$pMrgn <- master$p - master$pA

## matchup w-l differential
# master$mtch_mrgn <- master$mtch_w - master$mtch_l
master$mtchmrgn <- master$mtch_w - master$mtch_l

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

## conditional whether games was won
master$won <- master$p > master$pA


## create backup
write.csv(master, './data/master_backup.csv', row.names=FALSE)
# master <- read.csv('./data/master_backup.csv', stringsAsFactors=FALSE)
# master$date <- as.Date(master$date)



## prepare spreads and moneyline columns (for later use)

# filter out extra columns
spreads <- spreads[ , c('season', 'date', 'team', 'o_team', 'bookmaker_line', 'bookmaker_payout')]
moneylines <- moneylines[ , c('season', 'date', 'team', 'o_team', 'bookmaker_ml')]

# filter out missing rows
spreads <- spreads[complete.cases(spreads), ]
moneylines <- moneylines[complete.cases(moneylines), ]

# add game outcome column to dfs
join_cols <- c('date', 'team', 'o_team')
spreads <- left_join(spreads, master[ , c(join_cols, 'won', 'pMrgn')], by=join_cols)
moneylines <- left_join(moneylines, master[ , c(join_cols, 'won')], by=join_cols)

# change pMrgn col name to p_mrgn
names(spreads)[names(spreads)=='pMrgn'] <- 'p_mrgn'
names(moneylines)[names(moneylines)=='pMrgn'] <- 'p_mrgn'