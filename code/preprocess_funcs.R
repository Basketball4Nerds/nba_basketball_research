############ PREPROCESSOR FUNCTIONS ################

## this function takes in df and "fills in" the selected metrics
## for opponent by join method
fillInOpCols <- function(df, cols) {
  
  ## make a partial copy of df
  df_pc <- df[ , c('date', 'team', cols)]
  
  ## add o_ prefix to all column names except the date
  names(df_pc) <- paste0('o_', names(df_pc))
  names(df_pc)[names(df_pc)=='o_date'] <- 'date'
  
  ## make a left join
  df <- left_join(df, df_pc, by=c('date', 'o_team'))
  
  ## return 
  return(df)
}


## this function adds running count columns of wins/losses 
## by home/away, opponent def/off rank, opponent conf,
## and combinations of those
addRunWinLossGmCntCols <- function(df) {
  
  ## original cols
  origCols <- colnames(df)
  
  ## create variable-by list
  by_lst <- list('site', 
                 'cnf', 
                 'OG', 
                 'DG', 
                 c('site', 'cnf'), 
                 c('site', 'OG'), 
                 c('site', 'DG'))
  
  ## create variable-by df list
  var_df_lst <- lapply(by_lst, function(x) {
    createVarDf(by=x, type='tm-opp')
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


## this function add win percentage columns
addWinPcCols <- function(df) {
  
  ## get original columns
  origCols <- colnames(df)
  
  ## get win count column names
  winCntCols <- colnames(df)[grepl('^w', colnames(df))]  
  winCntCols <- setdiff(winCntCols, 'won')
  
  ## get game count column names
  gmCntCols <- gsub('^w', 'n', winCntCols)
  
  ## create win percent column names
  winPcCols <- gsub('^w', 'wPc', winCntCols)
  
  ## for each win count column name  
  for (i in 1:length(winCntCols)) {
    winCntCol <- winCntCols[i]
    gmCntCol <- gmCntCols[i]
    winPcCol <- winPcCols[i]
    df[winPcCol] <- df[winCntCol] / df[gmCntCol]
  }
  
  ## replace NaN w/ NA
  df[is.nan.data.frame(df)] <- NA
  
  ## get new columns created
  newCols <- setdiff(colnames(df), origCols)
  
  ## create win percentage columns for opponent
  df <- fillInOpCols(df, cols=newCols)
  
  ## return
  return(df)
}


## function to add two columns (j and o_j) for propagation juice
addJCols <- function(df, initJ=100, penaltyWt=0.1) {
  
  ## initialize the two columns with NA
  df$j <- df$o_j <- NA
  
  ## populate j with initial value of j 
  df$j[df$n==0] <- initJ
  df$o_j[df$o_n==0] <- initJ
  
  ## apply juice propation by season
  returnDf <- ddply(df, 'season', function(x) {
    
    ## create a list to store team objects
    ## (populated with initialized team objects)
    tmObLst <- vector(mode = "list", length = length(TEAMS))
    names(tmObLst) <- TEAMS
    for (teamName in TEAMS) {
      
      ## create team object
      tmOb <- Team(name=teamName, gmSchDf=x[, c('season', 'date', 'team', 'o_team')])
      
      ## add to list of team objects
      tmObLst[[teamName]] <- tmOb
    }
    
    ## sort by game id so that every adjacent two rows of df provide info
    ## on a single match between given two teams
    x <- sortByCol(x, 'gId')
    
    ## go through each game record (every two rows);
    ## every two rows captures a single matchup game
    for (i in seq(from=1, to=nrow(x), by=2)) {
      
      ## store game info into variables
      date <- x$date[i]
      team <- as.character(x$team[i])
      o_team <- as.character(x$o_team[i])
      won <- x$won[i]
      tmOb <- tmObLst[[team]]
      oTmOb <- tmObLst[[o_team]]
      nxtGmDate <- tmOb$getNextGmDate(date)
      oNxtGmDate <- oTmOb$getNextGmDate(date)
      
      ## calculate updated j after the game (for both team and opponent)
      if (won) {
        ## (team's new j) = (team's current j) + (fraction of opponent team's j)
        newJ <- tmOb$getJ() + oTmOb$getJ() * penaltyWt
        
        ## (opponent's new j) = (opponent's current j) - (fraction of opponent's j)
        oNewJ <- oTmOb$getJ() * (1 - penaltyWt)
      } else {
        ## (team's new j) = (team's current j) - (fraction of team's j)
        newJ <- tmOb$getJ() * (1 - penaltyWt)
        
        ## (opponent's new j) = (opponent's current j) + (fraction of team's j)
        oNewJ <- oTmOb$getJ() + tmOb$getJ() * penaltyWt
      }
      
      ## populate values for j and o_j 
      x$j[x$date==nxtGmDate & x$team==team] <- newJ
      x$j[x$date==oNxtGmDate & x$team==o_team] <- oNewJ
      x$o_j[x$date==nxtGmDate & x$o_team==team] <- newJ
      x$o_j[x$date==oNxtGmDate & x$o_team==o_team] <- oNewJ
      
      ## after updating the df, update j values in team objects
      tmObLst[[team]]$setJ(newJ)
      tmObLst[[o_team]]$setJ(oNewJ)
    }
    
    x
  })
  
  ## return
  return(returnDf)
}


## this function creates df that contains pts columns for each quarter and overtime
createQtrOtPtsDf <- function(df, qtrPtsCol) {
  
  ##  create of list of quarter points
  qtrPtsLst <- strsplit(df[ , qtrPtsCol], split='-')
  
  ## calculate maximum number of combined quarters and overtimes
  max <- max(unlist(lapply(qtrPtsLst, length)))
  
  ## convert vector data type from character to integer;
  ## standardize all vectors' length inside the list by 
  ## appending 0s at the end of the vecotrs that are 
  ## shorter than the maximum length;
  ## append n of overtimes at the end of each vector
  qtrPtsLst <- lapply(qtrPtsLst, function(x) {
    
    ## calculate number of OTs
    nOTs <- length(x) - 4
    
    ## vector of quarter scores and overtime scores
    x <- as.integer(c(x, rep(0, max - length(x))))
    
    ## n of overtimes at the end of vector
    x <- c(x, nOTs)
    
    ## local return
    x
  })
  
  ## collapse list of vectors into a df
  qtrPtsDf <- do.call(rbind.data.frame, qtrPtsLst)
  
  ## name the df
  names(qtrPtsDf) <- c('pQ1', 'pQ2', 'pQ3', 'pQ4', 'pOT1', 'pOT2', 'pOT3', 'pOT4', 'nOT')
  
  ## return
  return(qtrPtsDf)
}


## this function adds pts columns for each quarter and overtime
addQtrOtPtsCols <- function(df, qtrPtsCols) {
  qtrPtsCols <- sort(qtrPtsCols)
  qtrPtsDf <- createQtrOtPtsDf(df, qtrPtsCols[1])
  oQtrPtsDf <- createQtrOtPtsDf(df, qtrPtsCols[2])
  oQtrPtsDf$nOT <- NULL
  names(oQtrPtsDf) <- paste0(names(oQtrPtsDf), 'A')
  df <- cbind(df, qtrPtsDf, oQtrPtsDf)
  return(df)
}