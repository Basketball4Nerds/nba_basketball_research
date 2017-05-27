############ GENERAL/MISCELLANEOUS FUNCTIONS ################


## this function rounds numbers in df to nearest decimal places
## http://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}


## this function capitalizes the first letter in a string
## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


## this function determines if values of elements in df are NaN
## http://stackoverflow.com/questions/18142117/how-to-replace-nan-value-with-zero-in-a-huge-data-frame
is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))  
}


## this function calculates prediction accuracy from confusion matrix
calcAccFrConfMtx <- function(confMtx, rndDgt=3) {
  confMtx <- as.matrix(confMtx)
  accPerc <- sum(diag(confMtx)) / sum(confMtx)
  accPerc <- round(accPerc, rndDgt)
  return(accPerc)
}


## this function grabs the names of numeric variables
getNumericVarNames <- function(df) {
  numericVars <- colnames(df)[sapply(df, is.numeric)]
  return(numericVars)
}


## this function capitalizes words
capitalizeStr <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}


## this function concatenates datasets and exports into a single CSV
## by taking input and output paths as arguments
concatenateDatasets <- function(dirPath, exportPath) {
  
  ## get dataset file names
  rawFileNames <- list.files(dirPath)
  rawCSVFileNames <- rawFileNames[grepl('\\.csv$', rawFileNames)]
  rawCSVFilePaths <- file.path(dirPath, rawCSVFileNames)
  
  ## function to merge multiple datasets into one
  combineCSVs <- function(CSVFilePaths) {
    dataList <- lapply(CSVFilePaths, function(x) {read.csv(file=x, header=T, stringsAsFactors=F)})
    df <- Reduce(function(x, y) {rbind(x, y)}, dataList)
    return(df)
  }
  
  ## load regular and post season datasets
  df <- combineCSVs(rawCSVFilePaths)
  
  ## export
  write.csv(df, exportPath, row.names=FALSE)
}


## this function gets season from a given date
getSeasonFrDate <- function(date) {
  date <- as.Date(date)
  month <- as.integer(format(date, '%m'))
  year <- as.integer(format(date, '%Y'))
  if (month %in% c(10, 11, 12)) 
    season <- year
  else if (month %in% c(1, 2, 3, 4, 5, 6))
    season <- year - 1
  else 
    season <- NULL
  return(season)
}


## this function converts amongst city names, city abbreviations, and team names
convertTeamLabels <- function(df, 
                              from=c('city', 'city_abbr', 'team'), 
                              to=c('city', 'city_abbr', 'team'),
                              team_cols=c('team', 'o_team')) {
  
  from <- from[1]
  to <- to[1]
  
  for (i in 1:nrow(TeamCityConfDf)) {
    city <- TeamCityConfDf$City[i]
    city_abbr <- TeamCityConfDf$CityAbbr[i]
    team <- TeamCityConfDf$Team[i]
    
    for (team_col in team_cols) {
      df[[team_col]] <- gsub(get(from), get(to), df[[team_col]]) 
    }
  }
  
  return(df)
} 


## this function creates odds df by combining 
## expected spreads df and expected total points df
createOddsDf <- function(spreads, totals) {
  
  ## remove payout column from the dfs
  spreads$payout <- totals$payout <- NULL
  
  ## merge dfs
  odds <- merge(spreads, totals, by=c('season', 'date', 'team', 'o_team'), all=TRUE)
  
  ## create projection cols
  odds$pts_proj_om <- (odds$total / 2) - (odds$adjustor / 2)
  odds$pts_alwd_to_o_proj_om <- (odds$total / 2) + (odds$adjustor / 2)
  odds$pts_margin_proj_om <- -odds$adjustor
  odds$win_proj_om <- ifelse(odds$pts_margin_proj_om > 0, TRUE,
                             ifelse(odds$pts_margin_proj_om < 0, FALSE, NA))
  
  ## return
  return(odds)
}


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


## this function creates a "variation df"
## which lists out different combinations
## of team vs. opponent based on 
## site, conference, and off/def rank group;
## it also lists which team metric should be compared which
## opponent metric
createVarDf <- function(by=c('site', 'cnf', 'OG', 'DG'),
                        include.opp.cols=TRUE, 
                        metric=c('w_pc')) {
  
  ## possible combinations are: 
  # - site-cnf
  # - site-OG
  # - site-DG
  ## not allowed combinations are:
  # - cnf-OG
  # - cnf-DG
  # - OG-DG
  
  ## weed out error cases
  by <- unique(by)
  if (!all(by %in% c('site', 'cnf', 'OG', 'DG'))) stop('Incorrect variable given. Try again.')
  if (length(by) > 2) stop('Too many variables given. Please limit to 2.')    
  else if (length(by)==2 && !('site' %in% by)) stop('Incorrect variable combinations provided. With two variable combinations, one must be site.')
  
  ## specify variable options
  site_opts <- c('H', 'A')
  cnf_opts <- c('E', 'W')
  OG_opts <- c('A', 'B', 'C')
  DG_opts <- c('A', 'B', 'C')
  
  ## create varDf by expanding options
  if (length(by)==1) {
    opts <- get(paste0(by, '_opts'))
    varDf <- expand.grid(opts, opts)
    names(varDf) <- c(by, paste0('o_', by))
  }  else if (length(by)==2) {
    opts1 <- get(paste0(by[1], '_opts'))
    opts2 <- get(paste0(by[2], '_opts'))
    varDf <- expand.grid(opts1, opts2, opts1, opts2)
    names(varDf) <- c(by[1], by[2], paste0('o_', by[1]), paste0('o_', by[2]))
  }
  
  ## weed out incorrect cases (e.g. two teams both can't have home games)
  if ('site' %in% names(varDf)) {
    varDf <- varDf[varDf$site != varDf$o_site, ]
  }
  
  ## initialize values 
  wPcCols <- rep('wPc', nrow(varDf))
  o_wPcCols <- rep('o_wPc', nrow(varDf))

  ## append H/A specificity
  if ('site' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, varDf$site)
    o_wPcCols <- paste0(o_wPcCols, varDf$o_site)
  }
  
  ## append vs E/W specificity
  if ('cnf' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, 'Vs', varDf$o_cnf)
    o_wPcCols <- paste0(o_wPcCols, 'Vs', varDf$cnf)
  } 

  ## append vs offense group specificity
  if ('OG' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, 'VsOG', varDf$o_OG)
    o_wPcCols <- paste0(o_wPcCols, 'VsOG', varDf$OG)
  }

  ## append vs defense group specificity  
  if ('DG' %in% names(varDf)) {
    wPcCols <- paste0(wPcCols, 'VsDG', varDf$o_DG)
    o_wPcCols <- paste0(o_wPcCols, 'VsDG', varDf$DG)
  }
  
  ## add win and n game columns
  wCols <- gsub('^wPc', 'w', wPcCols)
  nCols <- gsub('^wPc', 'n', wPcCols)
  o_wCols <- gsub('^o_wPc', 'o_w', o_wPcCols)
  o_nCols <- gsub('^o_wPc', 'o_n', o_wPcCols)
  
  ## incorporate the comparable metrics into the variation df 
  varDf <- cbind(varDf, 
                 wPcCol=wPcCols, o_wPcCol=o_wPcCols, 
                 nCol=nCols, o_nCol=o_nCols, wCol=wCols, o_wCol=o_wCols)

  ## if opponent metrics are not desired
  if (!include.opp.cols) {
    varDf <- varDf[ , !grepl('o_', names(varDf))]
    varDf <- unique(varDf)
  }
  
  ## apply as.character function to each column
  varDf <- sapply(varDf, as.character)

  ## turn back into data frame
  varDf <- as.data.frame(varDf, stringsAsFactors=FALSE)
  
  ## return
  return(varDf)
}


## this function takes in a number of vectors and 
## returns a resultant vector by "majority vote" method
retByMajorityVote <- function(df) {

  ## initialize values to return
  ret <- rep(NA, nrow(df))
  
  ## for each row
  for (i in 1:nrow(df)) {
    
    ## get the number of times a value occurs 
    tbl <- table(t(df[i, ]))
    
    ## get the maximum count (or frequency)
    tbl_max <- max(tbl)
    
    ## if no majority found
    if (tbl_max==1) ret[i] <- NA
    
    ## if majority found, assign majority value as a return value    
    else ret[i] <- names(tbl)[tbl == tbl_max]
  }
  
  ## return
  return(ret)
}





############ DATA API / SCRAPER FUNCTIONS ################

## this function takes in a vector of interested metrics and contructs a API request URL
createSDQL <- function(metrics, date=NULL, season=NULL) {
  
  ## define SDQL query
  SDQL <- paste0(metrics, collapse=',')
  if (is.null(date) & is.null(season)) {
    print('No input received')
    return()
  } else if (!is.null(date)) {
    SDQL <- paste0(SDQL, '@date=%s')
    SDQL <- sprintf(SDQL, format(as.Date(date), '%Y%m%d'))
  } else if (!is.null(season)) {
    SDQL <- paste0(SDQL, '@season=%s')
    SDQL <- sprintf(SDQL, season) 
  } else {
    print('Invalid input')
    return()
  }
  
  return(SDQL)
}


## this function grabs raw NBA games data from Sports Database website via its API
## with a given game date
## http://sportsdatabase.com/nba/query
getRawGamesDataViaApi <- function(date=NULL, season=NULL, def_retry_wait_t=120) {
  
  ## initialize API key
  API_KEY <- 'guest'
  
  ## define metrics to query
  metrics <- c('season', 'date', 'site', 'playoffs',
               
               'team', 'points', 'wins', 'losses', 'rest',
               'steals', 'assists', 'blocks', 'defensive rebounds', 'offensive rebounds', 'rebounds', 'turnovers', 'fouls',
               'field goals attempted', 'field goals made', 'three pointers attempted', 'three pointers made', 
               'free throws attempted', 'free throws made', 'points in the paint', 'fast break points',
               'quarter scores', 'biggest lead', 
               
               'o:team', 'o:points', 'o:wins', 'o:losses', 'o:rest',
               'o:steals', 'o:assists', 'o:blocks', 'o:defensive rebounds', 'o:offensive rebounds', 'o:rebounds', 'o:turnovers', 'o:fouls',
               'o:field goals attempted', 'o:field goals made', 'o:three pointers attempted', 'o:three pointers made', 
               'o:free throws attempted', 'o:free throws made', 'o:points in the paint', 'o:fast break points',
               'o:quarter scores', 'o:biggest lead',
               
               'matchup wins', 'matchup losses', 'lead changes', 
               'margin after the first', 'margin at the half', 'margin after the third',
               'total', 'line')
  
  ## create SDQL
  SDQL <- createSDQL(metrics, date=date, season=season)
  
  ## create api request url                                                                                                             
  reqUrl <- paste0('http://api.sportsdatabase.com/nba/query.json?sdql=', URLencode(SDQL), '&output=json&api_key=', API_KEY)
  
  ## clean output JSON-like string
  webpage <- getURL(reqUrl)
  webpage <- gsub('\t', '', webpage)
  webpage <- gsub('^json_callback\\(', '', webpage)
  webpage <- gsub('\\);\n$', '', webpage)
  webpage <- gsub("'", '"', webpage)
  #webpage <- gsub('^ ', '', webpage)
  
  ## after getting webpage, if there is 503 error, wait and try again later
  err_msg_503 <- '503 Service Temporarily Unavailable'
  if (grepl(err_msg_503, webpage)) {
    print('Encountered 503 error. Trying again after wait...')
    Sys.sleep(def_retry_wait_t)
    return(getRawGamesDataViaApi(date=date, season=season, def_retry_wait_t=def_retry_wait_t*2))
  } 
  
  ## proceed to convert the returned webpage data into JSON if there is no 503 error
  else {
    
    ## load JSON data
    json <- fromJSON(json_str=webpage)

    ## if there is 599 timeout error indicated in the returned JSON data, then wait and try again later
    if ('html' %in% names(json)) {
      if (json$html=='HTTP 599: Timeout') {
        print('Encountered 599 error. Trying again after wait...')
        Sys.sleep(def_retry_wait_t)
        return(getRawGamesDataViaApi(date=date, season=season, def_retry_wait_t=def_retry_wait_t*2))
      }
    }

    ## modify column names  
    colNms <- metrics
    colNms <- gsub('(^ )|( $)', '', colNms)
    colNms <- gsub(' ', '_', colNms)
    colNms <- gsub('\\:', '_', colNms)
    colNms <- gsub('points', 'pts', colNms)
    colNms <- gsub('_the_', '_', colNms)
    #colNms[grepl('o_pts', colNms)] <- 'pts_alwd_to_o'
    
    ## return an empty data frame if no data  
    if (length(json)==0) {
      df <- as.data.frame(matrix(ncol=length(colNms), nrow=0))
      names(df) <- colNms
      return(df)
    }
    
    ## initialize df
    ncols <- length(colNms)  # length of the header columns
    nrows <- length(json$groups[[1]]$columns[[1]])  # length of the first column rows
    gmsRawDf <- as.data.frame(matrix(NA, nrow=nrows, ncol=ncols))
    names(gmsRawDf) <- colNms
    
    ## find cols indices of gmsRawDf that will contain quarter scores 
    qtrPtsColIndices <- which(grepl('quarter_scores', names(gmsRawDf)))
    
    ## populate df
    for (i in 1:ncols) {

      ## for quarter-points columns, column values must be conditioned to string of numbers delineated by "-"
      if (i %in% qtrPtsColIndices) {
        colValsLofL <- json$groups[[1]]$columns[[i]]
        colVals <- lapply(colValsLofL, function(x) {paste(x, collapse='-')})
        colVals <- unlist(colVals)
      } else {
        colVals <- json$groups[[1]]$columns[[i]]
        
        ## unlist; occasionally, the column values are not laid out as a vector but as a list of values
        if (class(colVals)=='list') {
          colVals <- lapply(colVals, function(x) { ifelse(is.null(x), NA, x) })
          colVals <- unlist(colVals)
        }
      }
      
      ## at the time of writing of this code, 
      ## the data feed for the lead changes column comes back as a vector of unmeaningful strings
      ## notated as "lead changes";
      ## that is an error of the API function;
      ## modify this part of the code when the API function returns proper values
      
      gmsRawDf[i] <- colVals
    }
    
    ## cast proper data format and type for date 
    gmsRawDf$date <- as.Date(as.character(gmsRawDf$date), format='%Y%m%d')
    
    ## NA for unknown future data
    #gmsRawDf$wins[is.na(gmsRawDf$pts)] <- NA
    #gmsRawDf$losses[is.na(gmsRawDf$pts)] <- NA
    #gmsRawDf$o_wins[is.na(gmsRawDf$pts)] <- NA
    #gmsRawDf$o_losses[is.na(gmsRawDf$pts)] <- NA
    
    ## return
    return(gmsRawDf)
  } 
  
}


## this function scrapes odds and lines data from Sportsbook Review website 
## for a given date and either return a df or writes to csv
#date <- '2006-05-13'  # example of no data (no game that day)
#date <- '2006-12-13'  # example of missing data
#date <- '2015-12-11'  # example of complete data
scrapeDataFrSportsbookReviewByDate <- function(date, relTeams, 
                                               type=c('point-spread', 'total-points')) {
  
  ## determine which kind of data to scrape 
  type <- type[1]
  
  ## get date and season
  date <- as.Date(date)
  season <- getSeasonFrDate(date)
  
  ## construct request URL and load webpage
  baseUrl <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/'
  if (type=='total-points')
    baseUrl <- paste0(baseUrl, 'totals/')
  reqUrl <- paste0(baseUrl, '?date=', strftime(date, '%Y%m%d'))
  webpage <- html(reqUrl)
  
  ## get a list of game info for a given date
  gmsInfo <- webpage %>% 
    html_nodes('div.event-holder div.eventLine')
  if (length(gmsInfo)==0) return()
  
  ## initialize empty columns
  tmCol <- oTmCol <- payoutCol <- projCol <- c()  
  
  ## loop through a list of games for that day
  for (i in 1:length(gmsInfo)) {
    
    ## get single game info
    gmInfo <- gmsInfo[[i]]
    
    ## get teams info
    tmsInfo <- gmInfo %>%
      html_nodes('a') %>%
      html_text()
    tm1 <- tmsInfo[2]
    tm2 <- tmsInfo[3]
    
    ## get data from Pinnacle source
    boxInfo <- gmInfo %>%
      html_node('div.el-div.eventLine-book') %>%
      html_nodes('div.eventLine-book-value > b') %>%
      html_text()
    boxInfo <- gsub('\\xa0', ' ', boxInfo)
    boxInfo <- gsub('½', '.5', boxInfo)
    
    if ('' %in% boxInfo) {
      projTm1 <- payoutTm1 <- ''
      projTm2 <- payoutTm2 <- ''
    } else {
      boxInfoTm1 <- strsplit(boxInfo[[1]], ' ')[[1]]
      projTm1 <- boxInfoTm1[[1]]
      payoutTm1 <- boxInfoTm1[[2]]
      
      boxInfoTm2 <- strsplit(boxInfo[[2]], ' ')[[1]]
      projTm2 <- boxInfoTm2[[1]]
      payoutTm2 <- boxInfoTm2[[2]]
    }
    
    ## append to vectors (which will be used as columns)
    tmCol <- c(tmCol, tm1, tm2)
    oTmCol <- c(oTmCol, tm2, tm1)
    payoutCol <- c(payoutCol, payoutTm1, payoutTm2)
    projCol <- c(projCol, projTm1, projTm2)
  }
  
  ## construct a df
  df <- data.frame(season=season, date=date, team=tmCol, o_team=oTmCol, payout=payoutCol, proj_col=projCol)
  
  if (type=='point-spread') {
    ## name projection column 
    name <- 'adjustor'
    names(df)[ncol(df)] <- name
    
    ## replace "PK" with 0s for the line adjustor metric
    # http://www.sportsbookreview.com/betting-odds/nba-basketball/?date=20110411
    df[[name]] <- gsub('PK', '0', df[[name]])
  } 
  
  else if (type=='total-points') {
    ## name projection column 
    name <- 'total_pts_proj_om'
    names(df)[ncol(df)] <- name
  }
  
  ## cast proper data types
  df$season <- as.integer(as.character(df$season))
  df$team <- as.character(df$team)
  df$o_team <- as.character(df$o_team)
  df$payout <- as.numeric(as.character(df$payout))
  df[[name]] <- as.numeric(as.character(df[[name]]))
  
  ## replace city names with team names
  #df <- replaceCityNmsToTeamNms(df)
  df <- convertTeamLabels(df, from='city', to='team')
  
  ## select valid data by removing rows for all star games
  df <- subset(df, team %in% relTeams & o_team %in% relTeams)
  
  ## return 
  return(df)
}


## this function takes in df and adds latest up-to-date data 
addLatestData <- function(df, req_wait_t=1, func, non_date_args) {
  
  ## set dates over which to download data 
  startDate <- as.Date(max(df$date))
  endDate <- Sys.Date()
  dates <- seq(startDate, endDate, by=1)
  
  ## flush the last day's data from the old data
  df <- subset(df, date != startDate)
  
  ## download latest daily data 
  newDataDfLst <- lapply(dates, function(date) {
    
    ## wait before each data feed request    
    Sys.sleep(req_wait_t)
    
    ## print date of data downloading
    print(date)
    
    ## construct a list of arguments
    args <- non_date_args
    args$date <- date
    
    ## execute data grab function
    do.call(what=func, args=args)
  })
  
  ## merge onto (append to) original df
  newDataDf <- do.call(rbind.data.frame, newDataDfLst)
  df <- rbind.data.frame(df, newDataDf)
  
  ## return
  return(df)
}





############ MOVING AVERAGE FUNCTIONS ################

## this function calculates moving averages
calcMovAvgVals <- function(vals, type=c('SMA', 'EMA', 'cummean'), n=NULL, coverLessThanN=TRUE) {

  ## specify moving average type
  type <- type[1]
  
  ## make sure there is no NAs in the vals
  ## (moving average function will break)
  if (any(is.na(vals))) 
    stop('There is NA in numeric values provided. Cannot compute moving average.')
  
  ## get length of given vector
  len <- length(vals)
  
  ## if length of vector is less than 1
  if (len==0) stop('Please provide vector of length greater than 0.')
  
  ## if length of vector is equal to 1, 
  ## then there is nothing to overage over; return NA
  if (len==1) return(NA)
  
  ## return cummulative mean if cummean method selected
  if (type=='cummean') { return(cummean(vals)) }

  ## the rest of the function deals with for SMA and EMA
  
  ## regular moving average;
  ## doesn't provide moving average values for indices less than n;
  ## e.g. if n=5, no moving average values from 1st through 4th
  if (!coverLessThanN) {

    ## if there are less numbers in the vector than n
    ## to average over, then moving average is a vector of NAs
    if (len < n) { runVals <- rep(NA, len) } 
    
    ## calculate MAs under normal condition
    else { runVals <- do.call(type, args=list(x=vals, n=n)) }
  }

  ## modified moving average;
  ## if n is larger than length of vectors to cover,
  ## then n adjusts to "fit" the vector and cover;
  ## e.g. if n=5, then:
  ## 2nd MA val will be from 1st and 2nd vals,
  ## 3rd MA val will be from 1st, 2nd, 3rd vals,
  ## 4th MA val will be from 1st, 2nd, 3rd, 4th vals,
  ## 5th MA val will be from 1st, 2nd, 3rd, 4th, 5th vals,
  ## 6th MA val will be from 2nd, 3rd, 4th, 5th, 6th vals
  else {
    
    ## if there are less numbers in the vector than n to average over,
    ## then shorten n to fit the length of the vector
    if (len <= n) { n <- len }

    ## first calculate MA normally
    runVals <- do.call(type, args=list(x=vals, n=n))
    
    ## then go back and fill in i < n entries
    runVals[1] <- vals[1]
    for (i in 2:(n-1)) {
      args <- list(x=vals[1:i], n=i)
      runVal <- do.call(type, args=args)[i]
      runVals[i] <- runVal
    }
  }

  ## return  
  return(runVals)
}


## this function adds new columns for moving averages
## (e.g. simple moving averages or running standard deviations)
addMaCols <- function(df, cols, type=c('SMA', 'EMA', 'cummean'), n=10, 
                      coverLessThanN=TRUE, aggVars=NULL, colApndStr='', rndDgt=3) {
  
  ## set type
  type <- type[1]
  
  ## base case
  if (is.null(aggVars)) {
    
    ## order the base subsets
    df <- df[order(df$date), ]

    ## for each column
    for (col in cols) {
      
      ## for cummulative mean (up-to-date moving average)
      if (type=='cummean') {

        ## calculate MA
        runVals <- calcMovAvgVals(vals=df[[col]], type=type)
        
        ## offset (shift) MA vals by 1
        runVals <- c(NA, runVals[-length(runVals)])
        
        ## round MA values
        runVals <- round(runVals, rndDgt)
        
        ## create new column name
        runValsColNm <- paste0(col, '_', tolower(type), colApndStr)
        
        ## add MA vals to df as column
        df[[runValsColNm]] <- runVals
      }
      
      ## for SMA and EMA
      else {

        ## in case n is a vector of integers
        for (i in n) {
          
          ## calculate MA
          runVals <- calcMovAvgVals(vals=df[[col]], n=i, coverLessThanN=coverLessThanN)
          
          ## offset (shift) MA vals by 1
          runVals <- c(NA, runVals[-length(runVals)])
          
          ## round MA values
          runVals <- round(runVals, rndDgt)
          
          ## create new column name
          runValsColNm <- paste0(col, '_', tolower(type), i, colApndStr)
          
          ## add MA vals to df as column
          df[[runValsColNm]] <- runVals
        }
      }
    }

    ## return
    return(df)
  }
  
  df <- sortByCol(df, aggVars, asc=TRUE)
  
  ## for each aggregation subset, apply ad
  outputDF <- ddply(df, aggVars, function(x) {
    addMaCols(df=x, cols=cols, type=type, n=n, coverLessThanN=coverLessThanN, 
              aggVars=NULL, colApndStr=colApndStr, rndDgt=rndDgt)
  })
  
  ## return
  return(outputDF)
}





############ OFFENSE/DEFENSE RANK FUNCTIONS ################

## this function takes in df of game logs and returns 
## a matrix of PPP (pts per pos); element A(i, j) represents 
## PPP of ith team agaist jth team in the matrix
createPppMatrix <- function(df, rnd_to_digits=3, rm_NA_rows=TRUE, rm_NA_cols=TRUE) {
  
  ## initialize PPP list 
  ## (will contain a list of vectors of PPP for each team-to-opponent combo)
  pppLst <- vector(mode='list', length=length(TEAMS) * length(TEAMS) - length(TEAMS))
  
  pppLstNms <- c()
  for (team1 in TEAMS) {
    for (team2 in TEAMS) {
      if (team1 != team2) {
        pppLstNms <- c(pppLstNms, paste0(team1, '_', team2))
      }
    }
  }
  names(pppLst) <- pppLstNms
  
  ## initialize PPP matrix
  M <- matrix(NA, nrow=length(TEAMS), ncol=length(TEAMS))
  colnames(M) <- rownames(M) <- TEAMS
  
  ## iterate through game logs data  
  for (k in 1:nrow(df)) {
    
    ## store info into variables for convenience
    team <- as.character(df$team[k])
    o_team <- as.character(df$o_team[k])
    ppp <- df$PPP[k]
    pppLstNm <- paste0(team, '_', o_team)
    i <- which(team==TEAMS)
    j <- which(o_team==TEAMS)
    
    ## add ppp to the record-tracking list
    pppLst[[pppLstNm]] <- c(pppLst[[pppLstNm]], ppp)
    
    ## populate matrix
    if (is.na(M[i, j])) {
      M[i, j] <- ppp
    } else {
      M[i, j] <- mean(pppLst[[pppLstNm]])
    }
  }
  
  ## round decimal places
  M <- round(M, rnd_to_digits)
  
  ## remove all-NA rows
  if (rm_NA_rows) {
    ind <- apply(M, 1, function(row) all(is.na(row)))
    M <- M[!ind, ]
  }
  
  ## remove all-NA cols
  ind <- apply(M, 2, function(col) all(is.na(col)))
  M <- M[ , !ind]
  
  ## return matrix
  return(M)
}


## this function takes in a PPP matrix and returns 
## defense rank matrix; each team listed row-wise has its
## own def ranking of its opponents listed column-wise
createDefRnkMatrix <- function(pppM) {
  
  ## calculate dim
  m <- nrow(pppM)
  n <- ncol(pppM)
  
  ## initialize def rank matrix  
  dRnkM <- matrix(NA, m, n)
  colnames(dRnkM) <- rownames(dRnkM) <- colnames(pppM)
  
  ## for each row (for each team against its opponents) 
  for (i in 1:m) {
    
    ## calculate def rank of opponents
    dRnk <- match(colnames(pppM), names(sort(pppM[i, ])))
    
    ## populate def rank matrix
    dRnkM[i, ] <- dRnk
  }
  
  ## return
  return(dRnkM)
}


## this function takes in a complete PPP matrix and returns 
## offense rank matrix; each team listed row-wise has its
## own off ranking of its opponents listed column-wise
createOffRnkMatrix <- function(pppM) {
  
  ## calculate dim
  m <- nrow(pppM)
  n <- ncol(pppM)
  
  ## initialize off rank matrix  
  oRnkM <- matrix(NA, m, n)
  colnames(oRnkM) <- rownames(oRnkM) <- colnames(pppM)
  
  ## transpose ppp matrix
  M <- t(pppM)
  
  ## for each row (for each team against its opponents) 
  for (i in 1:m) {
    
    ## calculate off rank of opponents
    oRnk <- match(colnames(M), names(sort(M[i, ], decreasing=TRUE)))
    
    ## populate off rank matrix
    oRnkM[i, ] <- oRnk
  }
  
  ## return
  return(oRnkM)
}


## this function takes in rank matrix (dRnkM or oRnkM)
## that contains team's ranking of its opponent listed column-wise
## (whether in offense or defense) and replaces the
## numerical rankings with categorical levels (e.g. A, B, C),
## with "A" being the highest mark (strongest offensive/defensive);
## create "graded rank matrix";
createGradedRnkM <- function(rnkM, k) {
  
  ## initialize a new graded rank matrix by copying original rank matrix and replacing all values to NA
  gradedRnkM <- rnkM
  gradedRnkM[TRUE] <- NA
  
  ## find the highest numeric rank (assigned to the opponent with the worst performance)
  maxRnk <- nrow(rnkM) - 1
  
  ## create a quantile (intervals of ranks) to assign the grades
  qtl <- quantile(1:maxRnk, probs=seq(0, 1, 1/k))
  
  ## populate the graded rank matrix based on the numeric values of rank matrix
  for (i in 1:(length(qtl)-1)) {
    gradedRnkM[rnkM >= qtl[i] & rnkM <= qtl[i+1]] <- LETTERS[i]
  }
  
  ## return
  return(gradedRnkM)
}


## this function takes a graded rank matrix (where opponents
## are graded by all teams) and returns a named vector of 
## performance grades, collectively agreed upon by all teams
returnCollectiveGrading <- function(gradedRnkM, pValThres) {
  
  ## initialize a vector to store canonical grading
  grades <- c()
  
  ## calculate number (k) of grade classes 
  ## (e.g. 3 if A-B-C grading system; 4 if A-B-C-D grading system)
  uniqGrades <- unique(c(gradedRnkM))
  k <- length(uniqGrades[!is.na(uniqGrades)])
  
  ## calculate dist perc when votes are equally distributed 
  thres <- 1 / k
  
  for (i in 1:ncol(gradedRnkM)) {
    
    ## calculate number of "votes" on grades;
    ## a.k.a. grade distribution (raw frequency)
    distr <- table(gradedRnkM[ , i])
    
    ## calculate percentage of "votes" on grades;
    ## grade distribution (percentage)
    pcDistr <- round(prop.table(distr), 3)
    
    ## perform chi-square test to determine significance
    pVal <- chisq.test(distr)$p.value
    
    ## if p-value is less than a certain threshold
    ## (if distribution anomaly is statistically significant)
    if (pVal <= pValThres) {
      
      ## if there is a majority of votes (more than 50%), then assign to the grade;
      ## e.g. Spurs def in 2015 (24 As, 3 Bs, 2 Cs) would canonically be graded as A 
      ## because more than 50% of the opponents graded Spurs' def as A
      if (any(pcDistr > 0.5)) {
        grade <- names(pcDistr[pcDistr > 0.5])
      }
      
      ## if there isn't majority votes but one or more grades received larger-than-normal share(s)
      else if (any(pcDistr > thres)) {
        
        ## if only one of the grades received outstanding (but less than majority) shares of votes
        if (sum(pcDistr > thres)==1) {
          grade <- names(pcDistr[pcDistr > thres])
        }
        
        ## if two or more of the grades received outstanding (but less than majority) shares of votes
        else if (sum(pcDistr > thres) > 1) {
          
          ## concatenate the grades (e.g. AB, BC, or AC)
          grade <- paste0(names(pcDistr[pcDistr > thres]), collapse='')
          
          ## if the two grades are not adjacent
          if (grade=='AC') grade <- 'X'
        }
      }
    } 
    
    ## if p-value doesn't indicate statistical significance
    else {
      grade <- 'X'
    }
    
    ## appends to grades vector
    grades <- c(grades, grade)
  }
  
  ## name the grades vector
  names(grades) <- colnames(gradedRnkM)
  
  ## return
  return(grades)
}


## this function adds letter grade column based on 
## performance of a certain given metric; grades can be
## A through C, where A denotes the best performance
addABCGradeCol <- function(df, metrics, higherNumBetterPerf, 
                           eff=TRUE, minN=10, method=c('qntl', 'sd')) {

  ## set method
  method <- method[1]
    
  ## stop if wrong higherNumBetterPerf input was given
  if (length(higherNumBetterPerf) != 1) {
    if (length(metrics) != length(higherNumBetterPerf)) {
      stop('Wrong input given; check higherNumBetterPerf input, please...')
    }
  }
  
  ## sort df by date
  df <- sortByCol(df, col='date')

  ## get length of metrics to iterate over
  k <- length(metrics)

  ## for each metric
  for (i in 1:k) {

    ## specify metric
    metric <- metrics[i]
    
    ## specify corresponding higherNumBetterPerf conditional for that metric
    higherNumBttrPerf <- ifelse(length(higherNumBetterPerf)==1,
                                higherNumBetterPerf,
                                higherNumBetterPerf[i])
      

    ## create new column name
    newColNm <- paste0('g', simpleCap(metric))
    newColNm <- gsub('_(sma|ema|cummean).*', '', newColNm)

    ## initialize an empty grades vector 
    ## (this vector will be added as a new column to df)
    grades <- c()
    
    ## employ efficient grading 
    ## (less accurate b/c it disregards the current season's data
    ## and uses previous years' data to assign grading) 
    if (eff) {
      
      ## get a vector of all seasons found in df
      seasons <- unique(df$season)
      
      ## NAs for the first season's grades
      grades <- c(grades, rep(NA, sum(df$season==seasons[1])))
      
      ## for each season except the first season 
      ## (skip first season b/c prior data is unavailable)
      for (season in seasons[-1]) {
        
        ## grab a vector of perfs for the season
        perfs <- df[df$season==season, metric]
        
        ## if standard deviation method selected
        if (method=='sd') {

          ## use previous seasons to calculate mean and sd
          prevSnsMean <- mean(df[df$season < season & df$n >= minN, metric], na.rm=TRUE)
          prevSnsSd <- sd(df[df$season < season & df$n >= minN, metric], na.rm=TRUE)
          
          ## if higher numeric value signifies better performance
          if (higherNumBttrPerf) {
            snGrades <- ifelse(perfs >= (prevSnsMean + prevSnsSd), 'A', 
                               ifelse(perfs <= (prevSnsMean - prevSnsSd), 'C', 'B'))
          }
          
          ## if higher numeric value signifies lower performance
          else {
            snGrades <- ifelse(perfs >= (prevSnsMean + prevSnsSd), 'C',
                               ifelse(perfs <= (prevSnsMean - prevSnsSd), 'A', 'B'))
          }
        }
        
        ## if quantile method selected
        else if (method=='qntl') {

          ## use previous seasons to calculate quantiles
          prevQntls <- quantile(df[df$season < season & df$n >= minN, metric], 
                                probs=seq(0, 1, 1/3), na.rm=TRUE)
          
          ## if higher numeric value signifies better performance
          if (higherNumBttrPerf) {
            snGrades <- ifelse(perfs >= prevQntls[3], 'A', 
                               ifelse(perfs <= prevQntls[2], 'C', 'B'))
          }
          
          ## if higher numeric value signifies lower performance
          else {
            snGrades <- ifelse(perfs >= prevQntls[3], 'C',
                               ifelse(perfs <= prevQntls[2], 'A', 'B'))
          }
          
        } 
        
        ## if invalid method selected
        else {
          stop('Please select the right grading method.')
        }

        ## append single season's grades to the grades vector
        grades <- c(grades, snGrades)
      }
    }
    
    ## employ accurate grading
    ## (more accurate b/c it utilizes most up-to-date data to 
    ## calculate mean and sd, but also more computationally intensive
    ## b/c it computes mean and sd for every new date encountered in df)
    else {
      
      ## get a vector of all dates found in df
      dates <- unique(df$date)
      sum(df$date==dates[1])
      
      ## NAs for the first date's grades
      grades <- c(grades, rep(NA, sum(df$date==dates[1])))
      
      ## for every date except the very first date 
      ## (because first date has no prior data)
      for (date in dates[-1]) {

        ## grab a vector of perfs for the date
        perfs <- df[df$date==date, metric]
        
        ## if standard deviation method selected
        if (method=='sd') {
          
          ## use previous dates to calculate mean and sd
          prevDatesMean <- mean(df[df$date < date & df$n >= minN, metric], na.rm=TRUE)
          prevDatesSd <- sd(df[df$date < date & df$n >= minN, metric], na.rm=TRUE)
          
          ## if higher numeric value signifies better performance
          if (higherNumBttrPerf) {
            dateGrades <- ifelse(perfs >= (prevDatesMean + prevDatesSd), 'A', 
                                 ifelse(perfs <= (prevDatesMean - prevDatesSd), 'C', 'B'))
          }
          
          ## if higher numeric value signifies lower performance
          else {
            dateGrades <- ifelse(perfs >= (prevDatesMean + prevDatesSd), 'C',
                                 ifelse(perfs <= (prevDatesMean - prevDatesSd), 'A', 'B'))
          } 
        }
        
        ## if quantile method selected
        else if (method=='qntl') {
          
          ## use previous dates to calculate quantiles
          prevQntls <- quantile(df[df$date < date & df$n >= minN, metric], 
                                probs=seq(0, 1, 1/3), na.rm=TRUE)

          ## if higher numeric value signifies better performance
          if (higherNumBttrPerf) {
            dateGrades <- ifelse(perfs >= prevQntls[3], 'A', 
                                 ifelse(perfs <= prevQntls[2], 'C', 'B'))
          }
          
          ## if higher numeric value signifies lower performance
          else {
            dateGrades <- ifelse(perfs >= prevQntls[3], 'C',
                                 ifelse(perfs <= prevQntls[2], 'A', 'B'))
          } 
        }

        ## if invalid method selected
        else {
          stop('Please select the right grading method.')
        }
        
        ## append single season's grades to the grades vector
        grades <- c(grades, dateGrades)
      }
    }
    
    ## designate "U" for unknowns
    grades[is.na(grades)] <- 'U'
    
    ## add grades vector as a column to original df
    df[newColNm] <- grades
  }

  ## return
  return(df)
}






############ OPTIMAL WEIGHTS FUNCTIONS ################

## this function calculates an average prediction error
calcAvgPredErr <- function(df, outcomeVar, projVar, wgts=NULL) {
  
  if (is.null(wgts)) {
    ret <- mean(abs(df[[outcomeVar]] - df[[projVar]]), na.rm=TRUE)    
  }

  else {
    wgtsMtrx <- matrix(wgts, ncol=1)
    indVarsMtrx <- as.matrix(df[ , indVars])
    proj <- as.numeric(indVarsMtrx %*% wgtsMtrx)
    err <- df[[outcomeVar]] - proj
    ret <- mean(abs(err), na.rm=TRUE)
  }
  
  return(ret)
}






############ FUNCTIONS TO PREDICT AND CALCULATE ACCURACY PERCENTAGES ################

## this function creates df of simple retrospective win prediction strengths (RWPS)
## by given metrics
createSimpleRetroWinPredAccDf <- function(df, cols) {
  
  ## initialize an empty list
  lst <- list()
  
  ## calculate retrospective win prediction strength (RWPS) for each metric
  for (col in cols) {
    
    ## construct o_col (opponent's metric) based on a given col
    if (grepl('Fcd', col)) {
      o_col <- gsub('Fcd', '', col)      
    } 
    else if (grepl('[A-Za-z]_p_[A-Za-z]', col)) {
      o_col <- unlist(strsplit(col, split='_p_'))
      o_col <- paste0(o_col, 'A')
      o_col <- paste0(o_col, collapse='_p_')
    } 
    else {
      o_col <- paste0(col, 'A')      
    }
    
    ## if o_col is not found in dataset, skip to the next metric
    if (!(o_col %in% names(df))) {
      print(paste('Unable to locate the following metric:', col))
      next
    }
    
    ## make a simple retrospective prediction 
    pred <- df[ , col] > df[ , o_col]
    
    ## create confusion matrix
    cnfMtx <- table(df$won, pred)
    
    ## calculate prediction accuracy
    acc <- calcAccFrConfMtx(cnfMtx)
    
    ## calculate the number of data points to calculate retro pred acc
    nDp <- sum(cnfMtx)
    
    ## create a list element
    lstElm <- c(col, acc, nDp)
    
    ## append list element to list
    lst <- c(lst, list(lstElm))
    
  }
  
  ## collapse list into df
  retDf <- do.call(rbind.data.frame, lst)
  
  ## set colnames for df
  names(retDf) <- c('metric', 'SRWPS', 'nDp')
  
  ## set proper data type 
  retDf$SRWPS <- as.numeric(as.character(retDf$SRWPS))
  retDf$nDp <- as.integer(as.character(retDf$nDp))
  
  ## return
  return(retDf)
}


## this function returns a vector of logical (an index) 
## by given conditions (var_df_row and n_min)
createIndex <- function(master_df, var_df_row, n_min) {
  
  ## grab variable columns
  by_cols <- grep('^(o_)?site$|^(o_)?cnf$|^(o_)?OG$|^(o_)?DG$', names(var_df), value=TRUE)
  
  ## initialize list of indices      
  ind_lst <- list()
  
  ## populate list of indices for each variable
  for (by_col in by_cols) {
    by_val <- var_df_row[[by_col]]
    ind <- master_df[[by_col]]==by_val
    ind_lst <- c(ind_lst, list(ind))
  }
  
  ## add to list of indices requirement for n-game minimum threshold
  min_n_col <- var_df_row$nCol
  o_min_n_col <- var_df_row$o_nCol
  ind <- master_df[[min_n_col]] >= n_min & master_df[[o_min_n_col]] >= n_min
  ind_lst <- c(ind_lst, list(ind))
  
  ## reduce list of index conditions with "and" operator
  ind <- Reduce('&', ind_lst)
  
  ## return
  return(ind)
}


## this function takes master_df, var_df, by, and n_min
## and output a vector of win predictions
createWinPred <- function(master_df, metric, by=NULL, n_min=5) {

  ## stop if there is error in input
  if (metric %in% c('site', 'line', 'mtch_mrgn', 'j', 'rst')) 
    if (!is.null(by)) stop('Given metric cannot be varied by the "by" variable.')

  ## if 'by' variable is not specified and not applicable,
  ## make simple predictions without 'by' variable 
  if (is.null(by)) {

    ## predict that home team will win
    if (metric=='site') {
      pred <- master_df$site=='H'
    }
    
    ## predict that favored team will win
    else if (metric=='line') {
      pred <- ifelse(master_df$line < 0, TRUE, 
                     ifelse(master_df$line > 0, FALSE, NA))
    }
    
    ## predict that whichever team who has won more games 
    ## against the other team will win
    else if (metric==c('mtch_mrgn')) {
      pred <- ifelse(master_df$mtch_mrgn > 0, TRUE,
                     ifelse(master_df$mtch_mrgn < 0, FALSE, NA))
    } 
    
    ## predict that whichever team with higher metric will win
    else if (metric %in% c('j', 'rst', 'wPc')) {
      
      ## create opponent metric
      o_metric <- paste0('o_', metric)
      
      ## predict that whichever team that had more rest will win the game
      pred <- ifelse(master_df[[metric]] > master_df[[o_metric]], TRUE,
                     ifelse(master_df[[metric]] < master_df[[o_metric]], FALSE, NA))
    } 
    
    else {
      stop('Metric not found in the master dataset.')
    }

    ## filter out predictions by using min n-game threshold
    pred[master_df$n < n_min | master_df$o_n < n_min] <- NA
  }

  
  ## if 'by' variable is specified and applicable,
  ## make variable-specific (e.g. cnf-specific) predictions
  else {
    
    ## create var_df to obtain rows of variations
    var_df <- createVarDf(by=by)
    
    ## for each variation listed in var_df
    for (i in 1:nrow(var_df)) {
      var_df_row <- var_df[i, ]
      
      ## select team and opponent metrics
      if (metric=='wPc') {
        metric_col <- var_df_row$wPcCol
        o_metric_col <- var_df_row$o_wPcCol
      } else if (metric=='sma10') {
        next  # skip for not; come back later and modify this part
      }
      
      ## create index of metric comparisons to make
      ind <- createIndex(master_df, var_df_row, n_min)
      
      ## make predictions for the applicable index
      pred[ind] <- ifelse(master_df[[metric_col]] > master_df[[o_metric_col]], TRUE,
                          ifelse(master_df[[metric_col]] < master_df[[o_metric_col]], FALSE, NA))[ind]
    }
  }    

  ## return
  return(pred)
}


## 



## this function returns df of win prediction accuracies
## when predicting wins by various win percentage metrics
createWinPredAccDfByWinPcMetrics <- function(master_df, n_min=c(5, 10)) {
  
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
  
  ## initialize df to populate
  ret_df <- as.data.frame(matrix(NA, nrow=length(by_lst)*length(n_min), ncol=4))
  names(ret_df) <- c('by', 'n_min', 'acc', 'n_pred')
  
  ## for each threshold in n-game min threshold vector
  for (i in seq_along(n_min)) {
    
    ## get threshold
    thres <- n_min[i]
    
    ## for each by specification
    for (j in seq_along(by_lst)) {
      by <- by_lst[[j]]
      var_df <- var_df_lst[[j]]
      
      ## create prediction
      pred <- createWinPred(master_df=master_df, var_df=var_df, n_min=thres)
      
      ## create confusion matrix
      cnfMtx <- table(pred, master_df$won)
      #cnfMtx <- table(c(T, T), c(T, F))
      
      ## calculate prediction accuracy
      acc <- calcAccFrConfMtx(cnfMtx, rndDgt=3)
      
      ## add results to the ret_df
      k <- (i-1) * length(by_lst) + j
      ret_df$by[k] <- paste0(by, collapse='_')
      ret_df$n_min[k] <- thres
      ret_df$acc[k] <- acc
      ret_df$n_pred[k] <- sum(cnfMtx)
    } 
  }
  
  ## return
  return(ret_df)
}


