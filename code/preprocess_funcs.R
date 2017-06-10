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
  createVarDf(by=x)
})

type = c('w', 'n')



names(master)
dRb_H_E_OGA


addRunMsrCols <- function(df, type=c('count', 'sum')) {
  
}

addRunCntCols <- function(df, type=) {
  
  ## original cols
  orig_cols <- colnames(df)

  ## sort by date
  df <- sortByCol(df, col='date')
  
  ## running tallies should contain only per-season-per-team counts
  df <- ddply(df, c('season', 'team'), function(x) {
    n <- nrow(x)

    ## for each "by" variation and combination
    for (i in 1:length(by_lst)) {
      
      ## select by and var_df
      by <- by_lst[[i]]
      var_df <- var_df_lst[[i]]
by
var_df
by

``

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



# ## this function adds running count columns of wins/losses 
# ## by home/away, opponent def/off rank, opponent conf,
# ## and combinations of those
# addRunWinLossGmCntCols <- function(df) {
#   
#   ## original cols
#   origCols <- colnames(df)
#   
#   ## create variable-by list
#   by_lst <- list('site', 
#                  'cnf', 
#                  'OG', 
#                  'DG', 
#                  c('site', 'cnf'), 
#                  c('site', 'OG'), 
#                  c('site', 'DG'))
#   
#   ## create variable-by df list
#   var_df_lst <- lapply(by_lst, function(x) {
#     createVarDf(by=x, type='tm-opp')
#   })
#   
#   ## running tallies should contain only per-season-per-team counts
#   df <- ddply(df, c('season', 'team'), function(x) {
#     n <- nrow(x)
#     x <- sortByCol(x, col='date')
#     
#     ## for each "by" variation and combination
#     for (i in 1:length(by_lst)) {
#       
#       ## select by and var_df
#       by <- by_lst[[i]]
#       var_df <- var_df_lst[[i]]
#       
#       ## if single-variable (e.g. by H/A or by E/W, etc.)
#       if (length(by)==1) {
#         
#         ## create column selector
#         selector <- ifelse(by=='site', by, paste0('o_', by))
#         
#         ## add running win count column and n-game column
#         for (j in 1:nrow(var_df)) {
#           wCntMetric <- var_df[j, 'wCols']
#           nGmMetric <- var_df[j, 'nCols']
#           x[ , wCntMetric] <- c(0, cumsum(x$won & x[[selector]]==var_df[j, selector])[-n])
#           x[ , nGmMetric] <- c(0, cumsum(x[[selector]]==var_df[j, selector])[-n])
#         }
#       }
#       
#       ## if multi-variable (two at most, with one being by "site")
#       else if (length(by)==2) {
#         
#         ## create second column selector (first selector is always "site" when there are two by variaables)
#         selector <- paste0('o_', by[2])
#         
#         ## add running win count column and n-game column
#         for (j in 1:nrow(var_df)) {
#           wCntMetric <- var_df[j, 'wCols']
#           nGmMetric <- var_df[j, 'nCols']
#           x[ , wCntMetric] <- c(0, cumsum(x$won & x$site==var_df$site[j] & x[[selector]]==var_df[j, selector])[-n])
#           x[ , nGmMetric] <- c(0, cumsum(x$site==var_df$site[j] & x[[selector]]==var_df[j, selector])[-n])
#         }
#       }
#       
#     }
#     
#     x
#   })
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
addJCols <- function(master_df, init_j=100, dist_wgts=c(0.05, 0.1, 0.15)) {

  ## create game ID (unique ID for each matchup) column 
  ## and sort so that every two adjacent rows are team instance of a single game
  master_df$gid <- gsub('-', '', as.character(master_df$date))
  master_df$gid[master_df$site=='H'] <- paste0(master_df$gid, 
                            substr(as.character(master_df$team), 1, 3), 
                            substr(as.character(master_df$o_team), 1, 3))[master_df$site=='H']
  master_df$gid[master_df$site=='A'] <- paste0(master_df$gid,
                            substr(as.character(master_df$o_team), 1, 3),
                            substr(as.character(master_df$team), 1, 3))[master_df$site=='A']
  master_df <- sortByCol(master_df, col='gid')

  ## split df by season
  df_lst <- split(master_df, master_df$season)
  
  ## for each season, apply juice propagation
  ret_df_lst <- lapply(df_lst, function(x) {
    
    ## for each redistrubtion weight
    for (dist_wgt in dist_wgts) {
      
      ## create a list of newly initialized team objects
      tm_obs_lst <- createTmObLst(gm_sch_df=x[, c('season', 'date', 'team', 'o_team')])
      
      ## create J column names
      j_col <- paste0('j', as.character(dist_wgt * 100))
      o_j_col <- paste0('o_', j_col)
      
      ## initialize two vectors with NAs 
      ## (later these vectors will become j and o_j columns)
      j_vec <- o_j_vec <- rep(NA, nrow(x))
      
      ## populate j with initial value of j 
      j_vec[x$n==0] <- init_j
      o_j_vec[x$o_n==0] <- init_j
      
      ## go through each game record (every other row)
      for (i in seq(from=1, to=nrow(x), by=2)) {
        
        ## store game info into variables
        date <- x$date[i]
        team <- as.character(x$team[i])
        o_team <- as.character(x$o_team[i])
        won <- x$won[i]
        tm_ob <- tm_obs_lst[[team]]
        o_tm_ob <- tm_obs_lst[[o_team]]
        nxt_gm_date <- tm_ob$get_next_gm_date(date)
        o_nxt_gm_date <- o_tm_ob$get_next_gm_date(date)
        
        ## calculate updated j after game depending on the game outcome
        if (won) {
          
          ## (team's new j) = (team's current j) + (fraction of opponent team's j)
          new_j <- tm_ob$get_j() + o_tm_ob$get_j() * dist_wgt
          
          ## (opponent's new j) = (opponent's current j) - (fraction of opponent's j)
          o_new_j <- o_tm_ob$get_j() * (1 - dist_wgt)
          
        } else {
          
          ## (team's new j) = (team's current j) - (fraction of team's j)
          new_j <- tm_ob$get_j() * (1 - dist_wgt)
          
          ## (opponent's new j) = (opponent's current j) + (fraction of team's j)
          o_new_j <- o_tm_ob$get_j() + tm_ob$get_j() * dist_wgt
        }
        
        ## populate values for j and o_j 
        j_vec[x$date==nxt_gm_date & x$team==team] <- new_j
        j_vec[x$date==o_nxt_gm_date & x$team==o_team] <- o_new_j
        o_j_vec[x$date==nxt_gm_date & x$o_team==team] <- new_j
        o_j_vec[x$date==o_nxt_gm_date & x$o_team==o_team] <- o_new_j
        
        ## add j-vectors as columns
        x[[j_col]] <- j_vec
        x[[o_j_col]] <- o_j_vec
        
        ## after updating the df, update j values in team objects
        tm_obs_lst[[team]]$set_j(new_j)
        tm_obs_lst[[o_team]]$set_j(o_new_j)
      }
    }
    
    ## return that adds to list
    x
  })

  ## collapse list of dfs into df
  ret_df <- do.call(rbind.data.frame, c(ret_df_lst, stringsAsFactors=FALSE))
  
  ## return
  return(ret_df)
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