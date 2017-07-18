############ PREPROCESSOR FUNCTIONS ################

## this function adds win percentage columns to master df
## this function adds varied-by-variable win percentage columns to master df
add_wpc_cols <- function(master_df, 
                         vary_by=NULL, 
                         new_colnm_apnd_str=NULL,
                         rnd_dgt=3, 
                         add_opp_cols=FALSE,
                         rm_w_cnt_cols=FALSE,
                         rm_n_cnt_cols=FALSE) {
  
  ## get original column names
  orig_cols <- names(master_df)
  
  ## add varied-by-variable cumulative win count (w) and game count (n)
  ## (do not add opponent cols)
  master_df <- add_cum_cnt_cols(master_df, 
                                cols=c('w', 'n'),
                                vary_by=vary_by,
                                add_opp_cols=FALSE)

  
  ## get a vector of win count column names
  win_cnt_cols <- colnames(master_df)[grepl('^w_', colnames(master_df))]
  
  ## get a vector of game count column names
  gm_cnt_cols <- colnames(master_df)[grepl('^n_', colnames(master_df))]
  
  ## create a vector win percent column names
  win_pc_cols <- gsub('^w', 'wpc', win_cnt_cols)
  
  ## for each pair of w-and-n column names
  for (i in 1:length(win_cnt_cols)) {
    win_cnt_col <- win_cnt_cols[i]
    gm_cnt_col <- gm_cnt_cols[i]
    win_pc_col <- win_pc_cols[i]
    
    ## calculate win perc and add as a column
    master_df[win_pc_col] <- round(master_df[win_cnt_col] / master_df[gm_cnt_col], rnd_dgt)
  }
  
  ## replace NaN w/ NA
  master_df[is.nan.data.frame(master_df)] <- NA

  ## remove win count columns if specified
  if (rm_w_cnt_cols)
    master_df <- master_df[ , !(grepl('^w_', names(master_df)))]
  
  ## remove n-game count columns if specified
  if (rm_n_cnt_cols) 
    master_df <- master_df[ , !(grepl('^n_', names(master_df)))]
  
  ## fill in opponent columns
  if (add_opp_cols) {
    
    ## get new columns created
    new_cols <- setdiff(colnames(master_df), orig_cols)
    
    ## create win percentage columns for opponent
    master_df <- fill_in_opp_cols(master_df, cols=new_cols)
  }
  
  ## return 
  return(master_df)
}


## this function creates a list of initialized team objects
createTmObLst <- function(gm_sch_df) {
  
  ## initialize an empty list
  tm_obs_lst <- vector(mode = "list", length = length(TEAMS))
  
  ## label the list objects
  names(tm_obs_lst) <- TEAMS
  
  ## for each team name
  for (team_name in TEAMS) {
    
    ## create team object
    tm_ob <- Team(name=team_name, gm_sch_df=gm_sch_df)
    
    ## add to list of team objects
    tm_obs_lst[[team_name]] <- tm_ob
  }
  
  ## return
  return(tm_obs_lst)
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
      j_vec[x$n_gen==0] <- init_j
      o_j_vec[x$o_n_gen==0] <- init_j
      
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



#head(master$qtrpts)
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



## this function takes in raw odds df and returns a parsed version of it
create_parsed_odds_df <- function(odds_df, type=c('spreads', 'totals', 'moneylines')) {
  
  ## set type
  type <- type[1]
  
  ## get names of original columns (that contain the odds) to iterate over 
  orig_odds_cols <- paste0('X', 1:10)
  
  ## base columns
  base_cols <- c('season', 'date', 'team', 'o_team')
  
  ## odds-maker sources
  oddsmakers <- c('pinnacle', 'betonline', 'interlops', 'bookmaker', '5dimes', 
                  'betdsi', 'heritage', 'bovada', 'youwager', 'justbet')
  
  ## processing moneylines df
  if (type=='moneylines') {
    
    ## for each col, convert type from character to numeric
    for (col in orig_odds_cols) {
      odds_df[[col]] <- as.numeric(odds_df[[col]])
    }
    
    ## change colnames
    new_odds_cols <- paste0(oddsmakers, '_ml')
    names(odds_df) <- c(base_cols, new_odds_cols)

    ## return
    return(odds_df)
  } 
  
  ## processing spreads/totals df
  else if (type %in% c('spreads', 'totals')) {
    
    ## create base df (that contain non-odds info, such as season, date, team, o_team)
    ret_df <- odds_df[ , base_cols]
    
    ## for each col, parse data
    for (col in orig_odds_cols) {

      ## select col, replace alphabet characters, split data
      col_vals <- odds_df[[col]] 
      col_vals <- gsub('^PK', '0 ', col_vals)  # rare PK cases for spreads data
      col_vals_split <- strsplit(col_vals, ' ')
      
      ## extract projections data (totals or lines) and payout data
      projections <- unlist(lapply(col_vals_split, function(x) x[1]))  # lines in case of spreads df; totals projected in case of totals df
      payouts <- unlist(lapply(col_vals_split, function(x) x[2]))  # payout moneylines
      
      ## add to df 
      ret_df <- cbind(ret_df, projections, payouts)
    }

    ## create col names
    new_odds_cols <- rep(oddsmakers, each=2)
    if (type=='spreads')
      new_odds_cols <- paste0(new_odds_cols, c('_line', '_payout'))
    else if (type=='totals')
      new_odds_cols <- paste0(new_odds_cols, c('_total', '_payout'))
    names(ret_df) <- c(base_cols, new_odds_cols)

    ## return
    return(ret_df)
  }
}


## this function adds game ID (gid) to df;
## gid consists of: 
## - game date without dashes
## - first 3 characters of team 1 
## - first 3 characters of team 2
## team 1 and team 2 are alphabetically ordered
add_gid <- function(df) {
  df$gid <- apply(cbind(format(as.Date(df$date), '%Y%m%d'), 
                            substr(as.character(df$team), 1, 3),
                            substr(as.character(df$o_team), 1, 3)), 
                      1, function(x) paste(sort(x), collapse="")) 
  return(df)
}
