############ PREPROCESSOR FUNCTIONS ################

## this function takes in df and "fills in" the selected metrics
## for opponent by join method
fill_in_opp_cols <- function(df, cols) {
  
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
## as well as running sum columns of other numeric values
## by home/away, opponent def/off rank, opponent conf, etc.
add_varsp_runsum_cols <- function(master_df, cols, 
                                  by=list('site', 'cnf'), add_opp_cols) {
  
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


## this function adds win percentage columns from w, n columns
add_wpc_cols_fr_w_n_cols <- function(df, rnd_dgts=3, add_opp_cols=FALSE) {

  ## get original columns
  orig_cols <- colnames(df)
  
  ## get win count column names
  win_cnt_cols <- colnames(df)[grepl('^w_', colnames(df))]
  
  ## get game count column names
  gm_cnt_cols <- colnames(df)[grepl('^n_', colnames(df))]  
  
  ## create win percent column names
  win_pc_cols <- gsub('^w', 'wpc', win_cnt_cols)
  
  ## for each win count column name  
  for (i in 1:length(win_cnt_cols)) {
    win_cnt_col <- win_cnt_cols[i]
    gm_cnt_col <- gm_cnt_cols[i]
    win_pc_col <- win_pc_cols[i]
    df[win_pc_col] <- round(df[win_cnt_col] / df[gm_cnt_col], rnd_dgts)
  }
  
  ## replace NaN w/ NA
  df[is.nan.data.frame(df)] <- NA
  
  ## fill in opponent columns
  if (add_opp_cols) {
    
    ## get new columns created
    new_cols <- setdiff(colnames(df), orig_cols)
    
    ## create win percentage columns for opponent
    df <- fill_in_opp_cols(df, cols=new_cols)
  }
  
  ## return
  return(df)
}


## this function adds varied-by-variable win percentage columns to master df
add_vary_by_wpc_cols <- function(master_df, 
                                 vary_by, 
                                 rnd_dgts=3, 
                                 add_opp_cols=FALSE) {

  ## for each vary-by variable
  for (var in vary_by) {
    
    ## add varied-by-variable cumulative win count (w) and game count (n)
    master_df <- add_cum_cnt_cols(master_df, 
                                  cols=c('w', 'n'),
                                  agg_vars=var,
                                  new_colnm_apnd_str=tocamel(paste0(var, '_sp')))
  }
  
  ## create and add win percentage columns from w, n columns
  master_df <- add_wpc_cols_fr_w_n_cols(master_df, rnd_dgts, add_opp_cols)
    
  ## remove win and loss count columns
  master_df <- master_df[ , !(grepl('^w_', names(master_df)) | grepl('^l_', names(master_df)))]
  
  ## return
  return(master_df)
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