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
  
  library(BBmisc)
  
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


