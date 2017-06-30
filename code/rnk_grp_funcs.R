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


# ddply(master, 'season', function(x) {
#   print(table(x$OG))
#   print(x$season[1])
#   print('---')
# })


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



## this function takes in a vector a retro-fills any NA values 
## e.g. c(NA, 1, 2, NA, NA, 3) would become c(1, 1, 2, 3, 3, 3)
retro_fill_nas <- function(x) {
  
  ## get a vector of non-NA indices 
  complete_nna_indices <- which(!is.na(x))
  
  ## for each non-NA index, apply retro-fill
  for (ind in complete_nna_indices) {
    
    ## get value to retro-fill
    retro_fill_val <- x[ind]
    
    ## get indices of values to retro-fill
    complete_retro_na_indices <- which(is.na(x[1:ind]))
    
    ## retro-fill with value
    x[complete_retro_na_indices] <- retro_fill_val
  }
  
  ## return
  return(x)
}


## this function creates performance-standing-by-date df
create_std_by_date_df <- function(master_df, metric) {
  
  ## create df with relevant columns
  df <- master_df[, c('date', 'team', metric)]
  
  ## create an initial standing-by-date df that contains NA "holes"
  std_by_date_df <- dcast(df, date ~ team, value.var=metric)
  
  ## store game dates in a variable
  gm_dates <- std_by_date_df$date
  
  ## remove game dates column in df and store as row names
  std_by_date_df$date <- NULL
  rownames(std_by_date_df) <- gm_dates
  
  ## for each team
  for (team in names(std_by_date_df)) {
    
    ## grab team's vector with NA "holes" to retro-fill
    x <- std_by_date_df[, team]
    
    ## retro-fill NAs
    x <- retro_fill_nas(x)
    
    ## first game date (and dates prior to the first game) should be assigned NA
    first_gm_date <- df[df$team==team, 'date'][1]
    na_indices <- 1:which(first_gm_date==gm_dates)
    x[na_indices] <- NA
    
    ## put retro-filled vector back into df
    std_by_date_df[, team] <- x
  }
  
  ## return standing-by-date df
  return(std_by_date_df)
}


## this function takes in a vector of numeric values and return letter-based 
## ranks (A, B, C, ...) based on quantile distribution, where A signifies the best 
## performance indicator
ret_ABC_rnks_by_qntl <- function(x, higher_num_bttr_perf) {
  
  ## use previous seasons to calculate quantiles
  qntls <- quantile(x, probs=seq(0, 1, 1/3), na.rm=TRUE)
  
  ## if higher numeric value signifies better performance
  if (higher_num_bttr_perf) {
    rnks <- ifelse(x >= qntls[3], 'A', 
                   ifelse(x <= qntls[2], 'C', 'B'))
  }
  
  ## if higher numeric value signifies lower performance
  else {
    rnks <- ifelse(x >= qntls[3], 'C',
                   ifelse(x <= qntls[2], 'A', 'B'))
  }
  
  ## return
  return(rnks)
}


## this function takes in a vector of numeric values and return letter-based
## ranks (A, B, C) based on standard deviation, where A signifies the best 
## performance indicator
ret_ABC_rnks_by_sd <- function(x, higher_num_bttr_perf) {
  
  ## calculate mean and sd of te numeric values provided
  mean <- mean(x)
  sd <- sd(x)
  
  ## if higher numeric value signifies better performance
  if (higher_num_bttr_perf) {
    rnks <- ifelse(x >= (mean + sd), 'A', 
                   ifelse(x <= (mean - sd), 'C', 'B'))
  }
  
  ## if higher numeric value signifies lower performance
  else {
    rnks <- ifelse(x >= (mean + sd), 'C',
                   ifelse(x <= (mean - sd), 'A', 'B'))
  }  
  
  ## return 
  return(rnks)
}


## this function takes in standing-by-date df and creates 
## rank-standing-by-date df
create_rnkd_tm_std_by_date_df <- function(master_df, metric, higher_num_bttr_perf, method) {
  
  ## apply the following lines of code by season
  ms_df <- ddply(master_df, 'season', function(ss_df) {
    
    ## create standing-by-date df (consists of numeric values)
    std_by_date_df <- create_std_by_date_df(ss_df, metric)
    
    ## initialize two ranked-standings-by-date dfs 
    ## by copying dimensions from original df
    rnkd_std_by_date_df <- create_empty_df_copy(std_by_date_df) 
    
    ## index where all row values are filled and NA-free
    complete_row_strt_ind <- max(apply(std_by_date_df, 2, function(x) min(which(!is.na(x)))))
    
    ## starting from rows where performance values are present for all teams
    for (i in complete_row_strt_ind:nrow(std_by_date_df)) {
      
      ## get current standing performance as a vector
      x <- as.numeric(std_by_date_df[i, ])
      
      ## derive quantile-derivation ranks
      if (method=='qntl') {
        rnks <- ret_ABC_rnks_by_qntl(x, higher_num_bttr_perf)
      } 
      
      ## derive sd-derivation ranks
      else if (method=='sd') {
        rnks <- ret_ABC_rnks_by_sd(x, higher_num_bttr_perf)
      }
      
      ## add vector of ranks as row
      rnkd_std_by_date_df[i, ] <- rnks
    }
    
    ## convert matrix to df
    rnkd_std_by_date_df <- as.data.frame(rnkd_std_by_date_df, stringsAsFactors=FALSE)
    
    ## add date column  
    rnkd_std_by_date_df$date <- as.Date(rownames(rnkd_std_by_date_df))
    
    ## get rank nomenclature prefix from metric
    rnk_nm_prefix <- strsplit(metric, '_')[[1]][1]
    
    ## get rank nomenclature suffix from method
    rnk_nm_suffix <- ifelse(method=='qntl', '_qntl_rnk',
                            ifelse(method=='sd', '_sd_rnk', '_rnk'))
    
    ## get rank nomenclature
    rnk_col_nm <- paste0(rnk_nm_prefix, rnk_nm_suffix)
    
    ## melt dfs
    rnk_df <- melt(rnkd_std_by_date_df, 
                   id.vars='date', 
                   variable.name='team', 
                   value.name=rnk_col_nm)
    
    ## set names for melted df
    names(rnk_df) <- c('date', 'team', rnk_col_nm)
    
    ## return df for single season
    rnk_df
  })
  
  ## un-factor team column
  ms_df$team <- as.character(ms_df$team)
  
  ## return
  return(ms_df)
}


## this function adds A-B-C offensive/defensive rank standings columns
add_rnk_cols <- function(master_df, metric, 
                         higher_num_bttr_perf, 
                         method, add_opp_cols=FALSE) {
  
  ## get original column names
  orig_cols <- names(master_df)
    
  ## for each metric and its order direction
  for (i in 1:length(metric)) {
    
    ## for each method
    for (m in method) {
      
      ## create ranked-team-standing-by-date df
      rnkd_tm_std_by_date_df <- create_rnkd_tm_std_by_date_df(master_df, metric[i], higher_num_bttr_perf[i], method=m)
      
      ## merge those dfs onto original master df
      master_df <- left_join(master_df, rnkd_tm_std_by_date_df, by=c('season', 'team', 'date'))
    }
  }
  
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


