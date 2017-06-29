############ GENERAL/HELPER/MISCELLANEOUS FUNCTIONS ################


## this function takes in a df and returns an empty copy of the original
create_empty_df_copy <- function(df) {
  empty_df_copy <- matrix(NA, nrow=nrow(df), ncol=ncol(df))
  colnames(empty_df_copy) <- colnames(df)
  rownames(empty_df_copy) <- rownames(df)
  return(empty_df_copy)
}


## this function determines if a value is TRUE;
## (can work with NA values)
is.true <- function(x) {
  !is.na(x) & x
}


## this function determines if a value is FALSE;
## (can work with NA values)
is.false <- function(x) {
  !is.na(x) & !x
}


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
