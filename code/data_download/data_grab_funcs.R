############ DATA API / SCRAPER FUNCTIONS ################

## this function takes in a vector of interested metrics and contructs a API request URL
create_SDQL <- function(metrics, date=NULL, season=NULL) {
  
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
get_raw_games_data_via_api <- function(date=NULL, season=NULL, metrics=NULL, def_retry_wait_t=120, api_key=NULL) {
  
  ## initialize API key
  if (is.null(api_key))
    api_key <- 'guest'
  
  ## define metrics to query if not set
  if (is.null(metrics)) {
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
  }

  ## create SDQL
  SDQL <- create_SDQL(metrics, date=date, season=season)
  
  ## create api request url                                                                                                             
  req_url <- paste0('http://api.sportsdatabase.com/nba/query.json?sdql=', URLencode(SDQL), '&output=json&api_key=', api_key)
  
  ## clean output JSON-like string
  webpage <- getURL(req_url)
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
    return(get_raw_games_data_via_api(date=date, season=season, metrics=metrics, def_retry_wait_t=def_retry_wait_t*2, api_key=api_key))
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
        return(get_raw_games_data_via_api(date=date, season=season, metrics=metrics, def_retry_wait_t=def_retry_wait_t*2, api_key=api_key))
      }
    }
    
    ## modify column names  
    col_nms <- metrics
    col_nms <- gsub('(^ )|( $)', '', col_nms)
    col_nms <- gsub(' ', '_', col_nms)
    col_nms <- gsub('\\:', '_', col_nms)
    col_nms <- gsub('points', 'pts', col_nms)
    col_nms <- gsub('_the_', '_', col_nms)
    #col_nms[grepl('o_pts', col_nms)] <- 'pts_alwd_to_o'
    
    ## return an empty data frame if no data  
    if (length(json)==0) {
      df <- as.data.frame(matrix(ncol=length(col_nms), nrow=0))
      names(df) <- col_nms
      return(df)
    }
    
    ## initialize df
    ncols <- length(col_nms)  # length of the header columns
    nrows <- length(json$groups[[1]]$columns[[1]])  # length of the first column rows
    gms_df <- as.data.frame(matrix(NA, nrow=nrows, ncol=ncols))
    names(gms_df) <- col_nms
    
    ## find cols indices of gms_df that will contain quarter scores 
    qtrPtsColIndices <- which(grepl('quarter_scores', names(gms_df)))
    
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
      gms_df[i] <- colVals
    }
    
    ## cast proper data format and type for date 
    gms_df$date <- as.Date(as.character(gms_df$date), format='%Y%m%d')
    
    ## NA for unknown future data
    #gms_df$wins[is.na(gms_df$pts)] <- NA
    #gms_df$losses[is.na(gms_df$pts)] <- NA
    #gms_df$o_wins[is.na(gms_df$pts)] <- NA
    #gms_df$o_losses[is.na(gms_df$pts)] <- NA
    
    ## return
    return(gms_df)
  } 
}


## this function constructs request URL to scrape data off of SportsbookReview
get_SBR_req_url <- function(date, type=c('spreads', 'totals', 'moneylines')) {
  type <- type[1]
  date <- as.Date(date)
  base_url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/'
  if (type=='totals') 
    base_url <- paste0(base_url, 'totals/')    
  else if (type=='moneylines') 
    base_url <- paste0(base_url, 'money-line/')
  else if (type != 'spreads')
    stop('Please provide correct odds type.')
  req_url <- paste0(base_url, '?date=', strftime(date, '%Y%m%d'))
  return(req_url)
}


## this function scrapes odds and lines data from Sportsbook Review website 
## for a given date and either return a df or writes to csv
#date <- '2006-05-13'  # example of no data (no game that day)
#date <- '2006-12-13'  # example of missing data
#date <- '2015-12-11'  # example of complete data
scrape_raw_odds_data_fr_SBR_by_date <- function(date, 
                                                type=c('spreads', 
                                                       'totals', 
                                                       'moneylines'), 
                                                rel_teams=TEAMS) {
  
  ## determine which kind of data to scrape 
  type <- type[1]
  
  ## get date and season
  date <- as.Date(date)
  season <- get_season_fr_date(date)
  
  ## contstruct request URL 
  req_url <- get_SBR_req_url(date, type)
  
  ## load webpage
  webpage <- read_html(req_url)
  
  ## get a list of game events (list of rows) for a given date
  event_row_lst <- webpage %>% 
    html_nodes('div.event-holder div.eventLine')
  if (length(event_row_lst)==0) return()
  
  ## initialize empty columns
  tm_col <- o_tm_col <- c()
  
  ## initialize an empty list to store row values
  lst <- list()
  
  ## loop through a list of game events for that day
  for (row in event_row_lst) {
    
    ## get teams info shown in each row
    tms_info <- row %>%
      html_nodes('a') %>%
      html_text()
    tm1 <- tms_info[2]
    tm2 <- tms_info[3]
    
    ## get odds info (for both team and opponent)
    row_odds <- row %>%
      html_nodes('div.el-div.eventLine-book') %>%
      html_nodes('div.eventLine-book-value > b') %>%
      html_text()
    
    ## remove special characters
    row_odds <- gsub('\\xa0', ' ', row_odds)
    row_odds <- gsub('½', '.5', row_odds)
    
    ## separate team and opponent odds
    tm1_odds <- row_odds[c(TRUE, FALSE)]
    tm2_odds <- row_odds[c(FALSE, TRUE)]
    
    ## append to list
    lst <- c(lst, list(tm1_odds), list(tm2_odds))
    
    ## append to vectors (which will be used as columns)
    tm_col <- c(tm_col, tm1, tm2)
    o_tm_col <- c(o_tm_col, tm2, tm1)
  }
  
  ## construct a df
  ret_df <- do.call(rbind.data.frame, lst)
  names(ret_df) <- paste0('X', 1:10)
  ret_df <- apply(ret_df, 2, as.character)
  ret_df <- cbind.data.frame(season=season, date=date, team=tm_col, o_team=o_tm_col, ret_df)
  
  ## replace city names with team names
  ret_df <- convert_team_labels(ret_df, from='city', to='team')
  
  ## select relevant teams 
  ## (used to filter out all star games in which teams have different names, such as East or West)
  ret_df <- subset(ret_df, team %in% rel_teams & o_team %in% rel_teams)
  
  ## return 
  return(ret_df)
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




## this function downloads latest raw odds data into directory
download_raw_odds_data <- function(dates,
                                   type=c('spreads', 'totals', 'moneylines'),
                                   file_dl_dir_path,
                                   req_wait_t=1.5) {
  
  ## set type
  type <- type[1]
  
  ## convert to character type
  dates <- as.character(dates)
  
  ## iterate over the dates and download data
  for (date in dates) {

    ## pause for specified seconds
    Sys.sleep(req_wait_t)
        
    ## print message
    print_msg <- paste0('Scraping ', type, ' data for the following date: ', date)
    print(print_msg)
    
    ## scrape raw odds data
    data <- scrape_raw_odds_data_fr_SBR_by_date(date=as.Date(date), type=type) 
    
    ## write file 
    file_nm <- paste0(type, '_', format(as.Date(date), '%Y%m%d'), '.csv')
    file_path <- file.path(file_dl_dir_path, file_nm)
    write.csv(data, file_path, row.names=FALSE)
  }
}


