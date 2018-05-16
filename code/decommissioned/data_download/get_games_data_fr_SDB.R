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

