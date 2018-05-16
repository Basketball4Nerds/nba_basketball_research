
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







## get dates to iterate over: method 1
start_date <- as.Date('2006-10-31')
today_date <- as.Date(Sys.time())
dates <- seq(from=start_date, to=today_date, by=1)

## get dates to iterate over method 2
dates <- unique(games$date)

## convert to character type
dates <- as.character(dates)

## for each date
for (date in dates) {
  
  ## for each type of odds 
  for (type in c('spreads', 'totals', 'moneylines')) {
    
    ## brief sleeper
    Sys.sleep(1.5)
    
    ## print alert on console
    print_msg <- paste0('Scraping ', type, ' data for the following date: ', date)
    print(print_msg)
    
    ## scrape data
    data <- scrape_raw_odds_data_fr_SBR_by_date(date=date, type=type)
    
    ##  only if there is data for the date
    if (!is.null(data)) {
      file_nm <- paste0(type, '_', format(as.Date(date), '%Y%m%d'), '.csv')
      file_dl_path <- file.path('./data/raw', type, file_nm)
      write.csv(data, file_dl_path, row.names=FALSE)
    }
  }
}



## download for today only
today_date <- Sys.Date()
# today_date <- as.Date('2011-02-05')
download_raw_odds_data(dates=today_date, type='spreads', 
                       file_dl_dir_path='./data/raw_in_queue/spreads')
download_raw_odds_data(dates=today_date, type='totals',
                       file_dl_dir_path='./data/raw_in_queue/totals')
download_raw_odds_data(dates=today_date, type='moneylines',
                       file_dl_dir_path='./data/raw_in_queue/moneylines')

