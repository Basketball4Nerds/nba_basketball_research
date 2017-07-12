scrape_odds_fr_info_box <- function(info_box, type) {

  ## remove special characters
  info_box <- gsub('\\xa0', ' ', info_box)
  info_box <- gsub('½', '.5', info_box)

  
    if (type %in% c('point-spread', 'total-points')) {
      
      ## if info box is blank
      if (info_box=='') {
        tm1_proj <- tm1_payout <- tm2_pro2 <- tm2_payout <- ''
      } 
      
      ## if inbox is filled
      else {
        tm1_info_box <- strsplit(info_box[[1]], ' ')[[1]]
        tm1_proj <- tm1_info_box[[1]]
        tm1_payout <- tm1_info_box[[2]]
        
        tm2_info_box <- strsplit(info_box[[2]], ' ')[[1]]
        tm2_proj <- tm2_info_box[[1]]
        tm2_payout <- tm2_info_box[[2]]
      }
    }
    
    else if (type=='money-line') {
      
      ## if info box is blank
      if (info_box=='') {
        tm1_payout <- tm2_payout <- ''
      } 
      
      else {
        tm1_payout <- d
      }
    }
  }
  
  ## construct a return object (a list)
  
}



## this function scrapes odds and lines data from Sportsbook Review website 
## for a given date and either return a df or writes to csv
#date <- '2006-05-13'  # example of no data (no game that day)
#date <- '2006-12-13'  # example of missing data
date <- '2015-12-11'  # example of complete data
date <- '2006-12-11'
scrape_data_fr_SBR_by_date <- function(date, 
                                       relTeams, 
                                       type=c('point-spread', 
                                              'total-points', 
                                              'money-line')) {
  
  ## determine which kind of data to scrape 
  type <- type[1]

  ## get date and season
  date <- as.Date(date)
  season <- get_season_fr_date(date)
  
  ## construct request URL and load webpage
  base_url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/'
  if (type=='total-points')
    base_url <- paste0(base_url, 'totals/')
  else if (type=='money-line')
    base_url <- paste0(base_url, 'money-line/')
  req_url <- paste0(base_url, '?date=', strftime(date, '%Y%m%d'))
  webpage <- read_html(req_url)

  ## get a list of game events (list of rows) for a given date
  event_row_lst <- webpage %>% 
    html_nodes('div.event-holder div.eventLine')
  if (length(event_row_lst)==0) return()

  ## initialize empty columns
  tm_col <- o_tm_col <- c()
  if (type=='point-spread')
    line_col <- c()
  else if (type=='total-points')
    tot_proj_col <- c()

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







# ## get data from Pinnacle source
# boxInfo <- gmInfo %>%
#   html_node('div.el-div.eventLine-book') %>%
#   html_nodes('div.eventLine-book-value > b') %>%
#   html_text()
# boxInfo <- gsub('\\xa0', ' ', boxInfo)
# boxInfo <- gsub('½', '.5', boxInfo)
# 
# if ('' %in% boxInfo) {
#   projTm1 <- payoutTm1 <- ''
#   projTm2 <- payoutTm2 <- ''
# } else {
#   boxInfoTm1 <- strsplit(boxInfo[[1]], ' ')[[1]]
#   projTm1 <- boxInfoTm1[[1]]
#   payoutTm1 <- boxInfoTm1[[2]]
# 
#   boxInfoTm2 <- strsplit(boxInfo[[2]], ' ')[[1]]
#   projTm2 <- boxInfoTm2[[1]]
#   payoutTm2 <- boxInfoTm2[[2]]
# }