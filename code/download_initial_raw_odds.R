
## get dates to iterate over: method 1
start_date <- as.Date('2006-10-31')
today_date <- as.Date(Sys.time())
dates <- seq(from=start_date, to=today_date, by=1)

## get dates to iterate over method 2
dates <- unique(games$date)

## convert to character type
dates <- as.character(dates)

## for each date
for (date in x) {
  
  ## for each type of odds 
  for (type in c('spreads', 'totals', 'moneylines')) {
    
    Sys.sleep(1.5)
    
    print_msg <- paste0('Scraping ', type, ' data for the following date: ', date)
    print(print_msg)
    data <- scrape_raw_odds_data_fr_SBR_by_date(date=date, type=type)        
    
    file_nm <- paste0(type, '_', format(as.Date(date), '%Y%m%d'), '.csv')
    file_dl_path <- file.path('./data/raw', type, file_nm)
    write.csv(data, file_dl_path, row.names=FALSE)
  }
}
