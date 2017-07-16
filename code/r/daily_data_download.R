
today_date <- Sys.Date()
# today_date <- as.Date('2011-02-05')
download_raw_odds_data(dates=today_date, type='spreads', 
                       file_dl_dir_path='./data/raw_in_queue/spreads')
download_raw_odds_data(dates=today_date, type='totals',
                       file_dl_dir_path='./data/raw_in_queue/totals')
download_raw_odds_data(dates=today_date, type='moneylines',
                       file_dl_dir_path='./data/raw_in_queue/moneylines')
