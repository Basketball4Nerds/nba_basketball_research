## import libraries
import urllib2
import datetime
import bs4
import csv
import time



## create a list of dates to iterate over
#start_date = datetime.date(2006, 10, 31)
#end_date = datetime.date.today()

#start_date = datetime.date(2006, 10, 31)
#end_date = datetime.date.today()


delta = end_date - start_date
dates = []
for i in range(delta.days + 1):
    date = start_date + datetime.timedelta(days=i)
    dates.append(date)
game_months = [1, 2, 3, 4, 5, 6, 10, 11, 12]
dates = [date for date in dates if date.month in game_months]



## iterate over a list of dates
base_url = 'http://www.sportsbookreview.com/betting-odds/nba-basketball/'
err_dates = []
for date in dates:
    ## create string representation of date
    date_str = date.strftime('%Y-%m-%d')

    ## add a delay mechanism
    time.sleep(1)

    ## get html from webpage
    url = base_url + '?date=' + date.strftime('%Y%m%d')
    res = urllib2.urlopen(url)
    html = res.read()

    ## load html in bs object
    soup = bs4.BeautifulSoup(html, 'html.parser')
    
    ## print url for display
    print url

    ## get all games data (multiple rows) for a particular day
    #games_info = soup.select('div.eventLine.status-scheduled div.eventLine.status-complete')
    games_info = soup.select('div.event-holder div.eventLine')
    if games_info:
        rows = []
        for game_info in games_info:

            ## get teams info
            teams_info = game_info.select('div.el-div.eventLine-team > div')
            team1 = teams_info[0].find('a').text
            team2 = teams_info[1].find('a').text

            
            ## get odds data (from Pinnacle only)
            ## sometimes, data cannot be easily scraped
            try:
                odds_info = game_info.select('div.el-div.eventLine-book')[0]
                odds_info = odds_info.select('div.eventLine-book-value > b')
                odds_info = [elem.text.replace(u'\xa0', u' ').replace(u'\xbd', u'.5') for elem in odds_info]

                team1_odds_info = odds_info[0].split(' ')
                team1_line_adjustor = team1_odds_info[0]
                team1_payout_odd = team1_odds_info[1]

                team2_odds_info = odds_info[1].split(' ')
                team2_line_adjustor = team2_odds_info[0]
                team2_payout_odd = team2_odds_info[1]

            except:
                err_dates.append(date_str)
                break

            ## extract season from date
            if date.month in [10, 11, 12]:
                season = date.year
            elif date.month in [1, 2, 3, 4, 5, 6]:
                season = date.year - 1

            ## create two rows per game
            row1 = [season, date_str, team1, team2, team1_payout_odd, team1_line_adjustor]
            row2 = [season, date_str, team2, team1, team2_payout_odd, team2_line_adjustor]

            ## append to rows list
            rows.append(row1)
            rows.append(row2)

        colnames = ['season', 'date', 'team', 'o_team', 'payout_odd', 'line_adjustor']
        rows = [colnames] + rows

        ## write to csv
        csv_file_path = '../data/odds_historical_data/odds_' + date_str + '.csv'
        with open(csv_file_path, "wb") as f:
            writer = csv.writer(f)
            writer.writerows(rows)


## print dates for which odds data scraping was unsuccessful
print err_dates
#err_dates = ['2006-12-13', '2007-05-06', '2007-05-26', '2007-05-31', '2008-11-08', '2008-11-15', '2008-11-23', '2008-12-22', '2010-03-24', '2010-10-09', '2010-10-13', '2010-12-31', '2011-03-09', '2011-11-01', '2012-11-01', '2013-02-01', '2013-04-02', '2014-01-16', '2014-10-04', '2014-10-08', '2014-10-22', '2014-10-23', '2014-10-24', '2014-11-01', '2015-11-07']


