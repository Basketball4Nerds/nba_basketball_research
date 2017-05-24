#### NOTES:
#### link to NBA Sport Data Query Language Access: http://sportsdatabase.com/nba/query
#### example SDQL query: season, date, team, points, o:team, o:points @ team=Wizards



#### define variables
teams_file_path = '../data/TEAMS.txt'
API_KEY = 'guest'



#### import libraries
import urllib
import urllib2
import json
import csv
import time



#### define functions
## this function takes in an api request url and returns python dictionary of json data returned
def get_json_from_url(request_url):
    res = urllib2.urlopen(request_url)
    page = res.read()
    page = page.lstrip('json_callback(')
    page = page.rstrip(';) *(\n)?')
    page = page.replace("'", "\"")  # quotes around json values must be double quotes, not single quotes
    json_data = json.loads(page)
    return json_data


## this function takes in python dictionary and return a 2-dimensional list (representing a data frame)
def convert_json_to_list2d(json):

    ## get a list of column names and a list of column values
    colnames = json['headers']
    columns = json['groups'][0]['columns']

    ## invert/transpose the list of lists (i.e. the 2-d data frame)
    df = [list(x) for x  in zip(*columns)]

    ## add the column names at the "top"
    df = [colnames] + df

    ## return data frame
    return df


## this function takes in a list of lists (2-d data frame representation) and output data path and export to csv
def write_2d_list_to_csv(list2d, output_path):
    with open(output_path, "wb") as f:
        writer = csv.writer(f)
        writer.writerows(list2d)



#### execute program
## get the names of teams
file = open(teams_file_path, 'r')
teams = [line.strip('\n') for line in file]
file.close()


## for each team, generate a request url to make an api call
for team in teams:

    ## print
    print 'Downloading historical data for the following team: ' + team

    ## define query 
    SDQL = 'season, date, team, points, wins, losses, o:team, o:points, o:wins, o:losses @ team={team}'.format(team=team)
    
    ## create api request url
    request_url = 'http://api.sportsdatabase.com/nba/query.json?sdql=' + urllib.quote_plus(SDQL) + '&output=json&api_key=' + API_KEY
    
    ## obtain json data
    json_data = get_json_from_url(request_url)

    ## convert json data to 2-d list
    df = convert_json_to_list2d(json_data)

    ## write data to csv
    output_file_name = team + '.csv'
    write_2d_list_to_csv(df, output_file_name)

    ## add a courtesy wait time (2 seconds)
    time.sleep(2)

