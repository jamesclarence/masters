# Get Masters Leaderboards

# import libraries
import pandas as pd

# Range of years (1934-2017)
year_list = list(range(1934, 2018))

# Get list of urls
'http://www.augusta.com/masters/historic/leaderboards/' + year + 'leaderboard.shtml'


for i in year_list:
	l = 'http://www.augusta.com/masters/historic/leaderboards/' + str(i) + 'leaderboard.shtml'
	print(l)

# specify the url
url = 'http://www.augusta.com/masters/historic/leaderboards/1969leaderboard.shtml'

# Use pandas (separate from bs4)
table = pd.read_html(url,  header=0)
table_df = table[0]
table_df.assign(year = '1969')
