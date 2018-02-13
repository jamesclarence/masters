# Get Masters Leaderboards

# import libraries
import pandas as pd

# Range of years (1934-2017)
year_list = list(range(1934, 2018))

# Remove years 1943-45 because of World War II
del year_list[9:12]

# Get list of urls with Masters results 
url_list = []
for i in year_list:
	u = 'http://www.augusta.com/masters/historic/leaderboards/' + str(i) + 'leaderboard.shtml'
	url_list.append(u)

url_dict = dict(year = year_list, url = url_list)

# Read tables from url, assign year, and add to leaderboard variable
leaderboard = []
for year, url in url_dict.items(): 
	t = pd.read_html(url, header=0)
	t_df = t[0]
	t_df.assign(year = str(year))
	leaderboard.append(t_df)

# for u in url_list: 
# 	t = pd.read_html(u, header=0)
# 	t_df = t[0]
# 	leaderboard.append(t_df)	

# specify the url
url = 'http://www.augusta.com/masters/historic/leaderboards/1969leaderboard.shtml'

# Read table from url
table = pd.read_html(url,  header=0)
table_df = table[0]
table_df.assign(year = '1969')

