# Get Masters Leaderboards from augusta.com

# Import libraries
import pandas as pd

# Range of tournament years (1934-2017)
year_list = list(range(1934, 2018))

# Remove years 1943-45 because of World War II
del year_list[9:12]

# Get list of urls with Masters results 
url_list = []
for i in year_list:
	u = 'http://www.augusta.com/masters/historic/leaderboards/' + str(i) + 'leaderboard.shtml'
	url_list.append(u)

# Add list of years and corresponding URLs into dictionary
# year is key, url is value
url_dict = dict(zip(year_list, url_list))

# Read tables from url, assign year, and add to empty list
l = []
for k, v in url_dict.items(): 
	t = pd.read_html(v, header=0)
	t_df = t[0]
	t_df = t_df.assign(year = k) # '{}'.format(key)
	l.append(t_df)

# Concatenate leaderboard list into one data frame
l_df = pd.concat(l)

# Export leaderboard
l_df.to_csv("leaderboard.csv")