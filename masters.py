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

# Get leaderboards from url_list
leaderboard = []
for u in url_list: 
	t = pd.read_html(u, header=0)
	t_df = t[0]
	l.append(t_df)

# Concatenate leaderboard list
l_df = pd.concat(leaderboard)

# Export concatenated leaderboard list
pd.l_df.to_csv("leaderboard.csv")


url_dict = dict(year = year_list, url = url_list)
# year is key, url is value

# Read tables from url, assign year, and add to leaderboard variable
# leaderboard = []
# for key, value in url_dict.items(): 
# 	t = pd.read_html(value, header=0)
# 	t_df = t[0]
# 	# t_df.assign(yr = key) # '{}'.format(key)
# 	leaderboard.append(t_df)

# for key, value in url_dict.items(): print("{} IS ".format(key))

# for key in url_dict: print("{}: {}".format(key, url_dict[key]))

