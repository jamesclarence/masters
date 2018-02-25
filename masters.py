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
	t_df = t_df.assign(year = k)
	l.append(t_df)

# Concatenate leaderboard list into one data frame
l_df = pd.concat(l)

# Lower column names
l_df.columns = l_df.columns.str.lower()

# Replace r4 value for Ray Billows in 1939 (Should = 76)
l_df.iat[261, 6] = 76

# Replace r4 value for Tommy Armour in 1940 (Should = 76)
l_df.iat[301, 6] = 76

# Replace r4 value for George Hamer in 1948 (Should = 75)
l_df.iat[558, 6] = 75

# Replace r3 value for Art Bell in 1948 (Should = 74)
l_df.iat[531, 5] = 74

# Export leaderboard
l_df.to_csv("masters_results_augustdotcom.csv", index=False)