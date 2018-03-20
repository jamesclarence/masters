# Clean Google sheet copied from masters.com

# Import modules
import pandas as pd
import re

# Read in google sheet from masters.com
m = pd.read_csv("masters_results_mastersdotcom.csv")

# Replace 'T' in position where ties (ie, 'T49' to '49')
# If matches the pattern r"(T[0-9]{1,2})", keep the number 
regex = re.compile(r'(?P<one>T)(?P<two>[0-9]{1,2})')
m_pos_series = pd.Series(m.position)
m_pos_new = m_pos_series.str.replace(regex, lambda x: x.group('two'))

# Assign new position column to data frame
m2 = m.assign(position_new = m_pos_new)

# Rearrange columns
m3 = m2[['year', 'player', 'position_new', 'r1', 'r2', 'r3', 'r4', 'total_strokes', 'total_par']]	

# Write final data frame to csv
m3.to_csv("leaderboard_mastersdotcom.csv", index=False)
