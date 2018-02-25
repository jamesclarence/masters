# Create one leaderboard file from different results files

# This file will:
    # Add earnings from masters_results_augustadotcom
    # Add earnings from masters_results_pgatourdotcom
    # Clean names in masters_results_augustadotcom and masters_results_pgatourdotcom
    # Clean position (Remove T from tied positions)

# Import modules
import pandas as pd
import re

# Read in leaderboard file from august.com
l = pd.read_csv("masters_results_augustdotcom.csv")

# Read in google sheet from pgatour.com
p = pd.read_csv("masters_results_pgatourdotcom.csv")

# Add cut data frame to masters_results_augustdotcom.csv
l_aug_before1970 = l[l.year < 1970] 

# Combine augustadotcom 1934-1969 + pgatourdotcom 1970-2017
l_concat = pd.concat([l_aug_before1970, p])

l_export = l_concat.sort_values(by = ['year','position'])

# Replace 'T' in position where ties (ie, 'T49' to '49')
# If matches the pattern r"(T[0-9]{1,2})", keep the number 
regex = re.compile(r'(?P<one>T)(?P<two>[0-9]{1,2})')
m = pd.Series(l_concat.position)
m2 = m.str.replace(regex, lambda x: x.group('two'))

# Assign new position column to data frame
l2 = l_concat.assign(position_new = m2)

# Regex: Replace "-" values in r1, r2, r3, and/or r4
regex_dash = re.compile(r'-')

# Rearrange columns
l3 = l2[['year', 'player', 'position_new', 'r1', 'r2', 'r3', 'r4', 'strokes', 'final', 'earnings', 'fedex_cup_points']]	

# Write final data frame to csv
l3.to_csv("leaderboard.csv", index=False)
