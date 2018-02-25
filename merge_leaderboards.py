# Add Cuts from 1970-2017 to masters_results_augustdotcom.csv
# Cuts from 1970-2017 are from Google Sheet copied from
# https://www.pgatour.com/tournaments/masters-tournament/past-results.html
# The CSV from the Google Sheet is masters_results_pgatourdotcom.csv
# Google Sheet is located here: https://docs.google.com/spreadsheets/d/1xcZDq3m2CtD_HsxFCstn8DW4KF-Uugqcs3_F-3o94CA/edit?usp=sharing

# Cuts started at the Masters in 1957
# After this, will need cuts from 1957-1969

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
x = list(l_concat.position)

# If matches the pattern r"(T[0-9]{1,2})", replace the 'T' with nothing 
regex = re.compile(r'(?P<one>T)(?P<two>[0-9]{1,2})')
m = pd.Series(l_concat.position)
m2 = m.str.replace(regex, lambda x: x.group('two'))

# Assign new position column to data frame
l2 = l_concat.assign(position_new = m2)

# Rearrange columns
l3 = l2[['year', 'player', 'position_new', 'r1', 'r2', 'r3', 'r4', 'strokes', 'final', 'earnings', 'fedex_cup_points']]	

# Write final data frame to csv
l3.to_csv("leaderboard.csv", index=False)

# Get average of first rounds by cut or not cut
# Criteria: Played both R1 and R2



#m = pd.Series(l_concat.position)
#if re.search(regex, m):
#
#    # If we want, we can use the MatchObject's start() and end() methods 
#    # to retrieve where the pattern matches in the input string, and the 
#    # group() method to get all the matches and captured groups.
#    match = re.search(regex, m)
#    
#    # This will print [0, 7), since it matches at the beginning and end of the 
#    # string
#    print("Match at index %s, %s" % (match.start(), match.end()))
#    
#    
##
#else:
#    # If re.search() does not match, then None is returned
#    print("The regex pattern does not match. :(")