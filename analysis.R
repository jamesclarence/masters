# Read in libraries
library(tidyverse)

# Read in files
setwd('C:/Users/fishe/Documents/masters/')
l <- read_csv('leaderboard_mastersdotcom.csv')

# Create column: CUT (y/n); Sums of R1-R2, R1-R3, and R1-4
l$r1 <- as.numeric(l$r1)
l$r2 <- as.numeric(l$r2)
l$r3 <- as.numeric(l$r3)
l$r4 <- as.numeric(l$r4)
l$total_par<- as.numeric(l$total_par)


# Sums of R1-R2, R1-R3, and R1-4
l2 <- l %>% mutate(
  cut = if_else(l$position_new == 'MC', 'y', 
                if_else(l$position_new == 'WD', 'wd',
                        'n')),
  r1r2 = r1 + r2,
  r1r3 = r1+ r2 + r3,
  r1r4 = r1 + r2 + r3 + r4)

par <- 72

# Make data frame long ----------------------------------------------------
l3 <- l2 %>% gather(key = round, value = score, r1, r2, r3, r4, r1r2, r1r3, r1r4) 

# Add calculations to long data frame -------------------------------------

l4 <- l3 %>% mutate(par_round = case_when(
  round == 'r1' ~ score - par,
  round == 'r1r2' ~ score - (par * 2),
  round == 'r1r3' ~ score - (par * 3),
  round == 'r1r4' ~ total_par))

# Rank each round within a year
l5 <- l4 %>% group_by(year, round) %>% mutate(round_rank = dense_rank(score))

# Add mean, standard deviation, and z score for each type of round
# l6 <- l5 %>% mutate(round_type = if_else(
#   round == 'r1r2' | round == 'r1r3' | round == 'r1r4', 'multi_rd', 'ind'
# )) %>% 
#   group_by(year, round_type) %>% 
#   mutate(mean_round_type = mean(score, na.rm = T),
#          sd_round_type = sd(score, na.rm = T),
#          z_round_type = ((score-mean_round_type)/sd_round_type))

l6 <- l5 %>% 
  group_by(year, round) %>% 
  mutate(mean_round = mean(score, na.rm = T),
         sd_round = sd(score, na.rm = T),
         z_round = ((score-mean_round)/sd_round))

# Clean player names -----------------------------------------------------

# Rename the A. Espinoza's
l6$player[l6$year == "1934" & l6$position_new == "7" & l6$player == "A. Espinosa"] <- "Al Espinosa"
l6$player[l6$year == "1935" & l6$position_new == "17" & l6$player == "A. Espinosa"] <- "Al Espinosa"
l6$player[l6$year == "1936" & l6$position_new == "15" & l6$player == "A. Espinosa"] <- "Al Espinosa"
l6$player[l6$year == "1937" & l6$position_new == "29" & l6$player == "A. Espinosa"] <- "Al Espinosa"
l6$player[l6$year == "1934" & l6$position_new == "38" & l6$player == "A. Espinosa"] <- "Abe Espinosa"
l6$player[l6$year == "1935" & l6$position_new == "50" & l6$player == "A. Espinosa"] <- "Abe Espinosa"

l7 <- l6 %>% mutate(name_lower = str_to_lower(player),
             name_no_per = str_replace_all(name_lower, "\\.", ""))
  
l8 <- l7 %>% mutate(name_clean = case_when(
  name_no_per == 'c howell' ~ 'c howell iii',
  name_no_per == 'd rummels' ~ 'd rummells',
  name_no_per == 'e woods' ~ 't woods',
  name_no_per == 'j hutchison jr' ~ 'j hutchison',
  name_no_per == 'j ridriguez' ~ 'j rodriguez',
  name_no_per == 'k choi'|name_no_per == 'k j choi'|name_no_per == 'kj choi' ~ 'kj choi',
  name_no_per == 'm m giles iii' ~ 'm giles iii',
  name_no_per == 'm pose (argintina)' ~ 'm pose',
  name_no_per == 'w c campbell' ~ 'w campbell',
  name_no_per == 'y e yang'|name_no_per == 'y yang'|name_no_per == 'ye yang' ~ 'ye yang',
  TRUE ~ as.character(name_no_per)
))

# Unique players
p <- l8 %>% ungroup() %>% select(name_clean) %>% distinct()

# Count number rounds for each player
p_ct <- l8 %>% 
  group_by(name_clean) %>% 
  filter(round_type == 'ind', is.na(score) == FALSE) %>% 
  mutate(rd_ct = n(),
         score_sum = sum(score),
         rd_avg = score_sum/rd_ct) %>%
  select(player = name_clean, rd_ct, score_sum, rd_avg) %>%
  distinct(player, rd_ct, score_sum, rd_avg) 


# https://discgolf.ultiworld.com/2018/02/13/introducing-2017-disc-golf-elo-ratings/
# Methods: The Elo rating equation is: Elo Rating=PR+K*(S-(2*ES/N), 
# where PR = previous rating, K = K-factor, S = round score, ES = expected score (based on other players competing in the same round), and 
# N = number of players. 
# K is a parameter that controls the volatility in ratings. Bigger K values mean more volatility. The K-factor I used was 20, which is a value that works well in a variety of sports. 
# The 2*ES/N portion is modified from the classic Elo rating equation to deal with the fact that disc golf is not a one-on-one sport like chess (see: Building a rating system and Building a modified Elo rating system).

# avg for r1 1934 = 76.4
# ES is 1/72
# S = standardized value of score z score

# For each year and round, need:
  # Standardized value of score (z-score)
  # Expected score
    # How to find expected score? Based on previous ELO rating?
    # 1/(10-((1535-1500)/400+1)) 0.112202
    # 1535+20*((-1)*(.095 - (2*(0.112202/71))))


# Columns to use:
  # Player, Year, Round, Score, Round Avg, SD Avg, Z score player by round, Previous ELO, Updated ELO (mutate)

# After each round, add to a data frame:
  # Player, Year, Round, ELO


x <- l8 %>%
  filter(
  (round == 'r1'|
  round == 'r1r2'|
  round == 'r1r3'|
  round == 'r1r4'),
  is.na(score) == FALSE) %>% 
  select(player = name_clean, year, round, z_round)

df <- data.frame(player = character(),
                 year = character(),
                 round = character(),
                 elo = numeric(),
                 elo_new = numeric(),
                 stringsAsFactors = F
                 )


# Add initial ELO score for each player (elo = 1500)
p <- l8 %>% 
  group_by(name_clean) %>% 
  slice(which.min(year)) %>%
  mutate(round = "r1",
         elo = 1500) %>% 
  select(player = name_clean, year, round, z_round, elo)

# Add each ELO score after each round to data frame df
  # Grab the most recent elo rating to calculate next elo rating


df <- data.frame(
  player = character(),
  year = integer(),
  round = character(),
  elo = numeric(),
  elo_new = numeric()
  )
# Function to get initial ELO rating
# Select right filter (year, round) from data frame of
  # name_clean, year, round, z_round, with no NAs
# Join that filtered data frame with list of players and their first year, round, and initial ELO rating (1500)
# Calculate new ELO ratings
# y = year, r = round
# get_elo_init('1934', 'r1')
get_elo_init <- function(y, r) {
  x %>% 
    filter(year == y, round == r) %>% 
    inner_join(p, by = c("player", "year", "round", "z_round")) %>% 
    add_tally() %>% 
    mutate(elo_new = (1500 + 20*((-1)*(z_round - (2 * ((1/n)/n)))))) %>% 
    select(player, year, round, elo, elo_new)
}

# Function: Get first year of elo
# first_year_elo('1934', 'r1')
first_year_elo <- 
  function(y_init, r_init) {
    init <- get_elo_init(y_init, r_init)
    
    zr2 <- x %>% 
      filter(year == y_init, round == 'r1r2') %>% 
      left_join(select(init, c(player, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round = round.x, elo, elo_new)
    
    zr3 <- x %>% 
      filter(year == y_init, round == 'r1r3') %>% 
      left_join(select(zr2, c(player, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round = round.x, elo, elo_new)
    
    zr4 <- x %>% 
      filter(year == y_init, round == 'r1r4') %>% 
      left_join(select(zr3, c(player, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round = round.x, elo, elo_new)
    
    bind_rows(init, zr2, zr3, zr4)
  }


# Function to get the next year of data
get_next_year_of_data <- function(next_year) {
  next_year_r1 <- x %>% 
    filter(year == next_year, round == 'r1')
  
  next_year_r1
  
  # Select latest round of player who has played
  max_round <- elo_1934 %>% 
    group_by(player) %>% 
    summarise(max_round = max(round))
  
  # Get ELO rating for latest round
  latest_elo <- max_round %>% 
    left_join(elo_1934, by = c("player", "max_round" = "round"))
  
  # Combine latest_elo with r1_1935 data
  # Players not in 1934 - add 1500 elo
  new_player_this_year <- r1_1935 %>%
    select(player) %>% 
    left_join(latest_elo, by = "player") %>% 
    filter(is.na(max_round) == T) %>% 
    mutate(elo = replace_na(elo_new, as.numeric("1500"))) %>% 
    select(year = year.x,
           round,
           player, 
           elo)
  
  # Players in 1935
  played_before_this_year <- r1_1935 %>%
    select(player) %>% 
    left_join(latest_elo, by = "player") %>% 
    filter(is.na(max_round) == F) %>% 
    select(year = year.x,
           round,
           player,
           elo = elo_new)
  
  # Combine players not in 1935 and in 1935
  year_player_list <- rbind(new_player_this_year, played_before_this_year)
  
  # Get z-score in 1935 and r1
  r1_for_calculating_elo <- r1_1935 %>% 
    left_join(year_player_list, by = c("player", "year", "round"))
}


# - Grab next year data
r1_1935 <- x %>% 
  filter(year == '1935', round == 'r1')

# - new data frame: Match on name from 1934. Initial ELO for this next year is the latest ELO from 1934 or initial rating of 1500
r1_1935 %>% 
  left_join(elo_1934, by = 'player')

# Select latest round of player
max_round <- elo_1934 %>% 
  group_by(player) %>% 
  summarise(max_round = max(round))

# Get ELO rating for latest round
latest_elo <- max_round %>% 
  left_join(elo_1934, by = c("player", "max_round" = "round"))

# Combine latest_elo with r1_1935 data
# Players not in 1934 - add 1500 elo
new_player_this_year <- r1_1935 %>%
  select(player) %>% 
  left_join(latest_elo, by = "player") %>% 
  filter(is.na(max_round) == T) %>% 
  mutate(elo = replace_na(elo_new, as.numeric("1500"))) %>% 
  select(year = year.x,
         round,
         player, 
         elo)

# Players in 1935
played_before_this_year <- r1_1935 %>%
  select(player) %>% 
  left_join(latest_elo, by = "player") %>% 
  filter(is.na(max_round) == F) %>% 
  select(year = year.x,
         round,
         player,
         elo = elo_new)

# Combine players not in 1935 and in 1935
year_player_list <- rbind(new_player_this_year, played_before_this_year)

# Get z-score in 1935 and r1
r1_for_calculating_elo <- r1_1935 %>% 
  left_join(year_player_list, by = c("player", "year", "round"))

# ELO for 1935 R1
elo_1935_r1 <- r1_for_calculating_elo %>% 
add_tally() %>% 
  mutate(
    expected_score = 1/(10-((elo-1500)/400+1)),
    elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
  ) %>% 
  select(player, year, round, elo, elo_new)

elo_1935_r2 <- x %>% 
  filter(year == '1935', round == 'r1r2') %>% 
  left_join(select(elo_1935_r1, c(player, elo = elo_new)), by = c("player", "year")) %>% 
  add_tally() %>% 
  mutate(
    expected_score = 1/(10-((elo-1500)/400+1)),
    elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
  ) %>% 
  select(player, year, round = round.x, elo, elo_new)

elo_1935_r3 <- x %>% 
  filter(year == '1935', round == 'r1r3') %>% 
  left_join(select(elo_1935_r2, c(player, elo = elo_new)), by = c("player", "year")) %>% 
  add_tally() %>% 
  mutate(
    expected_score = 1/(10-((elo-1500)/400+1)),
    elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
  ) %>% 
  select(player, year, round = round.x, elo, elo_new)

elo_1935_r4 <- x %>% 
  filter(year == '1935', round == 'r1r4') %>% 
  left_join(select(elo_1935_r3, c(player, elo = elo_new)), by = c("player", "year")) %>% 
  add_tally() %>% 
  mutate(
    expected_score = 1/(10-((elo-1500)/400+1)),
    elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
  ) %>% 
  select(player, year, round = round.x, elo, elo_new)

elo_1935 <- rbind(elo_1935_r1, elo_1935_r2, elo_1935_r3, elo_1935_r4)


# Function in-year calculation
function(y, r) {
  # Round 1
  
  # Round 2 (r1r2)
  
  # Round 3 (r1r3)
  
  # Round 4 (r1r4)
  
  # Add yea
  
  x %>% 
    filter(year == y, round == r) %>% 
    left_join(select(elo_1935_r1, c(player, elo = elo_new)), by = c("player", "year")) %>% 
    add_tally() %>% 
    mutate(
      expected_score = 1/(10-((elo-1500)/400+1)),
      elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
    ) %>% 
    select(player, year, round = round.x, elo, elo_new)
}

# - After matching the name from 1934 and the new year, get most recent ELO rating for each participant in 1934
# - If matching on name doesn't work, make new players' ELO 1500
# - Run ELO ratings for the new year using new data frame
# - Combine 1934 with new year in new data frame
# - Repeat process with new year





# Before 3/24/2018 --------------------------------------------------------


# Attach elo_1934 to p before going to 1935
p %>% 
  left_join(elo_1934, by = c("player", "year", "elo"))

# Need z_round for those in 1934 and in 1935

m <- x %>% 
  filter(year == '1935', round == 'r1') %>% 
  left_join(p, by = c("player", "year", "round", "z_round"))
n <- elo_1934 %>% filter(year == '1934', round == 'r1r4')


n %>%
  ungroup() %>% 
  select(-year, -round) %>% 
  right_join(m, by = c("player", "elo"))








# Function to get the most recent elo score to update it
function(y, r, prev_y, prev_r) {
  x %>% 
    filter(year == y, round == y) %>% 
    
}

# Set ELO rating as 1500

    # 1/(10-((1535-1500)/400+1)) 0.112202
    # 1535+20*((-1)*(.095 - (2*(0.112202/71))))

# 1300 + 20 * (1 - (1/(10(-dr/400) + 1))
# 
# For each round, have the following variables:
#   - pre_round_rating old/pre-match rating
#   - weight constant
#     - r1: 10
#     - r2: 20
#     - r3: 30
#     - r4: 40
#   - W
#     - 1 win
#     - 0.5 draw 
#     - 0 loss
#   - We (1 / (10(-dr/400) + 1))
#   - dr pre_round_ratingPLYER - pre_round_ratingOPP

################
# Expected Pairwise Against Avg. Driver = 1/(10-(Previous Rating - Average Driver Field Rating)/400+1)
# 
# 
# New Rating = Previous Rating +K(Event Score - ((2)Expected Pairwise Score/Number Drivers)
# 
# l3 %>% group_by(year, round) %>% summarise(round_avg = mean(score, na.rm = T))

 
 # Probability of shooting R1 score
# Probability of shooting R2 score
# Probability of shooting R3 score
# Probability of shooting R4 score
# Probability of making the cut


# Graphs ------------------------------------------------------------------
# Comparison between final position and round positions
# x axis = round_rank
# y axis = position_new
# fill = round
l9 %>% 
  filter(round == 'r1',
         position_new != 'WD',
         position_new != 'MC',
         position_new != 'Disqualified',
         position_new != 'DQ') %>% 
  ggplot(aes(x = round_rank, y = as.integer(position_new)) +
  geom_point() +
  xlim(1, 99)
  

