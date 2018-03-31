# Calculating Masters ELO


# Packages ----------------------------------------------------------------

library(tidyverse)

# Read in files -----------------------------------------------------------

p <- read_csv("player_list_initial_year.csv")
l <- read_csv("leaderboard_with_z_score_for_elo_calculating.csv")


# Empty data frame to append ELO calculations -----------------------------

# Add each ELO score after each round to data frame df
  # Grab the most recent elo rating to calculate next elo rating


df <- data.frame(
  player = character(),
  year = integer(),
  round = character(),
  elo = numeric(),
  elo_new = numeric()
  )


# Get initial ELO rating for 1934 -----------------------------------------


# get_elo_init()
# Function to get initial ELO rating
# Select right filter (year, round) from data frame of
  # name_clean, year, round, z_round, with no NAs
# Join that filtered data frame with list of players and their first year, round, and initial ELO rating (1500)
# Calculate new ELO ratings

# y = year
# r = round
# l = "leaderboard_with_z_score_for_elo_calculating.csv" 
# p = "player_list_initial_year.csv"
# get_elo_init('1934', 'r1')

get_elo_init <- function(y, r) {
  l %>% 
    filter(year == y, round == r) %>% 
    inner_join(p, by = c("player", "year", "round", "z_round")) %>% 
    add_tally() %>% 
    mutate(elo_new = (1500 + 20*((-1)*(z_round - (2 * ((1/n)/n)))))) %>% 
    select(player, year, round, elo, elo_new)
}


# Calculate first year of ELO (1934) --------------------------------------

# first_year_elo('1934', 'r1')

first_year_elo <- 
  function(y_init, r_init) {
    init <- get_elo_init(y_init, r_init)
    
    zr2 <- l %>% 
      filter(year == y_init, round == 'r1r2') %>% 
      left_join(select(init, c(player, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round = round.x, elo, elo_new)
    
    zr3 <- l %>% 
      filter(year == y_init, round == 'r1r3') %>% 
      left_join(select(zr2, c(player, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round = round.x, elo, elo_new)
    
    zr4 <- l %>% 
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
  next_year_r1 <- l %>% 
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
r1_1935 <- l %>% 
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

elo_1935_r2 <- l %>% 
  filter(year == '1935', round == 'r1r2') %>% 
  left_join(select(elo_1935_r1, c(player, elo = elo_new)), by = c("player", "year")) %>% 
  add_tally() %>% 
  mutate(
    expected_score = 1/(10-((elo-1500)/400+1)),
    elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
  ) %>% 
  select(player, year, round = round.x, elo, elo_new)

elo_1935_r3 <- l %>% 
  filter(year == '1935', round == 'r1r3') %>% 
  left_join(select(elo_1935_r2, c(player, elo = elo_new)), by = c("player", "year")) %>% 
  add_tally() %>% 
  mutate(
    expected_score = 1/(10-((elo-1500)/400+1)),
    elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
  ) %>% 
  select(player, year, round = round.x, elo, elo_new)

elo_1935_r4 <- l %>% 
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
  
  l %>% 
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

m <- l %>% 
  filter(year == '1935', round == 'r1') %>% 
  left_join(p, by = c("player", "year", "round", "z_round"))
n <- elo_1934 %>% filter(year == '1934', round == 'r1r4')


n %>%
  ungroup() %>% 
  select(-year, -round) %>% 
  right_join(m, by = c("player", "elo"))








# Function to get the most recent elo score to update it
function(y, r, prev_y, prev_r) {
  l %>% 
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
  

