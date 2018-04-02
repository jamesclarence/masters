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
# get_elo_init(1934, 'r1')

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

# y_init = initial year (1934)
# r_init = initial round (r1)
# l = "leaderboard_with_z_score_for_elo_calculating.csv" 

first_year_elo <- 
  function(y_init, r_init) {
    init <- get_elo_init(y_init, r_init)
    
    zr2 <- l %>% 
      filter(year == y_init, round == 'r1r2') %>% 
      left_join(select(init, c(player, year, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round, elo, elo_new)
    
    zr3 <- l %>% 
      filter(year == y_init, round == 'r1r3') %>% 
      left_join(select(zr2, c(player, year, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round, elo, elo_new)
    
    zr4 <- l %>% 
      filter(year == y_init, round == 'r1r4') %>% 
      left_join(select(zr3, c(player, year, elo = elo_new)), by = c("player", "year")) %>%
      add_tally() %>% 
      mutate(
        expected_score = 1/(10-((elo-1500)/400+1)),
        elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
      ) %>% 
      select(player, year, round, elo, elo_new)
    
    first_year_bind <- bind_rows(init, zr2, zr3, zr4)
    
    df <<- first_year_bind
  }



# Get data and calculate ELO after 1934 -----------------------------------

# get_next_year_of_data(next_year)

# next_year = the year you want to calculate ELO ('1935')
# l = "leaderboard_with_z_score_for_elo_calculating.csv"

# Function to get the next year of data
get_next_year_of_data <- function(next_year) {
  next_year_r1 <- l %>% 
    filter(year == next_year, round == 'r1')
  
  # Select latest round of player who has played
  max_round <- df %>% 
    group_by(player, year) %>% 
    summarise(max_round = max(round)) %>% 
    mutate(rank = dense_rank(desc(year))) %>% 
    filter(rank == 1)
  
  # Get ELO rating for latest round
  latest_elo <- max_round %>% 
    left_join(df, by = c("player", "year", "max_round" = "round"))
  
  # Combine latest_elo with next_year_r1 data
  # Players not in 1934 - add 1500 elo
  new_player_this_year <- next_year_r1 %>%
    select(player) %>% 
    left_join(latest_elo, by = "player") %>% 
    filter(is.na(max_round) == T) %>% 
    mutate(elo = replace_na(elo_new, as.numeric("1500"))) %>% 
    select(year,
           round = max_round,
           player, 
           elo)
  
  # Players who have played before next_year
  played_before_this_year <- next_year_r1 %>%
    select(player) %>% 
    left_join(latest_elo, by = "player") %>% 
    filter(is.na(max_round) == F) %>% 
    select(year,
           round = max_round,
           player,
           elo = elo_new)
  
  # Combine players not in next_year and in next_year
  year_player_list <- rbind(new_player_this_year, played_before_this_year)
  
  # Add year and 'r1' to data frame
  year_player_list_for_match <- year_player_list %>% 
    mutate(year_for_calc = as.integer(next_year),
           round_for_calc = 'r1') %>%
    select(year = year_for_calc,
           round = round_for_calc,
           player,
           elo)
  
  # Get z-score in next_year and r1
  r1_for_calculating_elo <- next_year_r1 %>% 
    left_join(year_player_list_for_match, by = c("player", "year", "round"))

  r1_for_calculating_elo
}



# Calculate ELO from get_next_year_of_data --------------------------------

# y = year we are calculating

calculate_elo <- function(y) {
  # Get data frame to start calculating
  r1_for_calculating_elo <- get_next_year_of_data(y)
  
  # Calculate ELO
  zr1 <- l %>% 
    filter(year == y, round == 'r1') %>% 
    inner_join(r1_for_calculating_elo, by = c("player", "year", "round", "z_round")) %>% 
    add_tally() %>% 
    mutate(elo_new = (1500 + 20*((-1)*(z_round - (2 * ((1/n)/n)))))) %>% 
    select(player, year, round, elo, elo_new)
  
  zr2 <- l %>% 
    filter(year == y, round == 'r1r2') %>% 
    left_join(select(zr1, c(player, year, elo = elo_new)), by = c("player", "year")) %>%
    add_tally() %>% 
    mutate(
      expected_score = 1/(10-((elo-1500)/400+1)),
      elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
    ) %>% 
    select(player, year, round, elo, elo_new)
  
  zr3 <- l %>% 
    filter(year == y, round == 'r1r3') %>% 
    left_join(select(zr2, c(player, year, elo = elo_new)), by = c("player", "year")) %>%
    add_tally() %>% 
    mutate(
      expected_score = 1/(10-((elo-1500)/400+1)),
      elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
    ) %>% 
    select(player, year, round, elo, elo_new)
  
  zr4 <- l %>% 
    filter(year == y, round == 'r1r4') %>% 
    left_join(select(zr3, c(player, year, elo = elo_new)), by = c("player", "year")) %>%
    add_tally() %>% 
    mutate(
      expected_score = 1/(10-((elo-1500)/400+1)),
      elo_new = elo + 20*((-1)*(z_round - (2*(expected_score/n))))
    ) %>% 
    select(player, year, round, elo, elo_new)
  
  year_bind <- bind_rows(zr1, zr2, zr3, zr4)
  
  df <<- rbind(df, year_bind)
}



# Notes -------------------------------------------------------------------



# - After matching the name from 1934 and the new year, get most recent ELO rating for each participant in 1934
# - If matching on name doesn't work, make new players' ELO 1500
# - Run ELO ratings for the new year using new data frame
# - Combine 1934 with new year in new data frame
# - Repeat process with new year


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

