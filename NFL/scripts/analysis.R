# Analysis

# Packages:
require(tidyverse); require(magrittr)

# Load data
load("NFL/data/merged_2001_2018.Rdata")

# Add "local time" column - where the locale is the team's locale - not where the game is being played. Here, we want to know the game start time in the timezone of the team's fanbase.

all_team_games %>%
  select(team) %>%
  arrange(team) %>%
  distinct() %>%
  pull() -> teams

team_et_offset <- data.frame(team = teams,
                             offset = c(
                               -2, # Arizona Cardinals
                               0, # Atlanta Falcons
                               0, # Baltimore Ravens
                               0, # Buffalo Bills
                               0, # Carolina Panthers
                               -1, # Chicago Bears
                               0, # Cincinnati Bengals
                               0, # Cleveland Browns
                               -1, # Dallas Cowboys
                               -2, # Denver Broncos
                               0, # Detroit Lions
                               -1, # Green Bay Packers,
                               -1, # Houston Texans
                               0, # Indianapolis Colts
                               0, # Jacksonville Jaguars
                               -1, # Kansas City Chiefs
                               -3, # Los Angeles Chargers
                               -3, # Los Angeles Rams
                               0, # Miami Dolphins
                               -1, # Minnesota Vikings
                               0, # New England Patriots
                               -1, # New Orleans Saints
                               0, # New York Giants
                               0, # New York Jets
                               -3, # Oakland Raiders
                               0, # Philadelphia Eagles
                               0, # Pittsburgh Steelres
                               -3, # San Diego Chargers
                               -3, # San Francisco 49ers
                               -3, # Seattle Seahawks
                               -1, # St. Louis Rams
                               0, # Tampa Bay Buccaneers
                               -1, # Tennessee Titans
                               0 # Washington Redskins
                             ), stringsAsFactors = FALSE)

# Merge
all_team_games %<>%
  left_join(team_et_offset, by = "team")

# Calculating game end times
all_team_games %<>%
  # Calculate end minute first - to see if it is above 60.
  mutate(end_minute_pre = start_minute + length_minutes,
         # If so, we must add an hour to the end hour column.
         add_hour = end_minute_pre >= 60,
         # Calculate the true end minute by taking the modulo of 60.
         end_minute = end_minute_pre %% 60,
         # Calculate the true end hour
         end_hour = start_hour + length_hours + ifelse(add_hour, 1, 0)) %>%
  # Calculate the start and end hours in terms of the team's time zone.
  mutate(team_start_hour = start_hour + offset,
         team_end_hour = end_hour + offset) %>%
  # Rearrange columns
  select(week:start_minute,
         -time,
         -link,
         -pm,
         length_hours, length_minutes,
         end_hour, end_minute,
         team_start_hour,
         team_end_hour)