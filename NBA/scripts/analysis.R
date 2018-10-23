# Analysis

# Packages:
require(tidyverse); require(magrittr);
require(lubridate); require(scales)

# Load data
load("NBA/data/merged_2001_2018.Rdata")

# Hasn't been a game before noon. Fix the games starting in the "am".


# Add "local time" column - where the locale is the team's locale - not where the game is being played. Here, we want to know the game start time in the timezone of the team's fanbase.

all_team_games %>%
  select(team) %>%
  arrange(team) %>%
  distinct() %>%
  pull() -> teams

# Below copied from NFL/scripts/analysis.R
# 
# team_et_offset <- data.frame(team = teams,
#                              offset = c(
#                                -2, # Arizona Cardinals
#                                0, # Atlanta Falcons
#                                0, # Baltimore Ravens
#                                0, # Buffalo Bills
#                                0, # Carolina Panthers
#                                -1, # Chicago Bears
#                                0, # Cincinnati Bengals
#                                0, # Cleveland Browns
#                                -1, # Dallas Cowboys
#                                -2, # Denver Broncos
#                                0, # Detroit Lions
#                                -1, # Green Bay Packers,
#                                -1, # Houston Texans
#                                0, # Indianapolis Colts
#                                0, # Jacksonville Jaguars
#                                -1, # Kansas City Chiefs
#                                -3, # Los Angeles Chargers
#                                -3, # Los Angeles Rams
#                                0, # Miami Dolphins
#                                -1, # Minnesota Vikings
#                                0, # New England Patriots
#                                -1, # New Orleans Saints
#                                0, # New York Giants
#                                0, # New York Jets
#                                -3, # Oakland Raiders
#                                0, # Philadelphia Eagles
#                                0, # Pittsburgh Steelres
#                                -3, # San Diego Chargers
#                                -3, # San Francisco 49ers
#                                -3, # Seattle Seahawks
#                                -1, # St. Louis Rams
#                                0, # Tampa Bay Buccaneers
#                                -1, # Tennessee Titans
#                                0 # Washington Redskins
#                              ), stringsAsFactors = FALSE)
# 
# # Merge
# all_team_games %<>%
#   left_join(team_et_offset, by = "team")
# 
# # Calculating game end times
# all_team_games %<>%
#   # Calculate end minute first - to see if it is above 60.
#   mutate(end_minute_pre = start_minute + length_minutes,
#          # If so, we must add an hour to the end hour column.
#          add_hour = end_minute_pre >= 60,
#          # Calculate the true end minute by taking the modulo of 60.
#          end_minute = end_minute_pre %% 60,
#          # Calculate the true end hour
#          end_hour = start_hour + length_hours + ifelse(add_hour, 1, 0)) %>%
#   # Calculate the start and end hours in terms of the team's time zone.
#   mutate(team_start_hour = start_hour + offset,
#          team_end_hour = end_hour + offset) %>%
#   # Rearrange columns
#   select(week:start_minute,
#          -time,
#          -link,
#          -pm,
#          length_hours, length_minutes,
#          end_hour, end_minute,
#          team_start_hour,
#          team_end_hour) %>%
#   # Add decimal time columns
#   mutate(start_decimal = start_hour + start_minute/60,
#          end_decimal = end_hour + end_minute/60,
#          team_start_decimal = team_start_hour + start_minute/60,
#          team_end_decimal = team_end_hour + end_minute/60)
# 
# # Analysis
# 
# # What are some of the games that went the latest?
# all_team_games %>% 
#   filter(# excluding Thanksgiving games (weeks 47, 48 of the year)
#          !(day == "Thu" & lubridate::week(date) %in% c(47,48)),
#          # exclude Christmas Eve, Christmas Day, New Years Eve games
#          !(lubridate::month(date) == 12 & lubridate::day(date) %in% c(24, 25, 31))) %>%
#   arrange(-team_end_hour, -end_minute) %>%
#   head(10) %>%
#   select(day, date, team, opp, team_end_hour, end_minute)
# 
# # What teams had games starting late?
# # Games grouped by team and start hour
# all_team_games %>% 
#   filter(# excluding Thanksgiving games (weeks 47, 48 of the year)
#     !(day == "Thu" & lubridate::week(date) %in% c(47,48)),
#     # exclude Christmas Eve, Christmas Day, New Years Eve games
#     !(lubridate::month(date) == 12 & lubridate::day(date) %in% c(24, 25, 31))) %>%
#   group_by(team, team_start_hour) %>%
#   summarize(games = n()) -> teams_by_start_hour
# 
# # What teams had games ending late?
# # Games grouped by team and end hour
# all_team_games %>% 
#   filter(# excluding Thanksgiving games (weeks 47, 48 of the year)
#     !(day == "Thu" & lubridate::week(date) %in% c(47,48)),
#     # exclude Christmas Eve, Christmas Day, New Years Eve games
#     !(lubridate::month(date) == 12 & lubridate::day(date) %in% c(24, 25, 31))) %>%
#   group_by(team, team_end_hour) %>%
#   summarize(games = n()) -> teams_by_end_hour
# 
# # Giants Cowboys (latest ending game): write-up https://www.bigblueinteractive.com/2003/09/18/game-review-dallas-cowboys-new-york-giants-september-15-2003/
# 
# # Let's take the inverse: what games have impacted productivity because they are weekday games starting early, causing people to watch on the job or leave work? (These will be mostly west coast teams.)
# all_team_games %>%
#   filter(!(day %in% c("Sat", "Sun")),
#          # excluding Thanksgiving games (weeks 47, 48 of the year)
#          !(day == "Thu" & lubridate::week(date) %in% c(47,48)),
#          # exclude Christmas Eve, Christmas Day, New Years Eve games
#          !(lubridate::month(date) == 12 & lubridate::day(date) %in% c(24, 25, 31))) %>%
#   arrange(team_start_hour, start_minute) %>%
#   head(10) %>%
#   select(day, date, team, is_away, opp, team_start_hour, start_minute)
# 
# # Plotting
# # Density function of team vs. league - start times
# all_team_games %>%
#   filter(date > "2002-08-01") %>%
#   select(team, team_start_decimal, team_end_decimal) %>%
#   ggplot(aes(x = team_start_decimal, color = (team == "New England Patriots"))) +
#   geom_density()
# 
# # Density function of team vs. league - end times
# all_team_games %>%
#   filter(date > "2002-08-01") %>%
#   select(team, team_start_decimal, team_end_decimal) %>%
#   ggplot(aes(x = team_end_decimal, color = (team == "New England Patriots"))) +
#   geom_density()