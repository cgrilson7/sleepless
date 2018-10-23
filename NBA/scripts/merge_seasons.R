# Merge seasons and analyze.
require(tidyverse);

# Create empty list to hold all enhanced_schedules
es_list <- list()

# Loop over 2001-2018 seasons, loading in enhanced_schedule and adding to list
for(yyyy in 2001:2018){
  
  load(paste0("NBA/data/es_", yyyy, ".Rdata"))
  
  es_list[[yyyy]] <- enhanced_schedule_df

}

# Bind enhanced_schedules together
all_es_df <- do.call("rbind", es_list) %>%
  data.frame(row.names = NULL)

# Reformat so each row corresponds to one team, with opponent ("opp") and opponent stats ("opp_pts") - not "visitor", "home" and "pts_visitor"/"pts_home" etc
visitors <- all_es_df %>%
  mutate(is_away = TRUE) %>%
  select(date, time, # unchanged
         team = visitor,
         is_away,
         opp = home,
         link, # unchanged
         pts = pts_visitor,
         opp_pts = pts_home,
         ot,
         att,
         start_hour:pm) # unchanged

homes <- all_es_df %>%
  mutate(is_away = FALSE) %>%
  select(date, time, # unchanged
         team = home,
         is_away,
         opp = visitor,
         link, # unchanged
         pts = pts_home,
         ot,
         att,
         start_hour:pm) # unchanged

all_team_games <- 
  bind_rows(visitors, homes) %>%
  arrange(date) %>%
  mutate(index = 1:nrow(all_team_games))

save(all_team_games, file = "NBA/data/merged_2001_2018.Rdata")