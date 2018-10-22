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

# Reformat so each row corresponds to one team, with opponent ("opp") and opponent stats ("opp_pts") - not "winner", "loser" and "ptsW"/"ptsL" etc
winners <- all_es_df %>%
  select(week:time, # unchanged
         team = winner,
         is_away, # unchanged
         opp = loser,
         link, # unchanged
         pts = ptsW,
         opp_pts = ptsL,
         yds = ydsW,
         opp_yds = ydsL,
         to = toW,
         opp_to = toL,
         start_hour:pm) # unchanged

losers <- all_es_df %>%
  select(week:time, # unchanged
         team = loser,
         is_away, # changed below in mutate (flipped)
         opp = winner,
         link, # unchanged
         pts = ptsL,
         opp_pts = ptsW,
         yds = ydsL,
         opp_yds = ydsW,
         to = toL,
         opp_to = toW,
         start_hour:pm) %>% # unchanged
  mutate(is_away = !is_away)

all_team_games <- 
  bind_rows(winners, losers) %>%
  arrange(date)

save(all_team_games, file = "NFL/data/merged_2001_2018.Rdata")