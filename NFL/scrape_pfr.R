# Scraping NFL game results from pro-football-reference.com
# 1970 merger - Week 6, 2018

# Dependencies:
require(tidyverse); require(rvest); require(foreach);

# Helpful functions
# Courtesy of Manny Elk in his corsica package @manny_hockey:
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}

# Scraping functions


scrape_schedule <- function(){
  
  ## Description
  # Scrapes the raw html tables from pro-football-reference.com's list of games played since 1970 AFL-NFL merger
  # Returns a single data.frame of the collated tables (games_df)
  
# Define scraping variables (URL, number of pages to loop over)
# URL for all games between 1970, Week 6 2018
url <- "https://www.pro-football-reference.com/play-index/tgl_finder.cgi?request=1&match=game&year_min=1970&year_max=2018&game_type=E&game_num_min=0&game_num_max=99&week_num_min=0&week_num_max=99&temperature_gtlt=lt&c5val=1.0&order_by=game_date&offset="
# Data is paginated. "Offset" variable governs the 100 rows displayed on each page. 
# Need to loop over all pages of games played since 1970.
offset_max <- 23000
offset_seq <- seq(0, offset_max, 100)

# Scrape each page in a foreach
games_list <- foreach(offset = offset_seq) %do% {
  
  cat(offset/offset_max, "% complete")
  cat("\n")
  
  Sys.sleep(1)
  
  games_table <- read_html(paste0(url, offset)) %>%
    html_nodes("table.sortable.stats_table") %>%
    html_table()
  return(games_table)
  
}

# rbind the games_list
games_bound <- do.call(Map, c(rbind, games_list))

# Extract the data.frame from the games_bound list
games_df <- games_bound[[1]]
# Define column names
colnames(games_df) <- c("index", "team", "year", "date", "time_est", "time_local", "is_away", "opp", "week", "game_num", "day", "result", "is_ot")

return(games_df)
}


enhance_schedule <- function(games_df){

  
  ## Description
  # Takes a games_df as the input, 
  # Returns a single data.frame of the collated tables (games_df)
  
  
  
games_enhanced <- games_df %>%
  # Filter out header rows
  filter(index != "Rk") %>% 
  # Convert character columns to numeric & logical
  mutate(index = nabs(index),
         year = nabs(year),
         date = as.Date(date),
         is_away = is_away != "",
         week = nabs(week),
         game_num = nabs(game_num),
         is_ot = is_ot != ""
         ) %>%
  # Split and convert time columns to numeric
  separate(time_est, into = c("hour_est", "minute_est"), sep = ":", convert = TRUE) %>%
  separate(time_local, into = c("hour_local", "minute_local"), sep = ":", convert = TRUE) %>%
  # Filter out London games based on time diference
  filter(abs(hour_est-hour_local) < 4) %>%
  # Add features
  mutate(is_win = str_sub(result, 1, 1) == "W")

# Fixes (the time anomaly in Minnesota)
# December 1, 2016 game between MIN & DAL listed as 10:25 CT start. Start was actually 7:25 CT.
games_enhanced$hour_local[games_enhanced$index %in% c(897,898)] <- 7

# September 11, 2017 game between MIN & NOR listed as 9:10 CT start. Start was actually 6:10 CT.
games_enhanced$hour_local[games_enhanced$index %in% c(690,691)] <- 6

# The following 1:00 ET games in Minnesota listed as 3:00 CT start. Start was actually 12:00 CT.
games_enhanced$hour_local[games_enhanced$index %in% c(8, 15, 92, 106,
                                                      171, 180, 210, 223,
                                                      275, 283, 414, 420,
                                                      522, 530, 545, 550,
                                                      604, 608, 641, 653,
                                                      744, 757, 812, 821,
                                                      938, 945, 994, 999)] <- 12

# DAL @ NYG 12-10-2017. 1:00 ET start, listed as 4:25 local time.
games_enhanced$hour_local[games_enhanced$index %in% c(312, 320)] <- 1
games_enhanced$minute_local[games_enhanced$index %in% c(312, 320)] <- 0

return(games_enhanced)

}

#save(games_enhanced, file = "NFL/games_enhanced.Rdata")