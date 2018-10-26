# Scraping NFL game results from pro-football-reference.com
# 1970 merger - Week 6, 2018

# Dependencies:
require(tidyverse); require(rvest); require(foreach); 
require(magrittr); require(pbapply);

# Helpful functions
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}

# Scraping functions
nfl.get_annual_schedule <- function(yyyy){
  
  # Description
  # Scrapes the schedule of all games played in a single NFL season from pro-football-reference.com's "YYYY NFL Weekly Schedule" Example page: https://www.pro-football-reference.com/years/2018/games.htm
  # Returns a data.frame of the Week,	Day, Date, Time, Winner/tie, Loser/tie, PtsW, PtsL, YdsW, TOW, YdsL, TOL...
  # ... and links to the boxscore for each game.
  
  url <- paste0("https://www.pro-football-reference.com/years/", yyyy, "/games.htm")
  
  out <- tryCatch({
    
  page <- read_html(url)
  
  # Get schedule table
  
  page %>%
    html_nodes("table.sortable.stats_table") %>%
    html_table() -> schedule_list
  
  schedule_df <- schedule_list[[1]]
  colnames(schedule_df) <- c("week", "day", "date", "time", "winner", "is_away", "loser", "link", "ptsW", "ptsL", "ydsW", "toW", "ydsL", "toL")
  
  schedule_df %<>%
    filter(!(week %in% c("Week", ""))) %>%
    mutate(is_away = is_away != "") %>%
    mutate_at(vars(ptsW:toL), funs(nabs))
  
  # Get boxscore links
  
  page %>% 
    html_nodes("a") %>%
    html_attr("href") -> all_links
  
  boxscore_links <- all_links[grepl("boxscores/[0-9]+", all_links)]
  
  schedule_df$link <- boxscore_links
  
  schedule_df %>% 
    mutate(date = as.Date(str_extract(schedule_df$link, "[0-9]+"), format = "%Y%m%d0")) %>%
    filter(date < Sys.Date()) -> schedule_df

  return(schedule_df)
  },
  error = function(cond){
    
    message(paste("^ This year's URL does not seem to exist:",url))
    closeAllConnections()
    
    })
  
  return(out)
  
}

nfl.get_times <- function(link_ending){
  
  url <- paste0("https://www.pro-football-reference.com", link_ending)
  
  out <- tryCatch({
  
  times <- read_html(url) %>%
    html_nodes("#content .scorebox_meta") %>%
    html_text() %>%
    str_extract_all("[0-9]+:[0-9]+.+") %>%
    unlist()
  
  if (length(times) == 0) {
    return(c(NA, NA))
  } else if (length(times) == 1) {
    return(c(times, NA))
  }
  
  return(times)
  },
  
  error = function(cond) {
    
    message(paste("^ This boxscore URL does not seem to exist:", url))
    return(c(NA, NA))
    closeAllConnections()
    
  })
  
  return(out)
  
}


nfl.enhance_schedule <- function(schedule_df){
  
  all_times <- pblapply(schedule_df$link, nfl.get_times)
  
  all_times_df <- data.frame(matrix(unlist(all_times), nrow = length(all_times), byrow = T), stringsAsFactors = FALSE)
  colnames(all_times_df) <- c("start_time", "game_length")

  enhanced_schedule_df <- bind_cols(schedule_df, all_times_df) %>%
    mutate(pm = grepl("pm", start_time, ignore.case = T),
           start_time = gsub("[A-z]{2}", "", start_time)) %>%
    separate(start_time, into = c("start_hour", "start_minute"), sep = ":", convert = TRUE) %>%
    mutate(start_hour = start_hour + ifelse(pm & start_hour < 12, 12, 0)) %>%
    separate(game_length, into = c("length_hours", "length_minutes"), sep = ":", convert = TRUE)
  
  return(enhanced_schedule_df)
  
}