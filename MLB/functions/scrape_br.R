# Scrape basketball-reference.com
require(tidyverse); require(rvest); require(pbapply)


# Helpful functions
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}

# Get annual schedule
# Baseball-reference not organized like the other leagues with monthly/yearly schedule tables.
# Have to first get the links, scrape each, and build a schedule table myself.

## PROBLEM - postseason games (at least in 2018) still have "preview" links, not boxscores. Need to convert to box score links.

mlb.get_boxscore_links <- function(year){
  
  url <- paste0("https://www.baseball-reference.com/leagues/MLB/", year, "-schedule.shtml")
  
  out <- tryCatch({
    page <- read_html(url) 
    
    page %>% 
      html_nodes("a") %>%
      html_attr("href") -> all_links
    
    # Boxscore link looks like:
    # https://www.baseball-reference.com/boxes/MIA/MIA201805270.shtml
    
    boxscore_links <- all_links[grepl("/boxes/[A-z]+/[A-z]+[0-9]+", all_links)]
    
    return(boxscore_links)
  },
  error = function(cond){
    message(paste("^ This year's URL does not seem to exist:",url))
    closeAllConnections()
  })
  
  return(out)
  
}

mlb.get_boxscore_details <- function(link_ending, wait = TRUE){
  
  if(wait){
    wait_time <- runif(1, 0, 3) # generate random time (between 0 & 3 seconds) to sleep before scraping the page
    Sys.sleep(wait_time)
  }
  
  url <- paste0("https://www.baseball-reference.com", link_ending)
  
  out <- tryCatch({
    
    box <- read_html(url)
    
    box %>%
      html_nodes("#content .scorebox") -> scorebox
    # "scorebox" has all we need to get scores, team names, time game started, time of game
    
    scorebox %>%
      html_nodes("a") %>%
      html_attr("href") -> scorebox_links
    
    teams <- scorebox_links[grepl("/teams/", scorebox_links)] %>%
      str_extract("[A-Z]{3}")
    # First element is the visiting team.
    # Second element is the home team.
    
    runs <- scorebox %>%
      html_nodes(".score") %>%
      html_text()
    # First element is the visiting team's score.
    # Second is the home team's score.
    
    scorebox %>%
      html_text() -> scorebox_text
    
    start_time_cols <- scorebox_text %>%
      str_extract("[0-9]+:[0-9]{2}[[:space:]][A-z]\\.[A-z]\\.[[:space:]][A-z]{2}") %>%
      str_split("[[:space:]]") %>%
      unlist()
    # First element is the time (12-hour clock).
    # Second element is AM/PM.
    # Third element is the time zone (ET/CT/PT).
    
    both_times <- scorebox_text %>%
      str_extract_all("[0-9]+:[0-9]{2}") %>%
      unlist()
    game_length <- both_times[2]
    # ^ There's a better way to do this.
    
    # #### Linescore table ####
    # Implement eventually, but don't really need it right now. 
    # Would be nice to have the number of innings that the game went.
    # box %>% 
    #   html_nodes(".linescore_wrap table") %>%
    #   html_table() -> test
    
    return(c(link_ending, teams, runs, start_time_cols, game_length))
    
  }, error = function(cond){
    
    message(paste("Boxscore does not seem to exist:", url))
    closeAllConnections()
    
  })
  
  return(out)
  
}

mlb.get_annual_schedule <- function(year){
  
  year_links <- mlb.get_boxscore_links(year)
  
  game_details <- pblapply(year_links, mlb.get_boxscore_details)
  
  schedule_df <- data.frame(matrix(unlist(game_details), nrow = length(game_details), byrow = T), stringsAsFactors = FALSE)
  
  colnames(schedule_df) <- c("link", "visitor", "home", "visitor_score", "home_score", "start_time", "pm", "time_zone", "game_length")
  
  return(schedule_df)
  
}

mlb.enhance_schedule <- function(schedule_df){
  
  schedule_df %>%
    mutate(pm = pm %in% c("p.m.", "pm", "PM", "P.M."),
           visitor_score = nabs(visitor_score),
           home_score = nabs(home_score)) %>%
    separate(start_time, into = c("start_hour", "start_minute"), sep = ":", convert = TRUE) %>%
    mutate(start_hour = start_hour + ifelse(pm & start_hour < 12, 12, 0)) %>%
    separate(game_length, into = c("length_hours", "length_minutes"), sep = ":", convert = TRUE) 
  
  return(enhanced_schedule_df)
  
}