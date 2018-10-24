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

# Games listed by month:
nba.get_monthly_schedule <- function(month, year){
  
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", month, ".html")
  
  out <- tryCatch({
    
  page <- read_html(url)
  
  page %>%
    html_nodes("table") %>%
    html_table() -> schedule_list
  
  schedule_df <- schedule_list[[1]]
  colnames(schedule_df) <- c("date", "time", "visitor", "pts_visitor", "home", "pts_home", "link", "ot", "att", "notes")
  
  schedule_df %<>%
    # Every April - when playoffs begin, there is an extra row in the schedule table - denoted "Playoffs"
    filter(date != "Playoffs") %>%
    mutate(pts_visitor = nabs(pts_visitor),
           pts_home = nabs(pts_home),
           ot = ot == "OT",
           att = nabs(gsub(",", "", att))) %>%
    select(-notes)
  
  # Get boxscore links
  page %>% 
    html_nodes("a") %>%
    html_attr("href") -> all_links
  
  # Get links that look like "/boxscores/YYYYMMDD0TEAM.html" - these are the game links we want
  boxscore_links <- all_links[grepl("boxscores/[0-9]+", all_links)]
  
  # Replace link column in schedule_df
  schedule_df$link <- boxscore_links
  
  # Modify date column (based on date in boxscore link)
  schedule_df %>% 
    mutate(date = as.Date(str_extract(schedule_df$link, "[0-9]+"), format = "%Y%m%d0")) %>%
    filter(date < Sys.Date()) -> schedule_df
  
  return(schedule_df)
  
  }, error = function(cond){
    
    message(paste("^ This month's URL does not seem to exist:", url))
    closeAllConnections()
    
  })
  
  return(out)
  
}

nba.get_annual_schedule <- function(year){
  
  all_months <- list() # create an empty list to hold each of the months scraped
  
  cat(paste0("Scraping ", year-1, "-", year, " schedule...\n"))
  
  for(m in c("October", "November", "December", "January", "February", "March", "April", "May", "June")){
    
    cat(m, "\n")
    
    new_month <- nba.get_monthly_schedule(tolower(m), year)
    
    all_months[[m]] <- new_month
    
  }
  
  annual_schedule <- do.call("rbind", all_months) %>%
    data.frame(row.names = NULL) %>%
    mutate(season = paste0(year-1, year))
  
  return(annual_schedule)
  
}

nba.get_times <- function(link_ending, wait = TRUE){
  
  if(wait){
    wait_time <- runif(1, 0, 2) 
    # Generate random time (between 0 & 2 seconds) to sleep before scraping the page
    Sys.sleep(wait_time)
  }
  
  url <- paste0("https://www.basketball-reference.com", link_ending)
  
  out <- tryCatch({
    
    box <- read_html(url)
    
    box %>%
      html_nodes("#content .scorebox") %>%
      html_text() %>%
      str_extract("[0-9]+:[0-9]{2} [A-z]+") -> game_start
    
    box %>%
      html_nodes("#content") %>%
      html_text() %>%
      str_extract("[0-9]+:[0-9]{2}\\n") %>%
      gsub("\\n", "",.) -> game_length
    
    return(c(game_start, game_length))
    
  }, error = function(cond){
    
    message(paste("^ This boxscore does not seem to exist:", url))
    closeAllConnections()
    
    return(c(NA, NA))
    
  })
  
  return(out)
 
}

nba.enhance_schedule <- function(schedule_df){
  
  all_times <- pblapply(schedule_df$link, nba.get_times)
  
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