# Scrape basketball-reference.com
require(tidyverse); require(rvest)

# Helpful functions
nabs <- function(x) {
  
  ## Description
  # nabs() returns x after first converting it to class numeric via character
  # Its primary use is converting objects of class factor to numeric
  # It also provides a more concise wrapper for standard numeric conversion
  
  return(as.numeric(as.character(x)))
  
}

# Games listed by month:
get_monthly_schedule <- function(month, year){
  
  url = paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_games-", month, ".html")
  
  page = read_html(url)
  
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
  
}

get_annual_schedule <- function(year){
  
  all_months <- list() #create an empty list
  
  for(m in c("October", "November", "December", "January", "February", "March", "April", "May", "June")){
    
    cat(paste0(m, " ", year, "\n"))
    
    new_month = get_monthly_schedule(tolower(m), year)
    
    all_months[[m]] = new_month
    
  }
  
  annual_schedule <- do.call("rbind", all_months) %>%
    data.frame(row.names = NULL) %>%
    mutate(season = paste0(year-1, year))
  
  return(annual_schedule)
  
}