# Scraping from 2001 to 2018
source('NBA/functions/scrape_bbr.R')

# Scrape each year in a foreach
for(yyyy in 2005:2018) {
  
  cat(paste0(yyyy,"\n"))
  Sys.sleep(1)
  
  schedule_df <- nba.get_annual_schedule(yyyy)
  
  cat("\nGot schedule and links to boxscores.\nScraping boxscores...")
  # Got schedule & boxscore links.
  # Scraping boxscores...
  
  enhanced_schedule_df <- nba.enhance_schedule(schedule_df)
  
  save(enhanced_schedule_df, file = paste0("NBA/data/es_", yyyy, ".Rdata"))
  
}