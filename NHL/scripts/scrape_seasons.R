# Scraping from 2001 to 2018
source('NHL/functions/scrape_hockeyr.R')

# Scrape each year in a foreach
for(yyyy in 2014:2018) {
  
  cat(paste0(yyyy,"\n"))
  Sys.sleep(1)
  
  schedule_df <- nhl.get_annual_schedule(yyyy)
  
  cat("\nGot schedule and links to boxscores.\nScraping boxscores...")
  # Got schedule & boxscore links.
  # Scraping boxscores...
  
  enhanced_schedule_df <- nhl.enhance_schedule(schedule_df)
  
  save(enhanced_schedule_df, file = paste0("NHL/data/es_", yyyy, ".Rdata"))
  
}

