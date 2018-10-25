# Scraping from 2001-2018
source('MLB/functions/scrape_br.R')

# Scrape each year in a foreach
for(yyyy in 2001:2018) {
  
  cat(paste0(yyyy,"\n"))
  Sys.sleep(1)
  
  schedule_df <- mlb.get_annual_schedule(yyyy, wait = FALSE)
  
  enhanced_schedule_df <- mlb.enhance_schedule(schedule_df)
  
  rm(schedule_df)
  
  save(enhanced_schedule_df, file = paste0("MLB/data/es_", yyyy, ".Rdata"))
  
  rm(enhanced_schedule_df)
  
}