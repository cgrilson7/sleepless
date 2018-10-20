# Scraping from 2001 to 2018
source('NFL/functions/scrape_pfr.R')

years <- 2001:2018

# Scrape each year in a foreach
for(yyyy in 2001:2018) {
  
  cat(paste0(yyyy,"\n"))
  Sys.sleep(1)
  
  schedule_df <- scrape_schedule(yyyy)
  
  enhanced_schedule_df <- enhance_schedule(schedule_df)
  
  save(enhanced_schedule_df, file = paste0("sked_",y))
  
}