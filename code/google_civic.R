library(httr)
library(jsonlite)
library(tidyverse)

# run each official polling place through the Google Civic API and see if it retruns the same location
# I have a saved API key for if we need to do this again in Nov

official_locations <- read_csv("~/Downloads/official.csv")

official_locations$address_parsed <- official_locations$PollingPlaceAddress %>% 
  gsub(pattern=" ", replacement = "%20", fixed = TRUE) %>%
  gsub(pattern=",", replacement = "", fixed = TRUE) 

small_address <- official_locations %>% filter(County == "ROCK COUNTY")

output <- data.frame()

for(i in 1:nrow(small_address)){
  Sys.sleep(3)
  print(i)
  a <- small_address$address_parsed[i]
  
  r <- paste0("https://civicinfo.googleapis.com/civicinfo/v2/voterinfo?address=",
  a, "&key=[APIKEY]") %>% GET()
  
  json <- fromJSON(rawToChar(r$content))
  
  if(is.null(json$pollingLocations$address)){
    
  output <- output %>% bind_rows(data.frame(locationName = "error",
                                              line1 = "error",
                                              city = "error",
                                              state = "error",
                                              zip = "error",
                                              PollingPlaceName = small_address$PollingPlaceName[i],
                                              ReportingUnit = small_address$ReportingUnit[i],
                                              County = small_address$County[i],
                                              Muni = small_address$Muni[i]))
    
    next;
  }
  
  output <- output %>% bind_rows(data.frame(json$pollingLocations$address,
                                            PollingPlaceName = small_address$PollingPlaceName[i],
                                            ReportingUnit = small_address$ReportingUnit[i],
                                            County = small_address$County[i],
                                            Muni = small_address$Muni[i]))
}


write_csv(output, "~/Desktop/rock_county.csv")