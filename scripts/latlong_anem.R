# latlong_anem ####
#' a function to find the lat long of a given fish
#' @export
#' @name latlong_anem
#' @author Michelle Stuart
#' @param x = anem_id - the id of the anem you are trying to get lat long for
#' @examples 
#' where <- latlong_anem(2268)

latlong_anem <- function(x){
  # connect to the database
  library(dplyr)
  library(tidyr)
  source("scripts/conleyte.R")
  
  # find the anem_table_id for the sample
  leyte <- conleyte()
  
  
  fish <- left_join(anem, dive, by = "anem_table_id")

  # find the date info and gps unit for this fish
  date <- leyte %>% 
    tbl("diveinfo") %>% 
    select(dive_table_id, date, gps) %>% 
    collect() %>% 
    filter(dive_table_id %in% fish$dive_table_id)
  
  fish <- left_join(fish, date, by = "dive_table_id") %>% 
    separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
    mutate(gpx_hour = as.numeric(hour) - 8)
  

  
  # find the lat long for this fish
  lat <- leyte %>%
    tbl("GPX") %>%
    select(lat, lon, time, unit) %>%
    collect(n = Inf) %>% 
    separate(time, into = c("date", "time"), sep = " ") %>% 
    filter(date %in% fish$date) %>% 
    separate(time, into = c("hour", "minute", "second"), sep = ":") %>% 
    filter(as.numeric(hour) == fish$gpx_hour & as.numeric(minute) == fish$minute)
  
  x <- (which(duplicated(lat$lat)) > 0 & which(duplicated(lat$lon) > 0))
  if(length(x) == 0){
    fish$lat <- round(summarise(lat, mean(as.numeric(lat))), digits = 5)
    fish$lon <- round(summarise(lat, mean(as.numeric(lon))), digits = 5)
  }else{
    fish$lat <- lat$lat[which(duplicated(lat$lat))]
    fish$lon <- lat$lon[which(duplicated(lat$lon))]
  }
  fish <- fish %>% 
    select(sample_id, lat, lon)
  
  return(fish)
  
}
