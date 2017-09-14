# latlong_fish ####
#' a function to find the lat long of a given fish
#' @export
#' @name latlong_fish
#' @author Michelle Stuart
#' @param x = sample_id - the id of the fish you are trying to get the lat long for
#' @examples 
#' where <- latlong_fish("APCL12_093")

latlong_fish <- function(x){
  # connect to the database
  library(dplyr)
  library(tidyr)
  leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  # find the anem_table_id for the sample
  anem <- leyte %>% 
    tbl("clownfish") %>%
    select(sample_id, anem_table_id) %>% 
    filter(sample_id == x) %>% 
    collect()
  
  # find the dive info and time for this fish
  dive <- leyte %>% 
    tbl("anemones") %>% 
    select(anem_table_id, obs_time, dive_table_id) %>% 
    collect() %>% 
    filter(anem_table_id %in% anem$anem_table_id)

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
