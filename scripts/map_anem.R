# map_anem ####
#' create a csv of anem locations and ids that can be imported into QGIS
#' @export
#' @name map_anem
#' @author Michelle Stuart
#' @param x = year
#' @examples 
#' dat <- map_anem(2016)

map_anem <- function(x){
  library(dplyr)
  library(tidyr)
  library(lubridate)
  source("scripts/con_leyte.R")
  source("scripts/date_dive.R")

  begin <- paste(x, "01", "01", sep = "-")
  end <- paste(x, "12", "31", sep = "-")
  
  leyte <- conleyte()
  
  # pull dives for date range
  dive <- date_dive(begin, end)
  
  # reduce number of columns
  dive <- dive %>% 
    select(dive_table_id, date, site, gps) %>% 
    rename(unit = gps)
  
  # find anemones that match those dives and dates
  anem <- leyte %>%
    tbl("anemones") %>%
    filter(dive_table_id %in% dive$dive_table_id & !is.na(anem_id)) %>%
    select(dive_table_id, anem_id, anem_obs, obs_time) %>%
    collect() %>% 
    separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
    mutate(gpx_hour = as.numeric(hour) - 8) %>% 
    mutate(minute = as.numeric(minute))
    

  anem <- left_join(anem, dive, by = "dive_table_id")
  rm(dive)
  
  # find the lat long for this anem
  lat <- leyte %>%
    tbl("GPX") %>%
    mutate(date = date(time)) %>%
    filter(date %in% anem$date) %>% 
    mutate(gpx_hour = hour(time)) %>% 
    mutate(minute = minute(time)) %>% 
    mutate(second = second(time)) %>% 
    select(-time, -second) %>% 
    collect()
  
  sum_lat <- lat %>%
    group_by(unit, date, gpx_hour, minute) %>% 
    summarise(lat = mean(as.numeric(lat)),
      lon = mean(as.numeric(lon)))
  
  anem <- left_join(anem, sum_lat, by = c("unit", "date", "gpx_hour", "minute"))


# Write out for QGIS (has column headers)
out <- anem %>%
  select(lat, lon, date, site, anem_id, anem_obs)

name <- paste(Sys.Date(), "_GPSSurvey_anemlatlong_", x, "_forQGIS.csv", sep = "")
write.table(out, file = name, col.names=T, sep=',', row.names=F, quote=T)

return(anem)
}


  
  


