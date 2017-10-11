# one source for all of the helper functions to work with Leyte database and clownfish data
library(dplyr)

# read_db ####
#' views all of the fish recaptured at a given site
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- read_Db("Leyte")

read_db <- function(db_name){
  
  db <- src_mysql(dbname = db_name, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}

# write_db ####
#' access db with intent to change it
#' @export
#' @name write_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- write_db("Leyte")

write_db <- function(db_name){
  library(RMySQL)
  db <- dbConnect(MySQL(), dbname = db_name, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}

# site_recap ####
#' for a given site, will list all recaptured fish
#' @export
#' @name site_recap
#' @author Michelle Stuart
#' @param x = site for which to get data
#' @examples 
#' info <- site_recap("Haina")

site_recap <- function(site){
  
  leyte <- read_db("Leyte")
  
  # select all dives at a given site
  dive <- leyte %>%
    tbl("diveinfo") %>%
    filter(site == site) %>%
    collect()
  
  # select all anemones from those dives
  anems <- leyte %>%
    tbl("anemones") %>%
    filter (dive_table_id %in% dive$dive_table_id) %>%
    select(anem_table_id, anem_obs, anem_id, old_anem_id) %>%
    collect()
  
  # select all fish that are on those anemones
  fish <- leyte %>%
    tbl("clownfish") %>%
    filter(anem_table_id %in% anems$anem_table_id) %>%
    select(sample_id, cap_id, anem_table_id, recap, tag_id) %>%
    collect()
  
  # are there any recaptured tags?
  recap <- fish %>% 
    filter(grepl("Y", recap))
  
  # are there any capids?
  capids <- fish %>% 
    filter(!is.na(cap_id))
  
  # select all sample_ids for which there is not a duplicated tag_id
  if(nrow(recap) != 0){
    uni <- fish %>% 
      filter(is.na(cap_id)) %>%  # don't want to remove fish that are capid
      distinct(tag_id) %>% 
      filter(!is.na(tag_id))
    
    # remove all fish that have a distinct tag_id (were not recaptured)
    fish <- anti_join(fish, uni, by = "tag_id")
  }
  
  # select all fish that have a tag_id or capid (because it is a recap, or are genetically recap)
  fish <- fish %>% 
    filter(!is.na(tag_id) | !is.na(cap_id))
  
  # attach anemone data
  fish <- left_join(fish, anems, by = "anem_table_id")
  
  return(fish)
}

# daterange_dive ####
#' for a given date range, list all of the dives
#' @export
#' @name daterange_dive
#' @author Michelle Stuart
#' @param x = beginning date
#' @param y = end date
#' @examples 
#' dat <- daterange_dive("2016-01-01", "2016-12-30")


daterange_dive <- function(begin_date, end_date){
  leyte <- read_db("Leyte")
  
  dive <- leyte %>% 
    tbl("diveinfo") %>% 
    filter(date > begin_date & date < end_date) %>% 
    collect()
  
  return(dive)
}


# year_map_fish ####
#' for a 
#' @export
#' @name year_map_fish
#' @author Michelle Stuart
#' @param x = year
#' @examples 
#' dat <- year_map_fish(2016)

year_map_fish <- function(year){
  # 
  library(tidyr)
  library(lubridate)
  
  begin <- paste(year, "01", "01", sep = "-")
  end <- paste(year, "12", "31", sep = "-")
  
  leyte <- read_db("Leyte")
  
  # pull dives for date range
  dive <- daterange_dive(begin, end)
  
  # reduce number of columns
  dive <- dive %>% 
    select(dive_table_id, date, site, gps) %>% 
    rename(unit = gps)
  
  # find anemones that match those dives and dates
  anem <- leyte %>%
    tbl("anemones") %>%
    filter(dive_table_id %in% dive$dive_table_id & !is.na(anem_id)) %>%
    select(dive_table_id, anem_table_id, anem_id, anem_obs, obs_time) %>%
    collect() %>% 
    separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
    mutate(gpx_hour = as.numeric(hour) - 8) %>% 
    mutate(minute = as.numeric(minute))
  
  anem <- left_join(anem, dive, by = "dive_table_id") %>% 
    select(-dive_table_id, -anem_obs)
  
  rm(dive)
  
  # fix date if gpx hour is less than 0
  test <- anem %>% 
    filter(gpx_hour < 0)
  
  if (nrow(test) > 0){
    anem <- anem %>%
      mutate(gpx_date = date) # create a new gpx date column
    
    other <- anem %>% 
      filter(gpx_hour < 0) 
    
    # remove from anem table
    anem <- anti_join(anem, other)
    
    # subtract date
    other <- other %>% 
      mutate(gpx_date = as.character(ymd(date) - days(1))) %>% 
      mutate(gpx_hour = gpx_hour + 24)
    
    # rejoin rows
    anem <- rbind(anem, other)
    
  }else{
    anem <- anem %>% mutate(gpx_date = date)
  }
  
  # retrieve fish for these anems
  fish <- leyte %>% 
    tbl("clownfish") %>% 
    filter(anem_table_id %in% anem$anem_table_id) %>% 
    filter(!is.na(sample_id)) %>% 
    collect()
  
  anem <- left_join(fish, anem, by = "anem_table_id")
  
  # find the lat long for this anem
  lat <- leyte %>%
    tbl("GPX") %>%
    mutate(gpx_date = date(time)) %>%
    filter(gpx_date %in% anem$gpx_date) %>% 
    mutate(gpx_hour = hour(time)) %>% 
    mutate(minute = minute(time)) %>% 
    mutate(second = second(time)) %>% 
    select(-time, -second) %>% 
    collect()
  
  sum_lat <- lat %>%
    group_by(unit, gpx_date, gpx_hour, minute) %>% 
    summarise(lat = mean(as.numeric(lat)),
      lon = mean(as.numeric(lon)))
  
  anem <- left_join(anem, sum_lat, by = c("unit", "gpx_date", "gpx_hour", "minute"))
  rm(lat, other, sum_lat, test, fish)
  
  out <- anem %>%
    select(lat, lon, date, site, anem_id, sample_id)
  
  name <- paste(Sys.Date(), "_GPSSurvey_fishlatlong_", year, "_forQGIS.csv", sep = "")
  write.table(out, file = name, col.names=T, sep=',', row.names=F, quote=T)
  
  return(anem) # this anem table contains fish
}

# sample_latlon ####
#' a function to find the lat long of a given fish
#' @export
#' @name sample_latlon
#' @author Michelle Stuart
#' @param x = sample_id - the id of the fish you are trying to get the lat long for
#' @examples 
#' where <- sample_latlon("APCL12_093")

sample_latlon <- function(sample_id){
  library(tidyr)
  # find the anem_table_id for the sample
  leyte <- read_db()
  anem <- leyte %>% 
    tbl("clownfish") %>%
    select(sample_id, anem_table_id) %>% 
    filter(sample_id == sample_id) %>% 
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
  
  sample_id <- (which(duplicated(lat$lat)) > 0 & which(duplicated(lat$lon) > 0))
  if(length(sample_id) == 0){
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



# anem_date ####
#' allows you to find the date based on the anem_table_id
#' @param x = anem_table_id - Where are the anem_table_id's located?
#' @keywords 
#' @export
#' @examples
#' anem_date(anem_table_id)
#' anem_date(2206)

anem_date <- function(anem_tbl_id){
  leyte <- read_db("Leyte")
  anem <- leyte %>% 
    tbl("anemones") %>% 
    filter(anem_table_id == anem_tbl_id) %>% 
    select(dive_table_id, anem_table_id) %>% 
    collect()
  
  day <- leyte %>% 
    tbl("diveinfo") %>%
    select(date, dive_table_id) %>%
    collect() %>% 
    filter(dive_table_id %in% anem$dive_table_id)
  
  day <- left_join(day, anem, by ="dive_table_id")
  return(day)
}

# sampleid_date ####
#' which date was a sample_id captured?
#' @export
#' @name sampleid_date
#' @author Michelle Stuart
#' @param x = sample_id - the id of the fish for which you are trying to get a date
#' @examples 
#' date <- sampleid_date("APCL12_093")


sampleid_date <- function(sampleid){
  leyte <- read_db("Leyte")
  
  fish <- leyte %>% 
    tbl("clownfish") %>% 
    select(anem_table_id, sample_id) %>% 
    collect() %>% 
    filter(sample_id %in% sampleid)
  
  anem <- leyte %>% 
    tbl("anemones") %>% 
    select(dive_table_id, anem_table_id) %>% 
    collect() %>% 
    filter(anem_table_id %in% fish$anem_table_id) 
  
  day <- leyte %>% 
    tbl("diveinfo") %>% 
    select(date, dive_table_id) %>% 
    collect() %>% 
    filter(dive_table_id %in% anem$dive_table_id)
  
  day <- left_join(day, anem, by = "dive_table_id")
  day <- left_join(day, fish, by = "anem_table_id") %>% 
    select(sample_id, date)
  
  return(day)
}

# anem_dive ####
#' for a given anemone , returns site, date, divetype
#' @export
#' @name anem_dive
#' @author Michelle Stuart
#' @param x = anem for which to get data
#' @examples 
#' info <- anem_dive(2183)

anem_dive <- function(anemid){
  leyte <- read_db("Leyte")
  
  # get the anem data  
  anems <- leyte %>% 
    tbl("anemones") %>% 
    collect() %>% 
    filter(anem_id %in% anemid)
  
  # retrieve date and site info for those anemones
  dive <- leyte %>% 
    tbl("diveinfo") %>%
    collect() %>% 
    filter(dive_table_id %in% anems$dive_table_id) %>% 
    select(dive_table_id, site, date, dive_type)
  
  
  # join dive info to anem info
  anems <- left_join(anems, dive, by = "dive_table_id")
  rm(dive)
  return(anems)
}








