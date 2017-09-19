# map_anem ####
#' create a csv of anem locations and ids that can be imported into QGIS
#' @export
#' @name map_anem
#' @author Michelle Stuart
#' @param x = year
#' @examples 
#' dat <- map_anem(2016)

map_anem <- function(){
  library(dplyr)
  source("scripts/con_leyte.R")
  source("scripts/date_dive.R")

  leyte <- conleyte()
  
  # pull dives for date range
  dive <- date_dive("2016-01-01", "2016-12-30")
  
  # reduce number of columns
  dive <- dive %>% 
    select(dive_table_id, date, site)
  
  # find anemones that match those dives and dates
  anem <- leyte %>%
    tbl("anemones") %>%
    filter(dive_table_id %in% dive$dive_table_id & !is.na(anem_id)) %>%
    select(dive_table_id, anem_id, anem_obs, obs_time) %>%
    collect() %>% 
    separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
    mutate(gpx_hour = as.numeric(hour) - 8)
    

  anem <- left_join(anem, dive, by = "dive_table_id")
  rm(dive)
  
  i <- min(anem$date)
  j <- max(anem$date)

    
  latlong <- leyte %>%
    tbl("GPX") %>%
    collect(n = Inf) %>% 
    filter(time > i & time < j)
    

anem$lat <- NA
anem$lon <- NA
for(i in 1:nrow(anem)){
  #Get date and time information for the anemone
  date <- as.character(anem$date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(anem$ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour < 0){
    day <- day - 1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    anem$lat[i] = latlong$lat[latlongindex][i2]
    anem$lon[i] = latlong$long[latlongindex][i2]
  }
}
### WAIT ###
anem$dive_table_id <- NULL

# Write out for QGIS (has column headers)

out <- anem[,c('lat', 'lon', 'date', "name", "anem_id", "anemobs")]

# for samples that don't have an anemobs, replace with anem_id
out$anemobs[is.na(anem$anemobs)] <- paste("#", out$anem_id[is.na(anem$anemobs)], sep = "")

write.table(out, file=paste(Sys.Date(), "GPSSurvey_anemlatlong_", year, "_forQGIS.csv", sep = ""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)

# 2015_05 anem data -------------------------------------------------------

suppressWarnings (dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2015-04-01" & date < "2016-01-01") %>% select(id, date, name) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id & !is.na(anem_id)) %>% select(dive_table_id, anem_id, anemobs, ObsTime) %>% collect()
anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
rm(dive)
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2015) %>% collect()

anem$lat <- NA
anem$lon <- NA
for(i in 1:nrow(anem)){
  #Get date and time information for the anemone
  date <- as.character(anem$date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(anem$ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour < 0){
    day <- day - 1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    anem$lat[i] = latlong$lat[latlongindex][i2]
    anem$lon[i] = latlong$long[latlongindex][i2]
  }
}
### WAIT ###
anem$dive_table_id <- NULL

# Write out for QGIS (has column headers)

out <- anem[,c('lat', 'lon', 'date', "name", "anem_id", "anemobs")]

# for samples that don't have an anemobs, replace with anem_id
out$anemobs[is.na(anem$anemobs)] <- paste("#", out$anem_id[is.na(anem$anemobs)], sep = "")

write.table(out, file=paste(Sys.Date(), "GPSSurvey_anemlatlong_", year, "_05_forQGIS.csv", sep = ""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)

# # 2015_01 anem data -------------------------------------------------------
# 
# suppressWarnings (dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2015-01-01" & date < "2015-04-01") %>% select(id, date, name) %>% collect())
# anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id & !is.na(anem_id)) %>% select(dive_table_id, anem_id, anemobs, ObsTime) %>% collect()
# anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
# rm(dive)
# latlong <- leyte %>% tbl("GPX") %>% filter(year == 2015) %>% collect()
# 
# anem$lat <- NA
# anem$lon <- NA
# 
# for(i in 1:nrow(anem)){
#   #Get date and time information for the anemone
#   date <- as.character(anem$date[i])
#   datesplit <- strsplit(date,"-", fixed = T)[[1]]
#   year <- as.numeric(datesplit[1])
#   month <- as.numeric(datesplit[2])
#   day <- as.numeric(datesplit[3])
#   time <- as.character(anem$ObsTime[i])
#   timesplit <- strsplit(time, ":", fixed = T)[[1]]
#   hour <- as.numeric(timesplit[1])
#   min <- as.numeric(timesplit[2])
#   sec <- as.numeric(timesplit[3])
#   
#   # Convert time to GMT
#   hour <- hour - 8
#   if(!is.na(hour) & hour < 0){
#     day <- day - 1
#     hour <- hour + 24
#   }
#   
#   # Find the location records that match the date/time stamp (to nearest second)
#   latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
#   i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
#   
#   # Calculate the lat/long for this time
#   if(length(i2)>0){
#     anem$lat[i] = latlong$lat[latlongindex][i2]
#     anem$lon[i] = latlong$long[latlongindex][i2]
#   }
# }
# ### WAIT ###
# anem$dive_table_id <- NULL
# 
# # Write out for QGIS (has column headers)
# 
# out <- anem[,c('lat', 'lon', 'date', "name", "anem_id", "anemobs")]
# 
# # for samples that don't have an anemobs, replace with anem_id
# out$anemobs[is.na(anem$anemobs)] <- out$anem_id[is.na(anem$anemobs)]
# 
# write.table(out, file=paste(Sys.Date(), "GPSSurvey_anemlatlong_", year, "_01_forQGIS.csv", sep = ""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)
# this data does not currently live on the database

# 2014 anem data ----------------------------------------------------------

suppressWarnings (dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2014-01-01" & date < "2015-01-01") %>% select(id, date, name) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id & !is.na(anem_id)) %>% select(dive_table_id, anem_id, anemobs, ObsTime) %>% collect()
anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
rm(dive)
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2014) %>% collect()

anem$lat <- NA
anem$lon <- NA
for(i in 1:nrow(anem)){
  #Get date and time information for the anemone
  date <- as.character(anem$date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(anem$ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour < 0){
    day <- day - 1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    anem$lat[i] = latlong$lat[latlongindex][i2]
    anem$lon[i] = latlong$long[latlongindex][i2]
  }
}
### WAIT ###
anem$dive_table_id <- NULL

# Write out for QGIS (has column headers)

out <- anem[,c('lat', 'lon', 'date', "name", "anem_id", "anemobs")]

# for samples that don't have an anemobs, replace with anem_id
out$anemobs[is.na(anem$anemobs)] <- paste("#", out$anem_id[is.na(anem$anemobs)], sep = "")

# remove lines without lat lon
out <- out[!is.na(out$lat), ]

write.table(out, file=paste(Sys.Date(), "GPSSurvey_anemlatlong_", year, "_forQGIS.csv", sep = ""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)

# 2013 anem data ----------------------------------------------------------
suppressWarnings (dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2013-01-01" & date < "2014-01-01") %>% select(id, date, name) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id & !is.na(anem_id)) %>% select(dive_table_id, anem_id, anemobs, ObsTime) %>% collect()
anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
rm(dive)
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2013) %>% collect()

anem$lat <- NA
anem$lon <- NA
for(i in 1:nrow(anem)){
  #Get date and time information for the anemone
  date <- as.character(anem$date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(anem$ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour < 0){
    day <- day - 1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    anem$lat[i] = latlong$lat[latlongindex][i2]
    anem$lon[i] = latlong$long[latlongindex][i2]
  }
}
### WAIT ###
anem$dive_table_id <- NULL

# Write out for QGIS (has column headers)

out <- anem[,c('lat', 'lon', 'date', "name", "anem_id", "anemobs")]

# for samples that don't have an anemobs, replace with anem_id
out$anemobs[is.na(anem$anemobs)] <- paste("#", out$anem_id[is.na(anem$anemobs)], sep = "")

# remove lines without lat lon
out <- out[!is.na(out$lat), ]

write.table(out, file=paste(Sys.Date(), "GPSSurvey_anemlatlong_", year, "_forQGIS.csv", sep = ""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)



# 2012 anem data ----------------------------------------------------------


suppressWarnings (dive <- leyte %>% tbl("diveinfo") %>% filter(date > "2012-01-01" & date < "2013-01-01") %>% select(id, date, name) %>% collect())
anem <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dive$id & !is.na(anem_id)) %>% select(dive_table_id, anem_id, anemobs, ObsTime) %>% collect()
anem <- left_join(anem, dive, by = c("dive_table_id" = "id"))
rm(dive)
latlong <- leyte %>% tbl("GPX") %>% filter(year == 2012) %>% collect()

anem$lat <- NA
anem$lon <- NA
for(i in 1:nrow(anem)){
  #Get date and time information for the anemone
  date <- as.character(anem$date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(anem$ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour < 0){
    day <- day - 1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    anem$lat[i] = latlong$lat[latlongindex][i2]
    anem$lon[i] = latlong$long[latlongindex][i2]
  }
}
### WAIT ###
anem$dive_table_id <- NULL

# Write out for QGIS (has column headers)

out <- anem[,c('lat', 'lon', 'date', "name", "anem_id", "anemobs")]

# for samples that don't have an anemobs, replace with anem_id
out$anemobs[is.na(anem$anemobs)] <- paste("#", out$anem_id[is.na(anem$anemobs)], sep = "")

# remove lines without lat lon
out <- out[!is.na(out$lat), ]

write.table(out, file=paste(Sys.Date(), "GPSSurvey_anemlatlong_", year, "_forQGIS.csv", sep = ""), col.names=TRUE, sep=',', row.names=FALSE, quote=TRUE)



}
