# date_fish_cap ####
#' which date was a sample_id captured?
#' @export
#' @name date_fish_cap
#' @author Michelle Stuart
#' @param x = sample_id - the id of the fish for which you are trying to get a date
#' @examples 
#' date <- date_fish_cap("APCL12_093")


datefishcap <- function(x){
  source("scripts/con_leyte.R")
  library(dplyr)
  
  leyte <- conleyte()
  
  fish <- leyte %>% 
    tbl("clownfish") %>% 
    select(anem_table_id, sample_id) %>% 
    collect() %>% 
    filter(sample_id %in% x)
  
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
