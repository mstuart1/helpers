# date_dive ####
#' a list of dives within a date range
#' @export
#' @name date_dive
#' @author Michelle Stuart
#' @param x = beginning date
#' @param y = end date
#' @examples 
#' dat <- date_dive("2016-01-01", "2016-12-30")


date_dive <- function(x, y){
  library(dplyr)
  source("scripts/con_leyte.R")
  
  leyte <- conleyte()
  
  dive <- leyte %>% 
    tbl("diveinfo") %>% 
    filter(date > x & date < y) %>% 
    collect()
  
  return(dive)
}
  
  
