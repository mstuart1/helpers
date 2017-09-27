# daterange_dive ####
#' for a given date range, list all of the dives
#' @export
#' @name daterange_dive
#' @author Michelle Stuart
#' @param x = beginning date
#' @param y = end date
#' @examples 
#' dat <- daterange_dive("2016-01-01", "2016-12-30")


daterange_dive <- function(x, y){
  leyte <- read_db()
  
  dive <- leyte %>% 
    tbl("diveinfo") %>% 
    filter(date > x & date < y) %>% 
    collect()
  
  return(dive)
}
  
  
