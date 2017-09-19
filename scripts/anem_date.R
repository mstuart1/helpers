# which date was an anemone observed based on clownfish table?

#' This function allows you to find the date based on the anem_table_id
#' @param table$anem_table_id - Where are the anem_table_id's located?
#' @keywords 
#' @export
#' @examples
#' date_anem(table$anem_table_id)
#' date_anem(2206)

date_anem <- function(x){
  source("scripts/con_leyte.R")
  library(dplyr)
  leyte <- conleyte()
  anem <- leyte %>% 
    tbl("anemones") %>% 
    filter(anem_table_id == x) %>% 
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
