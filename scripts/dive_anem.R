# dive_anem ####
#' attaches select dive info to anem data
#' @export
#' @name dive_anem
#' @author Michelle Stuart
#' @param x = anem for which to get data
#' @examples 
#' info <- dive_anem(2183, y= c("site", "date", dive_type))

dive_anem <- function(x){
  source("scripts/con_leyte.R")
  leyte <- conleyte()
  
  # select anemones and fields to pull from db
  if (length(x) == 1){
    anems <- leyte %>% 
      tbl("anemones") %>% 
      filter(anem_id == x) %>%
      select(dive_table_id, anem_id, old_anem_id, anem_obs, anem_spp, anem_dia, notes, obs_time) %>%
      collect()
  }else{
    anems <- leyte %>% 
      tbl("anemones") %>% 
      filter(anem_id %in% x) %>%
      collect()
  }
  
  # retrieve date and site info for those anemones
  dive <- leyte %>% 
    tbl("diveinfo") %>%
    filter(dive_table_id %in% anems$dive_table_id) %>%
    collect()
  
  # join dive info to anem info
  anems <- left_join(anems, dive, by = "dive_table_id")
  rm(dive)
  return(anems)
}
