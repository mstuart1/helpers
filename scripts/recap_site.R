# recap_site ####
#' views all of the fish recaptured at a given site
#' @export
#' @name recap_site
#' @author Michelle Stuart
#' @param x = site for which to get data
#' @examples 
#' info <- recap_site("Haina")

recap_site <- function(x){
  library(dplyr)
  source("scripts/con_leyte.R")
  leyte <- conleyte()
  
  # select all dives at a given site
  dive <- leyte %>%
    tbl("diveinfo") %>%
    filter(site == x) %>%
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
    filter(grepl(pattern = "Y", x = fish$recap))
    
  # are there any capids?
  capids <- fish %>% 
    filter(!is.na(cap_id))
  
  # select all sample_ids for which there is not a duplicated tag_id
  if(nrow(recap) != 0){
    uni <- fish %>% 
      distinct(tag_id) %>% 
      filter(!is.na(tag_id) & is.na(cap_id)) # don't want to remove fish that are capid
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

