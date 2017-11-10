# anem_obs ####
#' match up existing anemones with their old anem IDs
#' @export
#' @name anem_obs
#' @author Michelle Stuart
#' @param 
#' @examples 
#' dat <- anem_obs()

source("scripts/helpers.R")

# connect to the database
leyte <- read_db("Leyte")

# pull in all of the anemone data
anem <- leyte %>%
  tbl("anemones") %>%
  collect()


# select data to be changed
change <- anem %>% 
  filter(!is.na(anem_id))

# remove from the db
anem <- anti_join(anem, change, by = "anem_table_id")

# find all of the anemones that have a value in the oldAnemID column and remove duplicates - these are anems that have been seen more than once ~10, went from 276 to 266
multi <- change %>%
  filter(!is.na(old_anem_id)) %>%
  select(old_anem_id, anem_id) %>%
  distinct()

# find anem_ids that occur more than once
dups <- change %>% 
  group_by(anem_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>% 
  filter(anem_id != "-9999")

# find all of the anems that are in the dups list and add them to the multi - dont' do distinct because need multiple rows 
more <- change %>% 
  filter(anem_id %in% dups$anem_id) %>% 
  select(old_anem_id, anem_id) %>% 
  rbind(multi) %>% 
  distinct()

# at this point, some anem_ids are represented more than once, and some have an old_anem_id and some do not.  Remove any duplicated that do not have an old_anem_id

# make a list of duplicated
many <- multi %>% 
  group_by(anem_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) 

# take the ones that do not have an old_name_id
so_many <- multi %>% 
  filter(anem_id %in% many$anem_id) %>% 
  filter(is.na(old_anem_id))

# remove from multi
multi <- anti_join(multi, so_many)

# find the next obs number
n <- max(change$anem_obs, na.rm = T)
if (n == -Inf){
  n <- 0
}
multi$anem_obs <- (n+1):(n+nrow(multi))
rm(more, many, so_many, dups)

# for each row in multi, which old_anem_id = anem_id, assign the old anem_id the same anem_obs as anem_id
for (i in 1:nrow(multi)) {
  multi$anem_obs[which(multi$old_anem_id == multi$anem_id[i])] <- multi$anem_obs[i]
}

# find old_anem_ids that are not in the multi table as original anem_ids
have_match <- multi %>% 
  filter(old_anem_id %in% anem_id)

# remove all old_anem_ids that do not have a match in the id column and create an id column for them
need_match <- anti_join(multi, have_match) %>% 
  select(old_anem_id, anem_obs) %>% 
  mutate(anem_id = old_anem_id)

# rejoin to multi
multi <- rbind(multi, need_match) %>% 
  filter(anem_id != "-9999")

# to incorporate this into the anem table, no longer need old_anem_id column because it is represented in the anem_id column and matched to an anem_obs
multi <- multi %>% 
  select(-old_anem_id) %>% 
  distinct()


# remove rows to be changed from original db
old <- anti_join(old, anem)

# remove anem_obs from the anem table because nothing has ever been assigned an anem obs before, this won't work in the future - 
anem <- select(anem, -anem_obs)

# join the info
anem <- left_join(anem, multi, by = "anem_id")

# join to odb
anem <- rbind(old, anem)

# # too many rows, which row numbers are duplicated?
# prob <- anem %>% 
#   group_by(anem_table_id) %>% 
#   summarise(count = n()) %>% 
#   filter(count > 1) 
# 
# test <- filter(anem, anem_table_id == 860)


library(RMySQL)

# leyte <- dbConnect(MySQL(), "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
# 
# dbWriteTable(leyte, "anemones", anem, row.names = F, overwrite = T)
# # 
# # 
# dbDisconnect(leyte)
# rm(leyte)

