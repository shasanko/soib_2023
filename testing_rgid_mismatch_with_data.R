load("01_analyses_full/dataforanalyses.RData-data_opt") # Loads data
load("01_analyses_full/randomgroupids.RData") # Load randomgroupids

rgid_1 <- randomgroupids[,1]

convert_group_id <- function(x) {
  base <- ifelse(substr(x,1,1)=="S",0L,1000000000L) # G=>giga base
  return(base + as.integer(substr(x,2,12)))
}

rgid_1_int <- convert_group_id(rgid_1)

load("01_analyses_full/rgids-1.RData") 
# Loads randomgroupids, replaces the one from Load 1 with the subsampled randomgroupids generated via remap-grids.R

pull_randomid_into_data_int_opt <- data %>%
  filter(group.id %in% randomgroupids) # randomgroupids from rgids-1.RData not work
# randomgroupids is a dataframe

pull_randomid_into_data_int_opt_2 <- data %>%
  filter(group.id %in% rgid_1_int) # works
# rgid_1_int is a vector

class(randomgroupids)
class(rgid_1_int)
