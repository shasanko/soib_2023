library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)

source("00_scripts/00_functions.R")


# Is the current run for a new major SoIB version (every 3-4 years), 
# or for an interannual update (every year between major versions)?
# testing git
interannual_update = TRUE


# PART 0 (paths) ----------------------------------------------------------

source("00_scripts/01_create_metadata.R")

rawpath = "00_data/ebd_IN_unv_smp_relAug-2025.txt"
sensitivepath = "00_data/ebd_sensitive_relAug-2025_IN.txt"


preimp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
           "LOCALITY.ID","LOCALITY.TYPE","REVIEWED","APPROVED","STATE","COUNTY",
           "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED","OBSERVER.ID",
           "PROTOCOL.NAME","DURATION.MINUTES","EFFORT.DISTANCE.KM","EXOTIC.CODE",
           "NUMBER.OBSERVERS","ALL.SPECIES.REPORTED","GROUP.IDENTIFIER","SAMPLING.EVENT.IDENTIFIER")

# CATEGORY - species, subspecies, hybrid, etc.; COMMON.NAME - common name of species;
# SCIENTIFIC NAME - scientific name; OBSERVATION.COUNT - count of each species observed in a list;
# LOCALITY.ID - unique location ID; LOCALITY.TYPE - hotspot, etc.;
# LATITUDE and LONGITUDE - coordinates; OBSERVATION.DATE - checklist date; 
# TIME.OBSERVATIONS.STARTED - checklist start time; OBSERVER ID - unique observer ID;
# PROTOCOL TYPE - stationary, traveling, historical, etc.; DURATION.MINUTES - checklist duration;
# EFFORT.DISTANCE.KM - distance traveled; NUMBER.OBSERVERS - no. of birders;
# ALL.SPECIES.REPORTED - indicates whether a checklist is complete or not;
# GROUP.IDENTIFIER - unique ID for every set of shared checklists (NA when not shared);
# SAMPLING.EVENT.IDENTIFIER - unique checlist ID

nms = read.delim(rawpath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                 na.strings = c(""," ",NA))
nms = names(nms)
nms[!(nms %in% preimp)] = "NULL"
nms[nms %in% preimp] = NA

# read data from certain columns only
data = read.delim(rawpath, colClasses = nms, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))

# read sensitive species data
nms1 = read.delim(sensitivepath, nrows = 1, sep = "\t", header = T, quote = "", stringsAsFactors = F, 
                  na.strings = c(""," ",NA))
nms1 = names(nms1)
nms1[!(nms1 %in% preimp)] = "NULL"
nms1[nms1 %in% preimp] = NA


# read sensitive species data

sesp = read.delim(sensitivepath, colClasses = nms1, sep = "\t", header = T, quote = "", 
                  stringsAsFactors = F, na.strings = c(""," ",NA))


# merge both data frames
data = rbind(data, sesp) %>%
  # remove unapproved records and records of escapees
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  filter(!EXOTIC.CODE %in% c("X"))


## choosing important columns required for further analyses

imp = c("CATEGORY","COMMON.NAME","SCIENTIFIC.NAME","OBSERVATION.COUNT",
        "LOCALITY.ID", "REVIEWED","APPROVED","EXOTIC.CODE",
        "LOCALITY.TYPE","STATE","COUNTY",
        "LATITUDE","LONGITUDE","OBSERVATION.DATE","TIME.OBSERVATIONS.STARTED",
        "OBSERVER.ID","PROTOCOL.NAME",
        "DURATION.MINUTES","EFFORT.DISTANCE.KM",
        "ALL.SPECIES.REPORTED","group.id","SAMPLING.EVENT.IDENTIFIER")


# no of days in every month, and cumulative number
days = c(31,28,31,30,31,30,31,31,30,31,30,31)
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)

data = data %>%
  # create a column "group.id" which can help remove duplicate checklists
  mutate(group.id = ifelse(is.na(GROUP.IDENTIFIER), 
                           SAMPLING.EVENT.IDENTIFIER, GROUP.IDENTIFIER)) %>%
  dplyr::select(all_of(imp)) %>%
  # other useful columns
  # set date, add month, year and day columns using package LUBRIDATE
  mutate(OBSERVATION.DATE = as.Date(OBSERVATION.DATE), 
         month = month(OBSERVATION.DATE),
         day = day(OBSERVATION.DATE) + cdays[month], 
         #week = week(OBSERVATION.DATE),
         #fort = ceiling(day/14),
         cyear = year(OBSERVATION.DATE)) %>%
  dplyr::select(-c("OBSERVATION.DATE")) %>%
  mutate(year = ifelse(month > 5, cyear, cyear-1)) %>% # from June to May
  # add number of species/list length column (no.sp), for list length analyses (lla)
  group_by(group.id) %>% 
  mutate(no.sp = n_distinct(COMMON.NAME)) %>%
  ungroup()


# remove probable mistakes
source("00_scripts/rm_prob_mistakes.R")
data <- rm_prob_mistakes(data)


# create and write a file with common names and scientific names of all Indian species
# useful for mapping
temp = data %>%
  filter(CATEGORY == "species" | CATEGORY == "issf") %>%
  distinct(COMMON.NAME,SCIENTIFIC.NAME)
#write.csv(temp,"00_data/indiaspecieslist.csv", row.names=FALSE)

# create location file for LULC
locdat = data %>% distinct(LOCALITY.ID, LATITUDE, LONGITUDE)
#write.csv(locdat,"00_data/eBird_location_data.csv", row.names=FALSE)


# need to combine several closely related species and slashes/spuhs
# so, first changing their category to species since they will be combined next
data = data %>%
  mutate(SCIENTIFIC.NAME = NULL, # needed it for printing indiaspecieslists
         CATEGORY = case_when(COMMON.NAME %in% c(
           "Green/Greenish Warbler", "Siberian/Amur Stonechat", "Red-necked/Little Stint",
           "Western/Eastern Yellow Wagtail", "Common/Himalayan Buzzard",
           "Western/Eastern Marsh Harrier", "Tibetan/Greater Sand-Plover", "Baikal/Spotted Bush Warbler",
           "Lemon-rumped/Sichuan Leaf Warbler",
           "Bank Swallow/Pale Martin", "Riparia sp.", "Greater/Mongolian Short-toed Lark",
           "Taiga/Red-breasted Flycatcher", "Tricolored x Chestnut Munia (hybrid)", "Little/House Swift", 
           "Pin-tailed/Swinhoe's Snipe", "Booted/Sykes's Warbler", "Iduna sp.", "Greater/Malabar Flameback",
           "Indian/Oriental Cuckooshrike","European/Eastern Red-rumped Swallow",
           "Hainan Blue/Blue-throated/Chinese Blue Flycatcher"
         ) ~ "species",
         TRUE ~ CATEGORY)) %>%
  # combining species, slashes and spuhs
  mutate(COMMON.NAME = case_when(
    COMMON.NAME %in% c("Green Warbler", "Green/Greenish Warbler") ~ "Greenish Warbler",
    COMMON.NAME %in% c("Amur Stonechat", "Siberian/Amur Stonechat") ~ "Siberian Stonechat",
    COMMON.NAME %in% c("Red-necked Stint", "Red-necked/Little Stint") ~ "Little Stint",
    COMMON.NAME %in% c("Eastern Yellow Wagtail", 
                       "Western/Eastern Yellow Wagtail") ~ "Western Yellow Wagtail",
    COMMON.NAME %in% c("Himalayan Buzzard", 
                       "Common/Himalayan Buzzard") ~ "Common Buzzard",
    COMMON.NAME %in% c("Eastern Marsh Harrier", 
                       "Western/Eastern Marsh Harrier") ~ "Western Marsh Harrier",
    COMMON.NAME %in% c("Greater Sand-Plover", 
                       "Tibetan/Greater Sand-Plover") ~ "Tibetan Sand-Plover",
    COMMON.NAME %in% c("Baikal Bush Warbler", 
                       "Baikal/Spotted Bush Warbler") ~ "Spotted Bush Warbler",
    COMMON.NAME %in% c("Sichuan Leaf Warbler", 
                       "Lemon-rumped/Sichuan Leaf Warbler") ~ "Lemon-rumped Warbler",
    COMMON.NAME %in% c("Pale Martin", "Bank Swallow/Pale Martin", 
                       "Riparia sp.") ~ "Gray-throated Martin",
    COMMON.NAME %in% c("Mongolian Short-toed Lark", 
                       "Greater/Mongolian Short-toed Lark") ~ "Greater Short-toed Lark",
    COMMON.NAME %in% c("Taiga Flycatcher", 
                       "Taiga/Red-breasted Flycatcher") ~ "Red-breasted Flycatcher",
    COMMON.NAME %in% c("Chestnut Munia", 
                       "Tricolored x Chestnut Munia (hybrid)") ~ "Tricolored Munia",
    COMMON.NAME %in% c("House Swift", "Little/House Swift") ~ "Little Swift",
    COMMON.NAME %in% c("Swinhoe's Snipe", 
                       "Pin-tailed/Swinhoe's Snipe") ~ "Pin-tailed Snipe",
    COMMON.NAME %in% c("Sykes's Warbler", "Booted/Sykes's Warbler",
                       "Iduna sp.") ~ "Booted Warbler",
    COMMON.NAME %in% c("Malabar Flameback", 
                       "Greater/Malabar Flameback") ~ "Greater Flameback",
    COMMON.NAME %in% c("Nicobar Hooded Pitta") ~ "Western Hooded Pitta",
    COMMON.NAME %in% c("Oriental Cuckooshrike", 
                       "Indian/Oriental Cuckooshrike") ~ "Indian Cuckooshrike",
    COMMON.NAME %in% c("European Red-rumped Swallow", 
                       "European/Eastern Red-rumped Swallow") ~ "Eastern Red-rumped Swallow",
    COMMON.NAME %in% c("Hainan Blue Flycatcher", 
                       "Hainan Blue/Blue-throated/Chinese Blue Flycatcher") ~ "Blue-throated Flycatcher",
    TRUE ~ COMMON.NAME
  ))


## setup eBird data ##


# for automatically selecting the latest migratory year to use in 
# current SoIB run (done annually)
full_soib_my <- data |> 
  distinct(year, month) |> 
  group_by(year) |> 
  reframe(n_month = n_distinct(month)) |> 
  filter(n_month == 12) |> 
  pull(year)

latest_soib_my <- max(full_soib_my)

# median years for each historical timegroup
median_soib_hist_years <- data %>% 
  distinct(group.id, .keep_all = TRUE) %>% 
  filter(ALL.SPECIES.REPORTED == 1) %>%
  mutate(hist_period = case_when(year <= 1999 ~ 1,
                                 year > 1999 & year <= 2006 ~ 2,
                                 year > 2006 & year <= 2010 ~ 3,
                                 year > 2010 & year <= 2012 ~ 4)) %>% 
  group_by(hist_period) %>% 
  reframe(median_year = round(median(year))) %>% 
  arrange(hist_period) %>% 
  pull(median_year)

save(full_soib_my, latest_soib_my, median_soib_hist_years,
     file = "00_data/current_soib_migyears_prep_dataforanaly_extra.RData")
# this RData file gets updated each time readcleanrawdata() is run---
# which is usually only once in each annual or "major" update
# (intermediate changes usually don't run readcleanrawdata() so year won't change)


## remove repeats by retaining only a single group.id + species combination

data = data %>%
  distinct(group.id, COMMON.NAME, .keep_all = TRUE) |> 
  filter(year <= latest_soib_my) %>% 
  rename(ST_NM = STATE,
         DISTRICT = COUNTY) %>% 
  dplyr::select(-SAMPLING.EVENT.IDENTIFIER)

assign("data", data, .GlobalEnv)

# save workspace
save(data, file = "00_data/rawdata.RData")
rm(data, pos = ".GlobalEnv")

tic("Adding map and grid variables to dataset")
addmapvars()
toc() # 11 min

load("00_data/analyses_metadata.RData")


# 0. preparing data ----------------------------------------------------------

# mapping of SoIB-species-of-interest to a range of variables/classifications
# (manually created)
fullmap = read.csv("00_data/SoIB_mapping_2024.csv")


# species frequently misidentified and therefore ignored in analyses ###
spec_misid <- c("Besra","Singing Bushlark","Common Flameback",
                "Eastern Orphean Warbler","Richard's Pipit",
                "Asian Palm Swift")
# saving to read in resolve step
#save(spec_misid, file = "00_data/spec_misid.RData")


# species info for different slices ###
spec_resident = fullmap %>%
  filter(Migratory.Status.Within.India %in% c("Resident",
                                              "Resident & Altitudinal Migrant",
                                              "Resident & Winter Migrant",
                                              "Resident & Summer Migrant",
                                              "Resident & Local Migrant",
                                              "Resident & Localized Summer Migrant",
                                              "Resident & Within-India Migrant",
                                              "Resident (Extirpated)")) %>%
  pull(eBird.English.Name.2024)

# species filtered for certain habitat masks
spec_woodland = fullmap %>%
  filter(Habitat.Specialization %in% c("Forest",
                                       "Forest & Plantation")) %>%
  pull(eBird.English.Name.2024)

# we are considering cropland and ONE habitats together to classify "openland species"
spec_openland = fullmap %>%
  filter(Habitat.Specialization %in% c("Alpine & Cold Desert",
                                       "Grassland",
                                       "Grassland & Scrub",
                                       "Open Habitat")) %>%
  pull(eBird.English.Name.2024)


# 0. main data filtering -----------------------------------------------------

load("00_data/data.RData")

# for stats/summary of data filtering and properties at each step
stats1 = paste(nrow(data),"filter 0 observations")
stats2 = paste(length(unique(data$group.id)),"filter 0 unique checklists")

data = data %>%
  # not considering travelling lists covering > 50km at all
  filter(is.na(EFFORT.DISTANCE.KM) | EFFORT.DISTANCE.KM <= 50) %>%
  # data quality
  filter(REVIEWED == 0 | APPROVED == 1) %>%
  # removing exotic species observations
  filter(!EXOTIC.CODE %in% c("X"))

data = data %>%
  mutate(timegroups = case_when(year <= 1999 ~ soib_year_info("timegroup_lab")[1],
                                year > 1999 & year <= 2006 ~ soib_year_info("timegroup_lab")[2],
                                year > 2006 & year <= 2010 ~ soib_year_info("timegroup_lab")[3],
                                year > 2010 & year <= 2012 ~ soib_year_info("timegroup_lab")[4],
                                year >= 2013 ~ as.character(year))) 


# removing vagrants
data = removevagrants(data)

stats3 = paste(nrow(data),"filter 1 observations")
stats4 = paste(length(unique(data$group.id)),"filter 1 unique checklists")
stats5 = paste(nrow(data[data$ALL.SPECIES.REPORTED == 1,]),
               "filter 1 usable observations")
stats6 = paste(length(unique(data[data$ALL.SPECIES.REPORTED == 1,]$group.id)),
               "filter 1 unique complete checklists")

# removing false complete lists
data = completelistcheck(data)


data_base = data


data0 = data_base %>% dplyr::select(-REVIEWED,-APPROVED,-cyear)
# this file is for uses outside of main eBird trends analyses, has extra columns
save(data0, file = "00_data/dataforanalyses_extra.RData")
