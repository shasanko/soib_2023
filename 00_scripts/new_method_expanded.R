library(tidyverse)
library(lme4)
library(VGAM)

source('00_scripts/00_functions.R')

load("01_analyses_full/specieslists.RData")
load("01_analyses_full/dataforanalyses.RData")

# data = data %>%
#   # converting months to seasons
#   mutate(season = as.numeric(month)) %>% 
#   mutate(season = case_when(season %in% c(12,1,2) ~ "Win",
#                             season %in% c(3,4,5) ~ "Sum",
#                             season %in% c(6,7,8) ~ "Mon",
#                             season %in% c(9,10,11) ~ "Aut")) %>% 
#   mutate(season = as.factor(season))

## species-wise

# speclist_ht = specieslist$COMMON.NAME[!is.na(specieslist$ht)]
# 
# speclist_ht <- speclist_ht[1]

speclist_ht <- "Black Drongo"

reproducible <- TRUE

c = 0

for (species in speclist_ht)
{
  c = c + 1
  data1 = data
  rm(data)
  
  if(reproducible) {
    message("Setting seed to 0 to ensure reproducible runs")
    set.seed(0)
  }
  
  # get information for the species of interest 
  specieslist2 = specieslist %>% filter(COMMON.NAME == species)
  
  # three different flags for three different model types that will be run.
  # 0 is normal model, with full random effects. depending on restricted species,
  # model changes slightly.
  flag = 0
  if (species %in% restrictedspecieslist$COMMON.NAME)
  {
    flag = 1
    restrictedlist1 = restrictedspecieslist %>% filter(COMMON.NAME == species)
    specieslist2$ht = restrictedlist1$ht
    specieslist2$rt = restrictedlist1$rt
    
    if (restrictedlist1$mixed == 0) {
      flag = 2
    }
  }
  
  # filters data based on whether the species has been selected for long-term trends (ht) 
  # or short-term trends (rt) 
  # (if only recent, then need to filter for recent years. else, use all years so no filter.)
  
  singleyear <- FALSE
  
  if (singleyear == FALSE) {
    
    if (is.na(specieslist2$ht) & !is.na(specieslist2$rt)) {
      data1 = data1 %>% filter(year >= soib_year_info("cat_start", container))
    }
    
  } else if (singleyear == TRUE) {
    
    data1 = data1 %>% filter(year == soib_year_info("latest_year", container))
  }
  
  
  data1 = data1 %>%
    filter(COMMON.NAME == species) %>%
    distinct(gridg3, month) %>% 
    left_join(data1) %>%
    suppressMessages()
  
  dataset_size = nrow(data1)
  
  tm = data1 %>% distinct(timegroups)
  #rm(data, pos = ".GlobalEnv")
  
  # tmp <- data1 %>% filter(gridg0 == "102217" &
  #                           gridg1 == "4064" & 
  #                           gridg3 == "240")
  
  datay = data1 %>%
    distinct(gridg3, gridg1, group.id, .keep_all = TRUE) %>% 
    group_by(gridg3, gridg1) %>% 
    reframe(medianlla = median(no.sp)) %>%
    group_by(gridg3) %>% 
    reframe(medianlla = mean(medianlla)) %>%
    reframe(medianlla = round(mean(medianlla)))
  
  medianlla = datay$medianlla

    
  # setDT(data1)
  #   
  #   # Get distinct rows and filter based on a condition
  #   # (using base data.table because lazy_dt with immutable == FALSE would
  #   # modify data even though we are assigning to checklistinfo.
  #   # and immutable == TRUE copies the data and this is a huge bottleneck)
  #   # considers only complete lists
  #   
  #   if (singleyear == FALSE) {
  #     
  #     checklistinfo <- unique(data1[, 
  #                                  .(gridg1, gridg3, ALL.SPECIES.REPORTED,
  #                                    group.id, month, year, no.sp, timegroups)
  #     ])[
  #       # filter
  #       ALL.SPECIES.REPORTED == 1
  #     ]
  #     
  #   } else if (singleyear == TRUE) {
  #     
  #     checklistinfo <- unique(data1[, 
  #                                  .(gridg2, gridg3, ALL.SPECIES.REPORTED,
  #                                    group.id, month, year, no.sp)
  #     ])[
  #       # filter
  #       ALL.SPECIES.REPORTED == 1
  #     ]
  #     
  #   }
  #   
  #   
  #   
  #   checklistinfo <- checklistinfo[
  #     , 
  #     .SD[1], # subset of data
  #     by = group.id
  #   ]
  #   
  #   
  #   # expand data frame to include the bird species in every list
  #   
  #   join_by_temp <- if (singleyear == FALSE) {
  #     c("group.id", "gridg1", "gridg3",
  #       "ALL.SPECIES.REPORTED", "month", "year",
  #       "no.sp", "timegroups", "COMMON.NAME")
  #   } else if (singleyear == TRUE) {
  #     c("group.id", "gridg1", "gridg3",
  #       "ALL.SPECIES.REPORTED", "month", "year",
  #       "no.sp","COMMON.NAME")
  #   }
  #   
  #   data2 = checklistinfo %>% 
  #     lazy_dt(immutable = FALSE) |> 
  #     mutate(COMMON.NAME = species) %>% 
  #     left_join(data |> lazy_dt(immutable = FALSE),
  #               by = join_by_temp) %>%
  #     dplyr::select(-c("COMMON.NAME",
  #                      "ALL.SPECIES.REPORTED","group.id","year")) %>%
  #     # deal with NAs (column is character)
  #     mutate(OBSERVATION.COUNT = case_when(is.na(OBSERVATION.COUNT) ~ 0,
  #                                          OBSERVATION.COUNT != 0 ~ 1,
  #                                          TRUE ~ as.numeric(OBSERVATION.COUNT))) |> 
  #     as_tibble()
  #   
  #   rm(join_by_temp)
  #   
  #   return(data2)
  #   
  # }
  
  # expand dataframe to include absences as well
  ed = expand_dt(data1, species) %>%
    # converting months to seasons
    mutate(season = as.numeric(month)) %>% 
    mutate(season = case_when(season %in% c(12,1,2) ~ "Win",
                              season %in% c(3,4,5) ~ "Sum",
                              season %in% c(6,7,8) ~ "Mon",
                              season %in% c(9,10,11) ~ "Aut")) %>% 
    mutate(season = as.factor(season))
  
  # ed_tmp <- ed %>% filter(gridg0 == "102217" &
  #                    gridg1 == "4064" & 
  #                    gridg3 == "240")
  
  # save some values referenced later so we can get rid of memory hog data1
  gg1 <- data1$gridg1[1]
  gg3 <- data1$gridg3[1]
  rm(data1)
  
  # prepare a new data file to predict
  
  ltemp <- ed %>% 
    {if (singleyear == FALSE) {
      group_by(., season) %>% 
        reframe(., timegroups = unique(tm$timegroups))
    } else if (singleyear == TRUE) {
      distinct(., season)
    }} %>% 
    mutate(no.sp = medianlla,
           # taking the first value but any random value will do because we do not
           # intend to predict random variation across grids
           gridg1 = gg1,
           gridg3 = gg3)
  
  m = glmer(OBSERVATION.COUNT ~ timegroups + season + season:log(no.sp) + (1|gridg3/gridg1),
            data = ed, family = binomial(link = 'cloglog'),
            nAGQ = 0, control = glmerControl(optimizer = "bobyqa"))
  
  
  # bootMer
  
  pred_fun <- function(input_model) {
    predict(input_model, newdata = ltemp, re.form = NA, allow.new.levels = TRUE)
    # not specifying type = "response" because will later transform prediction along with SE
  }
  
  # tictoc::tic("bootMer 1000 sims")
  par_cores = 12
  pred_bootMer = bootMer(m, 
                         nsim = 100, # for faster compute, estimate doesn't change much with high sims
                         FUN = pred_fun, 
                         seed = 1000, use.u = FALSE, type = "parametric", 
                         parallel = "multicore", ncpus = par_cores)
  
  f2 = ltemp %>% 
    dplyr::select(., timegroups) %>%
    mutate(freq = 0, se = 0)  # this is not actually needed
  
  f2$freqt = colMeans(pred_bootMer$t)
  f2$set = apply(pred_bootMer$t,2,sd)
  # tictoc::toc()
  
  f1 = f2 %>%
    filter(!is.na(freqt) & !is.na(set)) %>%
    # average across season
    group_by(., timegroups) %>% 
    reframe(mean_trans = mean(freqt), se_trans = mean(set)) %>% 
    right_join(tm) %>% 
    left_join(databins %>% distinct(timegroups, year)) %>% 
    rename(timegroupsf = timegroups,
           timegroups = year) %>% 
    mutate(timegroupsf = factor(timegroupsf, 
                                levels = soib_year_info("timegroup_lab", "FALSE"))) %>%
    complete(timegroupsf) %>% 
    arrange(timegroupsf) %>%
    suppressMessages()
  
  # Years to project for IUCN comparison
  extra.years = soib_year_info("iucn_projection")
  
  f1 = f1 %>%
    mutate(COMMON.NAME = species) %>%
    mutate(lci = clogloglink(mean_trans - 1.96*se_trans, inverse = T),
           mean = clogloglink(mean_trans, inverse = T),
           rci = clogloglink(mean_trans + 1.96*se_trans, inverse = T)) %>%
    ungroup()
  
  modtrends = na.omit(f1) %>% # NAs are all spp. not included in long-term
    # _trans are link-scale, "mean" is back-transformed
    mutate(m1 = first(mean_trans),
           mean_year1 = first(mean),
           s1 = first(se_trans)) %>% 
    ungroup() %>% 
    # for calculating change in abundance index (as % change)
    mutate(mean_std = 100*mean/mean_year1) # back-transformed so value is % of year1 value
  
  # "main" simulated CIs
  set.seed(10) 
  modtrends = modtrends %>% 
    # calculating CIs
    group_by(timegroups) %>% 
    # 1000 simulations of transformed ratio of present:original values
    # quantiles*100 from these gives us our CI limits for mean_std
    reframe(tp0 = simerrordiv(mean_trans, m1, se_trans, s1)$rat) %>% 
    group_by(timegroups) %>% 
    reframe(lci_std = 100*as.numeric(quantile(tp0, 0.025)),
            rci_std = 100*as.numeric(quantile(tp0, 0.975))) %>% 
    right_join(modtrends, by = c("timegroups"))
  
  if (c == 1)
  {
    temp = modtrends
  }
  
  if (c > 1)
  {
    temp = temp %>%
      rbind(modtrends)
  }
  
  spec <- "bldr"
  
  write.csv(
    temp,
    glue("01_analyses_full/results/new_method_results_expanded_{spec}.csv"),
    row.names = FALSE
  )
}

