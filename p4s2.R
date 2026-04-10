# PART 4 (resolve) ------------------------------------------------------------------

# STEP 2: Resolve trends & occupancy for all selected species
# Run:
# - after above steps (P3, S1-2)
# Requires:
# - tidyverse, tictoc, sf, VGAM, writexl
# - data files:
#   - fullspecieslist.csv
#   - trends/trendsX.csv for whole country and individual mask versions
# Outputs: several

library(tidyverse)
library(glue)
library(tictoc)
# for parallel iterations
library(furrr)
library(parallel)

priority_update <- TRUE
interannual_update <- TRUE

if(priority_update == TRUE)
{
  source("00_scripts/00_functions.R")
  load("00_data/analyses_metadata.RData")
  
  analyses_metadata_temp <- analyses_metadata %>% filter (MASK != "none")
  
  tic.clearlog()
  tic("Finished classifying and summarising for all masks") # 2 min
  
  analyses_metadata_temp %>% 
    pull(MASK) %>% 
    # walking over each mask
    walk(., ~ {
      
      assign("cur_mask", .x, envir = .GlobalEnv)
      
      tic(glue("Finished classifying and summarising for {.x}"))
      source("00_scripts/classify_and_summarise.R")
      toc(log = TRUE)
      
    })
  
  toc(log = TRUE, quiet = TRUE) 
  tic.log()  
} else {
  message("priority_update is FALSE — skipping classification and summarisation.")
}
