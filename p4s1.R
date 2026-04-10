# PART 4 (resolve) ------------------------------------------------------------------

# STEP 1: Resolve trends & occupancy for all selected species
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

source("00_scripts/00_functions.R")


# Is the current run for a new major SoIB version (every 3-4 years), 
# or for an interannual update (every year between major versions)?
# testing git
interannual_update = TRUE

load("00_data/analyses_metadata.RData")

analyses_metadata_temp <- analyses_metadata %>% filter (MASK %in% c("woodland", "cropland", "ONEland", "PA"))

tic.clearlog()
tic("Resolved trends & occupancy for all 37 state masks")
# full-country takes 5 h 11 min; woodland 2 h 10 min; PA 3 h 30 min

print(glue("Activated future-walking using advanced Kenbunshoku Haki!"))

# start multiworker parallel session
plan(multisession, workers = parallel::detectCores()/2)

analyses_metadata_temp %>%
  pull(MASK) %>% 
  # future-walking over each mask
  future_walk(.progress = TRUE, .options = furrr_options(seed = TRUE), ~ {
    
    # new environment for each parallel iteration
    cur_env <- new.env()
    assign("cur_mask", .x, envir = cur_env)
    assign("interannual_update", interannual_update, envir = cur_env)
    
    tic(glue("Resolved trends & occupancy for {.x}"))
    source("00_scripts/resolve_trends_and_occupancy.R", local = cur_env)
    toc()
    
  })

# end multiworker parallel session
plan(sequential)

toc(log = TRUE, quiet = TRUE) 
tic.log()