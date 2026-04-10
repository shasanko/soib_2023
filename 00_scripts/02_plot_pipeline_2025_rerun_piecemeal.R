library(tidyverse)

interannual_update = TRUE

source("00_scripts/02_generate_plots_2025_rerun.R")
source("00_scripts/00_plot_functions_2025_rerun.R")

# single-species for full country ---------------------------------------------------

#gen_trend_plots("single", "LTT") # Done on Feb 5, 2026; 25 sec on the new server
#gen_trend_plots("single", "CAT") # Done on Feb 5, 2026; 26 sec on the new server

# single-species masks vs country ---------------------------------------------------

# 2.8 sec per species * no.sp * (no. masks or states)
#gen_trend_plots("single_mask", "LTT") 
# 1.8 sec per species * no.sp * (no. masks or states)
#gen_trend_plots("single_mask", "CAT") 


# multiple species plots ------------------------------------------------------------

#gen_trend_plots("multi")

# composite plots -------------------------------------------------------------------

#gen_trend_plots("composite") # 15 sec

# range maps -------------------------------------------------------------------------

# only for full country and states

# soib_rangemap("Oriental Dwarf Kingfisher") # testing error check
#soib_rangemap("Bar-headed Goose") # individual species

#gen_range_maps("country") # 487 sec
#gen_range_maps("state")
#gen_range_maps("state","Andaman and Nicobar Islands")
#gen_range_maps("state","Goa", "Nilgiri Flowerpecker")
#gen_range_maps("state", "Andaman and Nicobar Islands", "Common Snipe") # 46 min
#gen_range_maps("state", "Andaman and Nicobar Islands") # runs great
#gen_range_maps("state", "Andaman and Nicobar Islands", "Common Snipe")
#gen_range_maps("state", "Andhra Pradesh", "Indian Pitta")

# insufficient data plot ------------------------------------------------------------

soib_trend_plot("stamp")