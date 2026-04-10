require(tidyverse)

source("00_scripts/02_generate_plots_2025_rerun.R")
source("00_scripts/00_plot_functions_2025_rerun.R")

# single-species for full country ---------------------------------------------------

# gen_trend_plots("single", "LTT", "Ashy Prinia") 
gen_trend_plots("single", "LTT") # 25 mins (<10 min with advanced Kenbunshoku Haki; 85 sec on server)
gen_trend_plots("single", "CAT", "Fulvous Whistling-Duck") # 20.5 mins (11.3 min with advanced Kenbunshoku Haki; 70 sec on server)

# single-species masks vs country ---------------------------------------------------

# 2.8 sec per species * no.sp * (no. masks or states)
gen_trend_plots("single_mask", "LTT") 
# 1.8 sec per species * no.sp * (no. masks or states)
gen_trend_plots("single_mask", "CAT") 

# multiple species plots ------------------------------------------------------------

gen_trend_plots("multi") # 30 sec

# composite plots -------------------------------------------------------------------

gen_trend_plots("composite") # 15 sec

# range maps -------------------------------------------------------------------------

# only for full country and states

# soib_rangemap("Oriental Dwarf Kingfisher") # testing error check
# soib_rangemap("Hair-crested Drongo") # individual species

gen_range_maps("country") # 38 min
gen_range_maps("state") # 2.5 h

# insufficient data plot ------------------------------------------------------------

soib_trend_plot("stamp")
