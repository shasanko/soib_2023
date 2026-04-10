mask_type = "country"
which_mask = NULL
#which_spec = "Bar-headed Goose"

library(tidyverse)
library(glue)
library(tictoc)

source('00_scripts/00_functions.R')
source('00_scripts/00_plot_functions_2025_rerun.R')


# error checks ----------------------------------------------------------------------

which_metadata <- get_metadata() %>% filter(MASK.TYPE == mask_type)

if (!mask_type %in% c("country", "state")) {
  return("Select either a country or a state! Range maps cannot be created for habitat or CA masks.")
}

# if both mask type and mask specified, avoid mismatches
if (!is.null(which_mask)) {
  
  list_states <- get_metadata() %>% filter(MASK.TYPE == "state") %>% pull(MASK)
  
  if ((mask_type == "country" & which_mask != "none") |
      (mask_type == "state" & !(which_mask %in% list_states))) {
    return("Incorrect 'which_mask' specification! Leave as default (NULL), or: Mask type 'country' needs to be paired with mask 'none'. Mask type 'state' needs to be paired with a valid state mask.")
  }
  
}


# setup + map creation --------------------------------------------------------------

if (is.null(which_mask)) {
  
  which_mask <- which_metadata %>% 
    distinct(MASK) %>% 
    pull(MASK)
  
  tic(glue("Finished plotting {mask_type} maps for all relevant masks."))
  
} else {
  
  tic(glue("Finished plotting {mask_type} maps for specified mask {which_mask}."))
  
}

which_metadata <- get_metadata("none")

# input paths
path_speclists <- which_metadata$SPECLISTDATA.PATH
path_main <- which_metadata$SOIBMAIN.PATH
path_toplot <- which_metadata$MAP.DATA.OCC.PATH
path_vagrants <- which_metadata$MAP.DATA.VAG.PATH
path_occu_pres <- which_metadata$OCCU.PRES.PATHONLY
path_occu_mod <- which_metadata$OCCU.MOD.PATHONLY

load(path_speclists)
load("00_data/vagrantdata.RData")
load("00_data/dataforanalyses_extra.RData")

load("00_data/grids_sf_nb.RData")
our_neighbours <- g1_nb_q
rm(g1_nb_r, g2_nb_q, g2_nb_r, g3_nb_q, g3_nb_r, g4_nb_q, g4_nb_r)


list_mig = read.csv(path_main) %>% 
  dplyr::select(eBird.English.Name.2024, Migratory.Status.Within.India) %>%
  filter(Migratory.Status.Within.India != "Resident", 
         eBird.English.Name.2024 %in% specieslist$COMMON.NAME) %>%
  distinct(eBird.English.Name.2024) %>% 
  pull(eBird.English.Name.2024)


# setup -----------------------------------------------------------------------------

# later for plotting state-level range maps, we need info on which grids each species 
# has been reported from. saving that information here, to be read in in plotting.
info_state_spec_grid <- data0 %>% 
  distinct(ST_NM, COMMON.NAME, gridg1) %>% 
  arrange(ST_NM, COMMON.NAME, gridg1)

save(info_state_spec_grid, file = "01_analyses_full/data_rangemap_info4state.RData")



data0 = data0 %>% 
  filter(COMMON.NAME %in% list_mig, 
         year > (soib_year_info("latest_year") - 5)) %>% 
  dplyr::select(COMMON.NAME, day, gridg1)

# defining summer, winter, passage
datas = data0 %>% 
  filter(day > 145 & day <= 215) %>% 
  distinct(COMMON.NAME, gridg1) %>%
  mutate(status = "S")
dataw = data0 %>% 
  filter(day <= 60 | day > 325) %>% 
  distinct(COMMON.NAME, gridg1) %>%
  mutate(status = "W")
datap = data0 %>% 
  filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) %>% 
  distinct(COMMON.NAME, gridg1) %>%
  mutate(status = "P")

data_presence = datas %>% 
  bind_rows(dataw, datap) %>%
  dplyr::select(COMMON.NAME, status, gridg1) %>%
  mutate(prop_nb = NA, 
         occupancy = 1,
         gridg1 = as.numeric(gridg1),
         status = factor(status, levels = c("S","W","P")))


# vagrants

d = d %>% 
  filter(COMMON.NAME %in% list_mig, 
         year > (soib_year_info("latest_year") - 5)) %>% 
  # subset for state when required
  dplyr::select(COMMON.NAME, day, LATITUDE, LONGITUDE)

ds = d %>% 
  filter(day > 145 & day <= 215) %>% 
  distinct(COMMON.NAME, LATITUDE, LONGITUDE) %>%
  mutate(status = "S")
dw = d %>% 
  filter(day <= 60 | day > 325) %>% 
  distinct(COMMON.NAME, LATITUDE, LONGITUDE) %>%
  mutate(status = "W")
dp = d %>% 
  filter((day > 60 & day <= 145) | (day > 215 & day <= 325)) %>% 
  distinct(COMMON.NAME, LATITUDE, LONGITUDE) %>%
  mutate(status = "P")

vagrant_presence = ds %>% bind_rows(dw, dp)


# the following code is to create a file that provides the proportional occupancy
# for each cell in each relevant season for each species
# occ.model files 
occ.model <- list.files(path = path_occu_mod, full.names = T) %>% 
  map_df(read.csv)

occ.model.resident = occ.model %>%
  filter(prop_nb != 0, 
         presence == 0, 
         !COMMON.NAME %in% list_mig) %>%
  dplyr::select(COMMON.NAME, status, gridg1, prop_nb, occupancy)

occ.model.migrant = occ.model %>%
  filter(prop_nb != 0, 
         presence == 0, 
         COMMON.NAME %in% list_mig) %>%
  dplyr::select(COMMON.NAME, status, gridg1, prop_nb, occupancy) %>%
  mutate(status = NA)


# occ.presence files 
occ.presence <- list.files(path = path_occu_pres, full.names = T) %>% 
  map_df(read.csv)

listM = occ.presence %>%
  filter(status == "M") %>% 
  distinct(COMMON.NAME) %>%
  pull(COMMON.NAME)

occ.presence.resident = occ.presence %>%
  filter(!COMMON.NAME %in% list_mig) %>%
  dplyr::select(COMMON.NAME, status, gridg1) %>%
  mutate(prop_nb = NA, occupancy = 1)


# combining presence-based and modelled for residents
occ.resident = occ.model.resident %>% bind_rows(occ.presence.resident)

# combining presence-based and modelled for migrants
# this is to determine the migratory status of each cell without confirmed presence
# the most frequent neighbouring status will be the assigned status for that cell
for (i in list_mig) {
  #i <- "Bar-headed Goose"
  temp = occ.model.migrant %>% filter(COMMON.NAME == i)
  temp.dat = data_presence %>% filter(COMMON.NAME == i)
  
  if (length(temp$COMMON.NAME) > 0) {
    
    for (j in unique(temp$gridg1))
    {
      #j <- 10940
      nbs = our_neighbours[[j]]
      
      stats = temp.dat %>%
        filter(gridg1 %in% nbs) %>%
        count(status, sort = TRUE) %>%
        slice_max(n, with_ties = FALSE) %>%
        pull(status)
      
      if (length(stats) == 0) {
        message("EMPTY stats for i = ", i, ", j = ", j)
        next
      }
      
      occ.model.migrant$status[occ.model.migrant$COMMON.NAME == i & 
                                 occ.model.migrant$gridg1 == j] = as.character(stats)
    }
    
  }
  
  print(i)
  
}

occ.migrant = occ.model.migrant %>% bind_rows(data_presence)


occ.full = occ.resident %>%
  bind_rows(occ.migrant) %>%
  # remove duplicates that come in from incidentals
  group_by(COMMON.NAME, status, gridg1) %>%
  arrange(desc(occupancy), .by_group = T) %>% 
  slice(1) %>% 
  ungroup() %>%
  arrange(COMMON.NAME, status, desc(occupancy))

# Change everything to the four statuses of interest
occ_final = occ.full %>%
  group_by(COMMON.NAME, gridg1) %>%
  reframe(occupancy = max(occupancy),
          status = case_when(
            any(status == "R") ~ "YR",
            any(status == "S") & any(status == "W") & !COMMON.NAME %in% listM ~ "YR",
            any(status == "S") ~ "S",
            any(status == "W") ~ "W",
            TRUE ~ "P"
          )) %>% 
  distinct(COMMON.NAME, gridg1, status, occupancy)


# write
write.csv(occ_final, path_toplot, row.names = FALSE)
write.csv(vagrant_presence, path_vagrants, row.names = FALSE)