library(tidyverse)

data_rangemap_toplot_2023 <- read.csv("01_analyses_full/data_rangemap_toplot_2023_report.csv", 
                                      header = T)

data_rangemap_toplot_2025 <- read.csv("01_analyses_full/data_rangemap_toplot.csv", 
                                      header = T)

# Area of the grids

load("00_data/maps_sf.RData")

grid_area <- g1_in_sf %>%
  st_drop_geometry() %>%
  transmute(gridg1 = GRID.G1, area = AREA.G1)

grid_area$gridg1 <- as.numeric(grid_area$gridg1)

data_rangemap_toplot_2023_area <- left_join(data_rangemap_toplot_2023, 
                                            grid_area)

data_rangemap_toplot_2023_area$range_size <- data_rangemap_toplot_2023_area$occupancy*
  data_rangemap_toplot_2023_area$area

data_rangemap_toplot_2025_area <- left_join(data_rangemap_toplot_2025, 
                                            grid_area)

data_rangemap_toplot_2025_area$range_size <- data_rangemap_toplot_2025_area$occupancy*
  data_rangemap_toplot_2025_area$area

# Range size of Eurasian Kestrel

euke_range_size_2023 <- data_rangemap_toplot_2023_area %>% 
  filter(COMMON.NAME == "Eurasian Kestrel" & status == 'W') %>%
  summarise(range = sum(range_size))


euke_range_size_2025 <- data_rangemap_toplot_2025_area %>% 
  filter(COMMON.NAME == "Eurasian Kestrel" & status == 'W') %>%
  summarise(range = sum(range_size))
