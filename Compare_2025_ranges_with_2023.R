# This script compares the estimated range sizes of species between 2023 and 2025 ----

soib_main_2023 <- read.csv("01_analyses_full/results/past/SoIB_main_2023.csv", 
                           header = T)

soib_main_2023_ranges <- soib_main_2023 %>% dplyr::select(eBird.English.Name.2022,
                                                   eBird.Scientific.Name.2022,
                                                   rangelci,
                                                   rangemean,
                                                   rangerci)

soib_main_2023_ranges <- soib_main_2023_ranges %>% rename(rangelci_2023 = rangelci,
                                                          rangemean_2023 = rangemean,
                                                          rangerci_2023 = rangerci)

soib_main_2025 <- read.csv("01_analyses_full/results/SoIB_main.csv", 
                           header = T)

soib_main_2025_ranges <- soib_main_2025 %>% dplyr::select(eBird.English.Name.2024,
                                                          rangelci,
                                                          rangemean,
                                                          rangerci)

soib_main_2025_ranges <- soib_main_2025_ranges %>% rename(rangelci_2025 = rangelci,
                                                          rangemean_2025 = rangemean,
                                                          rangerci_2025 = rangerci)


soib_main_2025_ranges_join <- soib_main_2025_ranges %>%
  left_join(soib_main_2023_ranges, by = c("eBird.English.Name.2024" = "eBird.English.Name.2022"))

soib_main_2025_ranges_join$diff_range <- soib_main_2025_ranges_join$rangemean_2025-
  soib_main_2025_ranges_join$rangemean_2023

hist(soib_main_2025_ranges_join$diff_range)
# For some species, the 2025 range size is smaller than the 2023 range size 

# What are these species?
range_contract <- soib_main_2025_ranges_join %>% filter(diff_range < 0) %>% 
  pull(eBird.English.Name.2024)

# Eurasian Kestrel has the largest difference between 2025 and 2023. 
# To investigate this further, I am going to use the data_rangemap_toplot.csv
# from 2023 and 2025. The 2023 file is from here: 
# https://drive.google.com/drive/folders/1sSPJgUD0t5OdTnMpQBt8tSjyfRusPHVX