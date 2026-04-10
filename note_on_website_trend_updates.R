library(tidyverse)
temp <- SoIB_main %>% 
  filter(SoIB.Latest.Current.Status == "Insufficient Data") %>%
  pull(SoIB.Latest.Long.Term.Status) %>%
  unique()

temp <- SoIB_main %>% 
  filter(SoIB.Latest.Long.Term.Status == "Insufficient Data") %>%
  pull(SoIB.Latest.Current.Status) %>%
  unique()

temp <- SoIB_main %>% 
  filter(SoIB.Latest.Long.Term.Status == "Insufficient Data") %>%
  pull(SoIB.Major.Update.Long.Term.Status) %>%
  unique() # Insufficient Data

temp <- SoIB_main %>% 
  filter(SoIB.Latest.Current.Status == "Insufficient Data") %>%
  pull(SoIB.Major.Update.Current.Status) %>%
  unique() # Insufficient Data

# Pacific golden plover


