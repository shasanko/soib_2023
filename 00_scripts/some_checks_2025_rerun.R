library(tidyverse)

colnames(SoIB_main)

# Any species with LTT absent in the latest update but present in 2023?
temp <- SoIB_main %>% 
  filter(SoIB.Latest.Long.Term.Status == "Insufficient Data") %>%
  pull(SoIB.Major.Update.Long.Term.Status) %>%
  unique()
# No

# Any species with CAT absent in the latest update but present in 2023?    
temp_1 <- SoIB_main %>% 
  filter(SoIB.Latest.Current.Status == "Insufficient Data") %>%
  pull(SoIB.Major.Update.Current.Status) %>%
  unique()
# No

# Any species with LTT and CAT absent in the latest update but CAT present in 2023? No ----
temp_2 <- SoIB_main %>% 
  filter(SoIB.Latest.Current.Status == "Insufficient Data" & SoIB.Latest.Long.Term.Status == "Insufficient Data") %>%
  pull(SoIB.Major.Update.Current.Status) %>%
  unique()

# Any species with CAT absent in the latest update but LTT present in 2023? No ----
temp_3 <- SoIB_main %>% 
  filter(SoIB.Latest.Current.Status == "Insufficient Data" & SoIB.Latest.Long.Term.Status == "Insufficient Data") %>%
  pull(SoIB.Major.Update.Long.Term.Status) %>%
  unique()

# Any species with CAT present in the latest update but LTT absent?  ----



# Any species with LTT absent in the 2023 but present in latest update?
temp_4 <- SoIB_main %>% 
  filter(SoIB.Major.Update.Long.Term.Status == "Insufficient Data" & SoIB.Latest.Long.Term.Status != "Insufficient Data") %>%
  pull(eBird.English.Name.2024) %>%
  unique()
# 7 species. If the priorities are not going to change, then we cannot offer a graph nor can we offer a download

# Any species with CAT absent in the 2023 but present in latest update?
temp_5 <- SoIB_main %>% 
  filter(SoIB.Major.Update.Current.Status == "Insufficient Data" & SoIB.Latest.Current.Status != "Insufficient Data") %>%
  pull(eBird.English.Name.2024) %>%
  unique()
#  8 species.

#  Of these 8 species, which of them have LTT in the latest update?

temp_6 <- SoIB_main %>% 
  filter(eBird.English.Name.2024 %in% temp_5 & SoIB.Latest.Long.Term.Status != "Insufficient Data") %>%
  pull(eBird.English.Name.2024)


# Any species with CAT present but LTT absent in the 2023 but CAT absent and LTT present in latest update?

temp_7 <- SoIB_main %>% 
  filter(SoIB.Major.Update.Current.Status != "Insufficient Data" & SoIB.Major.Update.Long.Term.Status == "Insufficient Data" &
           SoIB.Latest.Long.Term.Status != "Insufficient Data" & SoIB.Latest.Current.Status == "Insufficient Data" ) %>%
  pull(eBird.English.Name.2024)

# No species, just as I thought

temp_4_subset <- temp_4 %>% 
  dplyr::select(eBird.English.Name.2024,
                SoIB.Latest.Long.Term.Status,
  SoIB.Latest.Current.Status,
  SoIB.Major.Update.Long.Term.Status,
  SoIB.Major.Update.Current.Status
  )

temp_4_subset <- temp_4 %>% 
  dplyr::select(eBird.English.Name.2024,
                SoIB.Latest.Long.Term.Status,
                SoIB.Latest.Current.Status,
                SoIB.Major.Update.Long.Term.Status,
                SoIB.Major.Update.Current.Status
  )
