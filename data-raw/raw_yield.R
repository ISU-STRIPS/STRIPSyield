library(dplyr)

raw_yield <- readr::read_csv("original/AllSitesEdit.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = year_) %>%
  select(-treatment,              # treatment is obtained from STRIPSMeta::watersheds
         -site,
         -id) %>%

  mutate(crop = plyr::revalue(crop, c('"SOYBEANS"' = "soybeans",
                                      '"CORN"' = "corn")))

devtools::use_data(raw_yield, overwrite = TRUE)
