library(dplyr)
library(tidyr)

# (v0.1.1) STRIPSyield/data-raw/raw_yield.R -------------------------------
legacy_raw_yield <- readr::read_csv("data-raw/legacy/AllSitesEdit.csv") %>%
  setNames(tolower(names(.))) %>%
  rename(year = year_) %>%
  select(-treatment,              # treatment is obtained from STRIPSMeta::watersheds
         -site,
         -id) %>%
  mutate(crop = plyr::revalue(crop, c('"SOYBEANS"' = "soybeans",
                                      '"CORN"' = "corn")))

devtools::use_data(legacy_raw_yield, overwrite = TRUE)

# (v0.1.1) STRIPSyield/data-raw/yield.R ----------------------------------
legacy_yield <- readr::read_csv("data-raw/legacy/STRIPS1YieldByWatershed2007_2015.csv") %>%
  select(-ends_with("PerMoist"))

names(legacy_yield) = tolower(names(legacy_yield))

legacy_yield <- legacy_yield %>%
  mutate(watershed = paste(site, plot,sep = "")) %>%
  select(year, watershed, crop, dryyield_buac)

devtools::use_data(legacy_yield, overwrite = TRUE)

# (v0.1.1)  STRIPSyield/data-raw/yield_conversion.R ---------------------
# Convert from bushels per acre to megagrams per hectare
legacy_yield_conversion = readr::read_csv("data-raw/legacy/lbs_per_bushel.csv")

Mgha_per_lbsac = .000453592* # Mg/lb
  2.47105                    # ac/ha

legacy_yield_conversion$Mgha_per_buac = Mgha_per_lbsac * legacy_yield_conversion$lbs_per_bushel

devtools::use_data(legacy_yield_conversion, overwrite = TRUE)
