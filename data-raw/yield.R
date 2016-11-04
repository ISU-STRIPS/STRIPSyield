library(dplyr)
library(tidyr)

yield = readr::read_csv("original/STRIPS1YieldByWatershed2007_2015.csv") %>%
  select(-ends_with("PerMoist"))

names(yield) = tolower(names(yield))

yield = yield %>%
  mutate(watershed = paste(site,plot,sep="")) %>%
  select(year, watershed, crop, dryyield_buac)

devtools::use_data(yield, overwrite = TRUE)
