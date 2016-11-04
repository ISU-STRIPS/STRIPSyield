# Convert from bushels per acre to megagrams per hectare
yield_conversion = readr::read_csv("original/lbs_per_bushel.csv")

Mgha_per_lbsac = .000453592* # Mg/lb
  2.47105                    # ac/ha

yield_conversion$Mgha_per_buac = Mgha_per_lbsac * yield_conversion$lbs_per_bushel

devtools::use_data(yield_conversion, overwrite = TRUE)
