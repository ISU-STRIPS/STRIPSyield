#' Crop yield data (legacy from v.0.1.1)
#'
#' Average crop yield for all watersheds from 2007-2016
#'
#' @format A data.frame with 4 variables: \code{year}, \code{watershed},
#' \code{crop}, and \code{dryyield_buac} (bushels per acre)
#'
#' @seealso \code{\link{legacy_yield_conversion}}
#'
#' @examples
#' Convert from bushels per acre to Megagrams per hectare
#' library(dplyr)
#' legacy_ <- legacy_ %>%
#'   left_join(legacy_yield_conversion, by="crop") %>%
#'   mutate(yield_Mgha = dryyield_buac * Mgha_per_buac)
"legacy_yield"

#' Crop yield conversions (legacy from v.0.1.1)
#'
#' Conversions from bushels/acre to Mg/ha for corn and soybean
#'
#' @format A data.frame with two variables: \code{crop}, \code{lbs_per_bushel},
#'  and \code{Mgha_per_buac}.
#'
#' @seealso \code{\link{legacy_yield}}
#'
#' @examples
#' Convert from bushels per acre to Megagrams per hectare
#' library(dplyr)
#' legacy_yield <- legacy_yield %>%
#'   left_join(legacy_yield_conversion, by="crop") %>%
#'   mutate(yield_Mgha = dryyield_buac * Mgha_per_buac)
"legacy_yield_conversion"

#' Raw crop yield data (legacy from v.0.1.1)
#'
#' Raw crop yield
#'
#' @format A data.frame with 4 variables: \code{longitude}, \code{latitude},
#' \code{moisture}, \code{crop}, \code{altitude}, \code{dry_bu_ac}
#' (dry bushels per acre), \code{watershed}, and \code{year}
#'
#' @seealso \code{\link{legacy_yield}}
#'
"legacy_raw_yield"

#' Construct water quality data used in PNAS paper
#'
#' This function will summarize surface and ground water by watershed and
#' year combination for analysis in the PNAS paper.
#'
#' @return A \code{data.frame} containing the columns PI, source, watershed,
#' year, response, and value.
#' @import dplyr
#' @import STRIPSMeta
#' @export
#' @examples
#' d <- pnas_data()
#' summary(d)
#'
pnas_data <- function() {
  legacy_yield %>%
    left_join(legacy_yield_conversion, by = "crop") %>%
    rename(response = crop) %>%
    left_join(STRIPSMeta::watersheds, by = "watershed") %>%
    mutate(crop_prop = 1 - prairie_pct/100) %>%
    mutate(value = crop_prop * dryyield_buac * Mgha_per_buac,
           PI = "yield", source = "yield") %>%
    select(PI, source, watershed, year, response, value)
}
