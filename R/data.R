#' Crop yield data data
#'
#' Average crop yield for all watersheds from 2007-2016
#'
#' @format A data.frame with 4 variables: \code{year}, \code{watershed},
#' \code{crop}, and \code{dryyield_buac} (bushels per acre)
#'
#' @seealso \code{\link{yield_conversion}}
#'
#' @examples
#' Convert from bushels per acre to Megagrams per hectare
#' library(dplyr)
#' yield <- yield %>%
#'   left_join(yield_conversion, by="crop") %>%
#'   mutate(yield_Mgha = dryyield_buac * Mgha_per_buac)
"yield"

#' Crop yield conversions
#'
#' Conversions from bushels/acre to Mg/ha for corn and soybean
#'
#' @format A data.frame with two variables: \code{crop}, \code{lbs_per_bushel},
#'  and \code{Mgha_per_buac}.
#'
#' @seealso \code{\link{yield}}
#'
#' @examples
#' Convert from bushels per acre to Megagrams per hectare
#' library(dplyr)
#' yield <- yield %>%
#'   left_join(yield_conversion, by="crop") %>%
#'   mutate(yield_Mgha = dryyield_buac * Mgha_per_buac)
"yield_conversion"
