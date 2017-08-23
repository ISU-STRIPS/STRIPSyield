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
  yield %>%
    left_join(yield_conversion, by="crop") %>%
    rename(response = crop) %>%
    left_join(STRIPSMeta::watersheds, by="watershed") %>%
    mutate(crop_prop = 1-prairie_pct/100) %>%
    mutate(value = crop_prop * dryyield_buac * Mgha_per_buac,
           PI = "yield", source = "yield") %>%
    select(PI, source, watershed, year, response, value)
}
