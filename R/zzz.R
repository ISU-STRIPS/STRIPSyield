#' @details
#' An R package containing STRIPS yield data.
#' To access the dataset from the PNAS paper [1], please run `pnas_data()`.
#' To learn more about the dataset, see the data curation vignette: `vignette('data-curation')`.
#' [1] Lisa A. Schulte, Jarad B. Niemi, Matthew J. Helmers, Matt Liebman, J. G. Arbuckle, David E. James, Randall K. Kolka, Matthew E. O’Neal, Mark D. Tomer, John C. Tyndall, Heidi Asbjornsen, Pauline Drobney, Jeri Neal, Gary Van Ryswyk, and Chris Witte. (2017) “Prairie strips improve biodiversity and the delivery of multiple ecosystem services from corn-soybean croplands” Proceedings of the National Academy of Sciences, 114(42), 11247-11252. http://www.pnas.org/content/114/42/11247.short
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    sprintf(
      "Run vignette('data-curation') to learn more about the dataset, or run pnas_data() to access the data for the PNAS paper.",
      utils::packageDescription("STRIPSyield")$Version
    )
  )
}
