% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legacy.R
\docType{data}
\name{legacy_yield}
\alias{legacy_yield}
\title{Crop yield data (legacy from v.0.1.1)}
\format{A data.frame with 4 variables: \code{year}, \code{watershed},
\code{crop}, and \code{dryyield_buac} (bushels per acre)}
\usage{
legacy_yield
}
\description{
Average crop yield for all watersheds from 2007-2016
}
\examples{
# Convert from bushels per acre to Megagrams per hectare
library(dplyr)
legacy_yield <- legacy_yield \%>\%
  left_join(legacy_yield_conversion, by="crop") \%>\%
  mutate(yield_Mgha = dryyield_buac * Mgha_per_buac)
}
\seealso{
\code{\link{legacy_yield_conversion}}
}
\keyword{datasets}
