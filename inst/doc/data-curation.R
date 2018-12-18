## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo     = FALSE,
  cache    = FALSE,
  comment  = "#>"
)

suppressPackageStartupMessages(library(shapefiles))
suppressPackageStartupMessages(library(STRIPSyield))

describe_shapefile <- function(shape) {
  describe_column <- function(shape, columnName) {
    column <- shape$dbf$dbf[, columnName]
    data.frame(
      name     = columnName,
      class    = class(column)[1],
      nFactors = if (is.factor(column)) { length(unique(column)) } else { NA },
      min      = if (is.factor(column)) { NA } else { min(column)  },
      mean     = if (is.factor(column)) { NA } else { mean(column) },
      max      = if (is.factor(column)) { NA } else { max(column)  },
      example  = trimws(as.character(column[1]))
    )
  }

  do.call(
    rbind,
    lapply(colnames(shape$dbf$dbf), describe_column, shape = shape)
  )
}

describe_yield <- function(yield) {
  describe_column <- function(yield, columnName) {
    column <- yield[, columnName]
    data.frame(
      name     = columnName,
      class    = class(column)[1],
      nFactors = if (is.factor(column)) { length(unique(column)) } else { NA },
      min      = if (is.factor(column)) { NA } else { min(column)  },
      mean     = if (is.factor(column)) { NA } else { mean(column) },
      max      = if (is.factor(column)) { NA } else { max(column)  },
      example  = trimws(as.character(column[1]))
    )
  }

  do.call(
    rbind,
    lapply(colnames(yield), describe_column, yield = yield)
  )
}

## ------------------------------------------------------------------------
dYield <- describe_yield(yieldExtra)
units  <- rep("Unknown", nrow(dYield))
units  <- c(NA, NA, NA, "Hectare", NA, "% (hundreds)", NA, "% (hundreds)", NA, NA, "Unknown", "Integer", "Date", "POSIX", "Unknown", "Unknown", "Feet", "MPH", "Degrees", "Unknown", "Unknown", "% (hundreds)", "Unknown")
extra  <- ifelse(colnames(yieldExtra) %in% colnames(yield), "Yes", "No")

k <- knitr::kable(
  cbind(dYield, units = units, "extra only" = extra), 
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ------------------------------------------------------------------------
shape2009  <- read.shapefile("../data-raw/yield_original/2009-basswood")
dShape2009 <- describe_shapefile(shape2009)

k <- knitr::kable(
  dShape2009, 
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ------------------------------------------------------------------------
actions <- c("Rename", "Drop", "TBD", "Reformat")
action  <- c(1, 1, 1, 3, 4, 3, 1, 2, 1, 2, 3, 4, 1, 4, 1, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3)

k <- knitr::kable(
  cbind(name = as.character(dShape2009$name), action = actions[action]),
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k)

## ------------------------------------------------------------------------
shape2015  <- read.shapefile("../data-raw/yield_original/2015-basswood")
dShape2015 <- describe_shapefile(shape2015)

k <- knitr::kable(
  dShape2015, 
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ------------------------------------------------------------------------
actions <- c("Rename", "Drop", "TBD", "Reformat")
action  <- c(4, 2, 4, 1, 1, 1, 4, 1, 1, 4, 1, 4, 1, 1, 3, 3, 3, 3, 4, 4, 4, 4, 4, 2, 2, 2, 2, 2, 2, 2, 1, 3, 3, 4, 4)

k <- knitr::kable(
  cbind(name = as.character(dShape2015$name), action = actions[action]),
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k)

## ------------------------------------------------------------------------
shape2015  <- read.shapefile("../data-raw/yield_curated/2015-basswood")
dShape2015 <- describe_shapefile(shape2015)
units      <- c(NA, NA, "Unknown", "Integer", "Datetime", "POSIXct", "Unknown", "Unknown", "Unknown (feets?)", "MPH", "Degrees", "Unknown", "Unknown", "% (hundreds)", "Unknown")

k <- knitr::kable(
  cbind(dShape2015[, 1:2], units),
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k)

