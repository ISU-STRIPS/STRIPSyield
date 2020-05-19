## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo     = FALSE,
  cache    = FALSE,
  comment  = "#>"
)

suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(shapefiles))
suppressPackageStartupMessages(library(STRIPSyield))
suppressPackageStartupMessages(library(reshape2))

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

## ----yieldExtra-description, eval = FALSE--------------------------------
#  dYield <- describe_yield(yieldExtra)
#  units  <- c(
#    NA, NA, NA, "Hectare", NA, "Hectare", NA, "% (hundreds)", NA,
#    "% (hundreds)", NA, NA, "Feet", "Integer", NA,
#    NA, NA, "LatLong", "LatLong", "UTM", "UTM", NA, "Feet", "MPH", "Degrees",
#    "Feet", "Seconds", "Pounds per second", "% (hundreds)",
#    "Pounds (dry)", "Pounds (std)", "Pounds (wet)",
#    "Kilograms (dry)", "Kilograms (std)", "Kilograms (wet)",
#    "Bushels per acre (dry)", "Bushels per acre (std)", "Bushels per acre (wet)",
#    "Megagrams per hectare (dry)", "Megagrams per hectare (std)",
#    "Megagrams per hectare (wet)"
#  )
#  extra <- ifelse(colnames(yieldExtra) %in% colnames(yield), "Yes", "No")
#  colNames <- c(
#    "Name", "Class", "# of levels", "Min", "Mean", "Max", "Example"
#  )
#  
#  tab <- cbind(dYield, units = units, "extra only" = extra)
#  colnames(tab) <- c(colNames, "Measurement unit", "yieldExtra only")
#  
#  k <- knitr::kable(
#    tab,
#    format = "latex", booktabs = TRUE,
#    label = "yieldExtra-description",
#    caption = "Structure and content of the yield and yieldExtra datasets.",
#    digits = 2, format.args = list(scientific = FALSE)
#  )
#  
#  kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ----2009-basswood-description, eval = FALSE-----------------------------
#  shape2009  <- read.shapefile("../data-raw/yield_original/2009-basswood")
#  dShape2009 <- describe_shapefile(shape2009)
#  
#  tab <- dShape2009
#  colnames(tab) <- colNames
#  k <- knitr::kable(
#    tab,
#    format = "latex", booktabs = TRUE,
#    digits = 2, format.args = list(scientific = FALSE),
#    caption = "Structure and content of the Basswood 2015 original shapefile.",
#    label = "2009-basswood-description"
#  )
#  
#  kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ----2015-basswood-description, eval = FALSE-----------------------------
#  shape2015  <- read.shapefile("../data-raw/yield_original/2015-basswood")
#  dShape2015 <- describe_shapefile(shape2015)
#  
#  tab <- dShape2015
#  colnames(tab) <- colNames
#  k <- knitr::kable(
#    tab,
#    format = "latex", booktabs = TRUE,
#    digits = 2, format.args = list(scientific = FALSE),
#    label = "2015-basswood-description",
#    caption = "Structure and content of the Basswood 2015 original shapefile."
#  )
#  
#  kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ----curated-shapefile-description, eval = FALSE-------------------------
#  shape2015  <- read.shapefile("../data-raw/yield_curated/2015-basswood")
#  dShape2015 <- describe_shapefile(shape2015)
#  units      <- c(
#    NA, NA, "Feet", "Integer", "Integer", NA, "WGS84", "WGS84", "Feet above sea",
#    "Miles per hour", "Arc degrees", "Feet", "Seconds", NA, "Pounds per second",
#    "% (hundreds)", "Dry bushels per acre"
#  )
#  
#  tab <- cbind(dShape2015[, 1:2], units)
#  colnames(tab) <- c("Name", "Class", "Measurement unit")
#  k <- knitr::kable(
#    tab,
#    format = "latex", booktabs = TRUE,
#    digits = 2, format.args = list(scientific = FALSE),
#      caption = "Structure a curated shapefile.",
#    label = "curated-shapefile-description"
#  )
#  
#  kableExtra::kable_styling(k)

## ----data-visualization--------------------------------------------------
plot_grid <- function(df, title = NULL) {
  p <- ggplot(df) +
  geom_violin(aes(x = year, y = value), draw_quantiles = c(0.5)) +
  facet_grid(
    rows = vars(variable),
    cols = vars(site),
    scales = "free"
  ) +
  labs(
    title = title,
    x     = "Year",
    y     = "Values"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x        = element_text(angle = 90, hjust = 1)
  )
  
  suppressWarnings(print(p))
}

keys <- c("site", "year", "crop", "record")
cols <- c(
  "swath", "elevation", "speed", "distance",
  "cycle", "flow", "moisture", "yieldDryBuAc"
)

yieldExtraLong <- melt(
  data = yieldExtra,
  id.vars = keys,
  measure.vars = cols
)

## ---- out.width = "\\textwidth", fig.width = 9, fig.height = 11, fig.cap = "\\label{fig:data-visualization-soybeans} Visualization of some selected `yieldExtra` data frame variables.", warning = FALSE----
plot_grid(
  df = yieldExtraLong[
    yieldExtraLong$site != "OffBounds" & 
    yieldExtraLong$crop == "Soybeans", 
  ],
  title = "Soybeans"
)

## ---- out.width = "\\textwidth", fig.width = 9, fig.height = 11, fig.cap = "\\label{fig:data-visualization-corn} Visualization of some selected `yieldExtra` data frame variables.", warning = FALSE----
plot_grid(
  df = yieldExtraLong[
    yieldExtraLong$site != "OffBounds" & 
    yieldExtraLong$crop == "Corn", 
  ],
  title = "Maize"
)

## ----yieldExtra-description, eval = TRUE---------------------------------
dYield <- describe_yield(yieldExtra)
units  <- c(
  NA, NA, NA, "Hectare", NA, "Hectare", NA, "% (hundreds)", NA, 
  "% (hundreds)", NA, NA, "Feet", "Integer", NA, 
  NA, NA, "LatLong", "LatLong", "UTM", "UTM", NA, "Feet", "MPH", "Degrees", 
  "Feet", "Seconds", "Pounds per second", "% (hundreds)", 
  "Pounds (dry)", "Pounds (std)", "Pounds (wet)",
  "Kilograms (dry)", "Kilograms (std)", "Kilograms (wet)",
  "Bushels per acre (dry)", "Bushels per acre (std)", "Bushels per acre (wet)",
  "Megagrams per hectare (dry)", "Megagrams per hectare (std)", 
  "Megagrams per hectare (wet)"
)
extra <- ifelse(colnames(yieldExtra) %in% colnames(yield), "Yes", "No")
colNames <- c(
  "Name", "Class", "# of levels", "Min", "Mean", "Max", "Example"
)

tab <- cbind(dYield, units = units, "extra only" = extra)
colnames(tab) <- c(colNames, "Measurement unit", "yieldExtra only")

k <- knitr::kable(
  tab,
  format = "latex", booktabs = TRUE,
  label = "yieldExtra-description",
  caption = "Structure and content of the yield and yieldExtra datasets.",
  digits = 2, format.args = list(scientific = FALSE)
)

kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ----2009-basswood-description, eval = TRUE------------------------------
shape2009  <- read.shapefile("../data-raw/yield_original/2009-basswood")
dShape2009 <- describe_shapefile(shape2009)

tab <- dShape2009
colnames(tab) <- colNames
k <- knitr::kable(
  tab, 
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE),
  caption = "Structure and content of the Basswood 2015 original shapefile.",
  label = "2009-basswood-description"
)

kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ----2015-basswood-description, eval = TRUE------------------------------
shape2015  <- read.shapefile("../data-raw/yield_original/2015-basswood")
dShape2015 <- describe_shapefile(shape2015)

tab <- dShape2015
colnames(tab) <- colNames
k <- knitr::kable(
  tab, 
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE),
  label = "2015-basswood-description",
  caption = "Structure and content of the Basswood 2015 original shapefile."
)

kableExtra::kable_styling(k, latex_options = c("scale_down"))

## ----curated-shapefile-description, eval = TRUE--------------------------
shape2015  <- read.shapefile("../data-raw/yield_curated/2015-basswood")
dShape2015 <- describe_shapefile(shape2015)
units      <- c(
  NA, NA, "Feet", "Integer", "Integer", NA, "WGS84", "WGS84", "Feet above sea", 
  "Miles per hour", "Arc degrees", "Feet", "Seconds", NA, "Pounds per second", 
  "% (hundreds)", "Dry bushels per acre"
)

tab <- cbind(dShape2015[, 1:2], units)
colnames(tab) <- c("Name", "Class", "Measurement unit")
k <- knitr::kable(
  tab,
  format = "latex", booktabs = TRUE,
  digits = 2, format.args = list(scientific = FALSE),
    caption = "Structure a curated shapefile.",
  label = "curated-shapefile-description"
)

kableExtra::kable_styling(k)

