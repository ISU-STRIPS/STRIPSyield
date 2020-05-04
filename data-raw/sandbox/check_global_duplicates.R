# Check raw shapefiles for global duplicates
library(shapefiles)

getColumn <- function(df, colNames) {
  for (colName in colNames) {
    if (colName %in% colnames(df))
      return(df[, colName])
  }

  stop("None of the colNames matched.")
}

# https://stackoverflow.com/a/50797374
fast.rbind <- function(...,method=c("common","fill"),value=NA){
  if("fill"==method[1]) {
    fun1 <- function(x,y,value=NA){
      x[setdiff(colnames(y),colnames(x))] <- value

      y[setdiff(colnames(x),colnames(y))] <- value

      return(rbind(x,y))
    }
  }

  if("common"==method[1]) {
    fun1 <- function(x,y,value=NULL){
      common_cols <- intersect(colnames(x), colnames(y))
      return(rbind(x[, common_cols,drop=F],y[, common_cols,drop=F]))
    }
  }
  return(Reduce(function(x,y){fun1(x=x,y=y,value=value)},list(...)))
}

# Per year ----------------------------------------------------------------
years <- 2007:2018

for (year in years) {
  print(sprintf("YEAR %s", year))
  files <- dir(
    path       = "./data-raw/yield_original/",
    pattern    = glob2rx(sprintf("*%s*.dbf", year)),
    full.names = TRUE
  )

  df <- do.call(
    fast.rbind,
    pbapply::pblapply(tools::file_path_sans_ext(files), function(f) {
      shp <- read.shapefile(f)
      cbind(
        f = f,
        shp$dbf$dbf,
        shp$shp$shp
      )
    })
  )

  colNames <- if("Crop_Flw_M" %in% colnames(df)) {
    "Crop_Flw_M"
  } else {
    "FLOW"
  }

  print(df[duplicated(df[, c("x", "y", colNames)]), ])
}

