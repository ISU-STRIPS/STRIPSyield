# This is the only script that you should run manully to build the data objects to be distributed with the STRIPSYield package. It relies on the shapefiles as well as the many functions laid out other .R files stored in the data-raw folder. Note that the whole data-raw folder is not part of the user-facing part of the STRIPYield package: the raw shapefiles and these scripts are only meant to be used locally to build the data objects before the package is published. For further questions, see ?whatever.

# 0. Settings -------------------------------------------------------------
yieldPathIn     <- "data-raw/yield_original"    # Folder where the original shapefiles are stored
yieldPathOut    <- "data-raw/yield_curated"     # Folder where the curated shapefiles will be stored
boundaryPathIn  <- "data-raw/boundary_original" # Folder where the original shapefiles are stored
boundaryPathOut <- "data-raw/boundary_curated"  # Folder where the curated shapefiles will be stored
nCores          <- 10                           # Number of cores for point coordinates classification

# 1. Preamble -------------------------------------------------------------
suppressPackageStartupMessages(library(shapefiles))
source('data-raw/boundary.R')
source('data-raw/build.R')
source('data-raw/curate.R')
source('data-raw/shapefiles.R')
source('data-raw/yield.R')

# 2. Curate the shapefiles. -----------------------------------------------
if (!dir.exists(yieldPathOut))
  dir.create(yieldPathOut)

curate_all_yield_shapefiles(yieldPathIn, yieldPathOut)

# 3. Curate the boundary shapefiles ---------------------------------------
if (!dir.exists(boundaryPathOut))
  dir.create(boundaryPathOut)

curate_all_boundary_shapefiles(boundaryPathIn, boundaryPathOut)

# 3. Build master data.frame ----------------------------------------------
yieldShapes    <- read_all_shapefiles(yieldPathOut)
boundaryShapes <- read_all_shapefiles(boundaryPathOut)
yieldDF        <- build_yield(yieldShapes)
boundaryDF     <- build_boundaries(boundaryShapes)
yieldExtra     <- build_extra(yieldDF, boundaryDF)

warning(
  unique(yieldExtra[yieldExtra$watershed == "OffBounds", ]$year)
)

# 4. Create summary -------------------------------------------------------
yieldSummary   <- "TO DO"

# 5. Rename, subset and export --------------------------------------------
boundaries     <- boundaryDF
yield          <- yieldExtra[, c(1:3, 5, 9, 10, 15:16, 22:23)]

devtools::use_data(boundaries, overwrite = TRUE)
devtools::use_data(yield,      overwrite = TRUE)
devtools::use_data(yieldExtra, overwrite = TRUE)

# 6. Build legacy datasets ------------------------------------------------
source('data-raw/legacy.R')
