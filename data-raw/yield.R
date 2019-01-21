#' Return a curated data.frame for the `dbf` element of a shape list. This function may drop, modify headers, and modify the content of the original shape list.
#'
#' @param shape A named list with a valid structure for a shapefile (e.g. one created by \code{\link{read_shapefile}}).
#' @return A data.frame.
#' @author Luis Damiano
#' @seealso \code{\link{curate_dbf}}
curate_yield_dbf_template1 <- function(shape) {
  dbf  <- shape$dbf$dbf
  t    <- as.POSIXct(dbf$TIME, origin = "1970-01-01")
  date <- as.Date(t)
  site <- curate_sitename(dbf$FIELD)
  crop <- curate_cropname(dbf$CROP)

  data.frame(
    site      = site,
    crop      = crop,
    swath     = dbf$SWATH,
    record    = dbf$ID,
    date      = date,
    x         = dbf$LONGITUDE,
    y         = dbf$LATITUDE,
    elevation = dbf$ALTITUDE,
    speed     = dbf$SPEED,
    direction = NA,
    distance  = dbf$DISTANCE,
    timelapse = dbf$TIMELAPSE,
    flow      = dbf$FLOW,
    moisture  = dbf$MOISTURE,
    yield     = dbf$DRY_BU_AC
  )
}

#' Return a curated data.frame for the `dbf` element of a shape list. This function may drop, modify headers, and modify the content of the original shape list.
#'
#' @param shape A named list with a valid structure for a shapefile (e.g. one created by \code{\link{read_shapefile}}).
#' @return A data.frame.
#' @author Luis Damiano
#' @seealso \code{\link{curate_dbf}}
curate_yield_dbf_template2 <- function(shape) {
  dbf  <- shape$dbf$dbf
  site <- curate_sitename(dbf$Field)
  crop <- curate_cropname(dbf$Product)

  data.frame(
    site      = site,
    crop      = crop,
    swath     = dbf$Swth_Wdth_,
    record    = dbf$Obj__Id,
    date      = dbf$Date,
    x         = shape$shp$shp[, "x"],
    y         = shape$shp$shp[, "y"],
    elevation = dbf$Elevation_,
    speed     = dbf$Speed_mph_,
    direction = dbf$Track_deg_,
    distance  = dbf$Distance_f,
    timelapse = dbf$Duration_s,
    flow      = dbf$Crop_Flw_M,
    moisture  = dbf$Moisture__,
    yield     = dbf$Yld_Vol_Dr
  )
}

#' Return a curated data.frame for the `dbf` element of a shape list. This function may drop, modify headers, and modify the content of the original shape list.
#'
#' @param shape A named list with a valid structure for a shapefile (e.g. one created by \code{\link{read_shapefile}}).
#' @return A data.frame.
#' @author Luis Damiano
curate_dbf <- function(shape) {
  colNames <- names(shape$dbf$dbf)

  curated  <- shape$dbf$dbf
  if (all(c("FIELD", "TIME", "LATITUDE", "DRY_BU_AC") %in% colNames))
    curated <- curate_yield_dbf_template1(shape)
  if (all(c("Product", "Elevation_", "Obj__Id", "Yld_Vol_Dr") %in% colNames))
    curated <- curate_yield_dbf_template2(shape)

  curated
}

#' Write a curated shapefile to the disk. This function may drop, modify headers, and modify the content of the original shape.
#'
#' @param fileIn A character vector with the filename of the original shapefile.
#' @param fileout A character vector with the desired filename of the curated shapefile.
#' @return Nothing.
#' @author Luis Damiano
#' @example curate_shapefile("data-raw/original/2009-basswood", "data-raw/curated/2009-basswood")
curate_shapefile <- function(fileIn, fileOut) {
  shape         <- read_shapefile(fileIn)
  shape$dbf$dbf <- curate_dbf(shape)

  write_shapefile(shape, fileOut)
}

#' Run the curation protocol (see ?curation) in one or more shapefiles/
#'
#' @param pathIn A character vector with the filename of the original shapefile, or a path to a directory with one or more shapefiles.
#' @param pathOut A character vector with the desired filename of the curated shapefile, or a path to a directory where the curated files should be written.
#' @param recursive If TRUE (default), the listing recurses into directories.
#' @param verbose If TRUE (default), the function will print to the console information about the progress.
#' @return Nothing.
#' @export
#' @examples curate_all_yield_shapefiles("data-raw/original", "data-raw/curated")
curate_all_yield_shapefiles <- function(pathIn, pathOut, recursive = TRUE, verbose = TRUE) {
  filenames <- strip_extension(dir_shapefile(pathIn, recursive))
  nFiles    <- length(filenames)
  nChar     <- max(sapply(filenames, nchar))

  if (verbose) {
    msg <- sprintf(
      "[ 0/%2d] %s Found %2d shapefiles in %s.\n",
      nFiles, format(Sys.time(), "%T"), nFiles, pathIn
    )
    cat(msg)
  }

  for (i in seq_along(filenames)) {
    fileIn  <- filenames[i]
    fileOut <- gsub(pathIn, pathOut, fileIn)

    if (verbose) {
      msg <- sprintf(
        "[%2d/%2d] %s %-*s -> %-*s ...",
        i, nFiles, format(Sys.time(), "%T"), nChar, fileIn, nChar, fileOut
      )
      cat(msg)
    }

    wMessage <- NULL
    tryCatch(
      expr    = curate_shapefile(fileIn, fileOut),
      warning = function(w) { wMessage <<- w },
      finally = if (verbose) { cat(" DONE.\n") }
    )

    if (verbose) {
      msg <- sprintf(
        "      ^ WARNING: %s\n",
        wMessage$message
      )
      cat(msg)
    }
  }
}

#' Creates a `yield` data.frame from one or many shape lists.
#'
#' @param shapes A named list with a valid structure for a shapefile (e.g. one created by \code{\link{read_shapefile}}).
#' @return A yield data.frame containing the following columns: site, year, crop, swath, record, date, x, y, elevation, speed, direction, distance, flow, moisture, yield.
build_yield <- function(shapes) {
  DF <- do.call(
    rbind,
    lapply(lapply(shapes, `[[`, "dbf"), `[[`, "dbf")
  )

  DF$year       <- factor(substr(DF$date, 0, 4))
  DF$year       <- postprocess_year(DF)
  DF$swath      <- postprocess_swath(DF)
  DF$distance   <- postprocess_distance(DF)
  DF$record     <- postprocess_record(DF)

  rownames(DF)           <- NULL
  attr(DF, "data_types") <- NULL

  rowOrder      <- order(DF$site, DF$year, DF$record)
  columnOrder   <- c(1, 16, 2:15)
  DF            <- DF[rowOrder, columnOrder]

  DF
}

#' Post processing rule for year
#' Replaces year "2004" for "2008".
#'
#' @param DF A yield data.frame containing the following columns:
#' site, year, crop, swath, record, date, x, y, elevation, speed,
#' direction, distance, flow, moisture, yield.
#' @return A character vector with the year.
postprocess_year <- function(DF) {
  levels(DF$year)[levels(DF$year) == "2004"] <- "2008"
  factor(DF$year, levels = sort(unique(as.character(DF$year))))
}

#' Post processing rule for swath width.
#' Rescale swath width from inches to foot for rows corresponding to
#' years 2007, 2008, 2009, 2009, 2010, 2012.
#'
#' @param DF A yield data.frame containing the following columns:
#' site, year, crop, swath, record, date, x, y, elevation, speed,
#' direction, distance, flow, moisture, yield.
#' @return A numeric vector with swath width in foot.
postprocess_swath <- function(DF) {
  ind <- DF$year %in% c("2007", "2008", "2009", "2010", "2012")
  ifelse(ind, DF$swath / 12, DF$swath)
}

#' Post processing rule for distance.
#' Rescale distance from inches to foot for rows corresponding to
#' years 2007, 2008, 2009, 2009, 2010, 2012.
#'
#' @param DF A yield data.frame containing the following columns:
#' site, year, crop, swath, record, date, x, y, elevation, speed,
#' direction, distance, flow, moisture, yield.
#' @return A numeric vector with distance in foot.
postprocess_distance <- function(DF) {
  ind <- DF$year %in% c("2007", "2008", "2009", "2010", "2012")
  ifelse(ind, DF$distance / 12, DF$distance)
}

#' Post processing rule for record.
#' Realign record index so that the index is always increasing
#' for any combination of year and site.
#'
#' @param DF A yield data.frame containing the following columns:
#' site, year, crop, swath, record, date, x, y, elevation, speed,
#' direction, distance, flow, moisture, yield.
#' @return A numeric vector with an increasing index.
postprocess_record <- function(DF) {
  # ind     <-
  #   DF$year %in% c("2013", "2014", "2015") & DF$site == "Orbweaver" |
  #   DF$year == "2014" & DF$site == "Interim" |
  #   DF$year == "2011" & DF$site == "Orbweaver"

  ind     <- seq_along(DF$record)
  splits  <- split(
    seq_along(DF$record[ind]),
    list(DF$site[ind], DF$year[ind]),
    drop = TRUE
  )

  records <- DF$record
  for (splitInd in splits) {
    breakPos <- which(diff(records[ind][splitInd]) < 0)
    for (pos in breakPos) {
      n      <- length(records[ind][splitInd])
      offset <- records[ind][splitInd][pos]
      records[ind][splitInd][(pos + 1):n] <-
        records[ind][splitInd][(pos + 1):n] + offset
    }
  }

  records
}
