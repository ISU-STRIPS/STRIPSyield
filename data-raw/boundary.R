#' Write a curated shapefile to the disk. This function changes the projection of the boundary shapefiles using the following proj4 string: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0\.
#'
#' @param fileIn A character vector with the filename of the original shapefile.
#' @param fileout A character vector with the desired filename of the curated shapefile.
#' @return Nothing.
#' @author Luis Damiano
#' @example curate_boundary_shapefile("data-raw/boundaries_original/basswood1", "data-raw/boundaries_curated/basswood1")
curate_boundary_shapefile <- function(fileIn, fileOut) {
  fileIn  <- sprintf("%s.shp", strip_extension(fileIn))
  fileOut <- sprintf("%s.shp", strip_extension(fileOut))
  crs     <- "\"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0\""
  cmd     <- "ogr2ogr -f \"ESRI Shapefile\" -t_srs %s %s %s"
  system(sprintf(cmd, crs, fileOut, fileIn))
}

#' Run the curation protocol (see ?curation) on one or more boundary shapefiles.
#'
#' @param pathIn A character vector with the filename of the original shapefile, or a path to a directory with one or more shapefiles.
#' @param pathOut A character vector with the desired filename of the curated shapefile, or a path to a directory where the curated files should be written.
#' @param recursive If TRUE (default), the listing recurses into directories.
#' @param verbose If TRUE (default), the function will print to the console information about the progress.
#' @return Nothing.
#' @export
#' @examples curate_all_boundary_shapefiles("data-raw/boundaries_original", "data-raw/boundaries_curated")
curate_all_boundary_shapefiles <- function(pathIn, pathOut, recursive = TRUE, verbose = TRUE) {
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
      expr    = curate_boundary_shapefile(fileIn, fileOut),
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

#' Return a curated data.frame with the boundaries of the shapefile. This function may drop, modify headers, and modify the content of the original shape list.
#'
#' @param shape A shape list (e.g. one created by \code{\link{read_shapefile}}).
#' @return A data.frame.
#' @author Luis Damiano
curate_boundary_template <- function(shape) {
  dbf    <- shape$dbf$dbf
  shp    <- shape$shp$shp

  l <- lapply(seq_along(shp), function(i) {
    s <- shp[[i]]

    data.frame(
      x          = s$points$X,
      y          = s$points$Y,
      area       = dbf[i, ]$AREA,
      vegetation = curate_vegetation(dbf[i, ]$BNAME)
    )
  })

  do.call(rbind, l)
}

#' Creates a `boundary` data.frame from one or many shape lists.
#'
#' @param shapes A named list with a valid structure for a shapefile (e.g. one created by \code{\link{read_shapefile}}).
#' @return A data.frame.
#' @author Luis Damiano
build_boundaries <- function(shapes) {
  l     <- lapply(seq_along(shapes), function(i) {
    DF           <- curate_boundary_template(shapes[[i]])
    DF$watershed <- factor(curate_watershedname(names(shapes)[i]))
    DF
    DF[, c(5, 1:4)]
  })

  DF                     <- do.call(rbind, l)
  rownames(DF)           <- NULL
  attr(DF, "data_types") <- NULL

  DF
}
