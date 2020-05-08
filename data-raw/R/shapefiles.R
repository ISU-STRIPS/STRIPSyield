#' Remove the file extension from a filename.
#'
#' @param x A character vector, or a vector of character vectors.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example strip_extension("data-raw/original/2009-basswood.shp")
strip_extension <- function(x) {
  gsub("\\.shp$", "", x)
}

#' Read a shapefile stored in a path.
#'
#' @param filename A character string with the full path to the file.
#'
#' @return A named list with the components of a shapefile (may include `shp`, `shx`, `dbf`).
#' @export
#' @author Luis Damiano
#' @example shape <- read_shapefile("data-raw/original/2009-basswood")
read_shapefile <- function(filename) {
  shapefiles::read.shapefile(strip_extension(filename))
}

#' Write a named list to the disk in a shapefile format.
#'
#' @param shape A named list with a valid structure for a shapefile (e.g. one created by \code{\link{read_shapefile}}).
#' @param filename A character string with the full path to the target destination.
#'
#' @return Nothing.
#' @export
#' @author Luis Damiano
#' @example write_shapefile(shape, "data-raw/curated/2009-basswood")
write_shapefile <- function(shape, filename) {
  shapefiles::write.shapefile(shape, strip_extension(filename))
}

#' Return the names to the shapefiles stored in a path.
#'
#' @param path A character vector with the full path.
#' @param recursive If TRUE (default), the listing recurses into directories.
#'
#' @return A vector of character vectors containing the names of the shapefiles in the specified path.
#' @export
#' @author Luis Damiano
#' @example dir_shapefile("data-raw/original")
dir_shapefile <- function(path, recursive = TRUE) {
  pattern   <- "\\.shp$"

  dir(
    path       = path,
    pattern    = pattern,
    full.names = TRUE,
    recursive  = recursive
  )
}

#' Read all the shapefiles stored in a path.
#'
#' @param path A character vector with the full path.
#' @param recursive If TRUE (default), the listing recurses into directories.
#'
#' @return A named list where each element is itself a named list with the components of a shapefile (may include `shp`, `shx`, `dbf`).
#' @export
#' @author Luis Damiano
#' @example shapes <- read_all_shapefiles("data-raw/curated")
read_all_shapefiles <- function(path, recursive = TRUE) {
  pattern   <- "\\.shp$"

  filenames <- dir(
    path       = path,
    pattern    = pattern,
    full.names = TRUE,
    recursive  = recursive
  )

  filenames <- gsub(pattern, "", filenames)
  l         <- lapply(filenames, read_shapefile)
  names(l)  <- filenames

  l
}
