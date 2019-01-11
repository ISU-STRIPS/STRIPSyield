#' Check the consistency between the time and timelapse fields.
#'
#' @param path A character vector with the filename of the shapefiles to check.
#' @return Nothing
#' @examples
#' check_timelapse("data-raw/yield_original/")
check_timelapse <- function(path) {
  dirs <- dir_shapefile(path)

  for (i in seq_along(filenames)) {
    shape <- read.shapefile(filenames[i])
    dbf   <- shape$dbf$dbf

    if ("TIMELAPSE" %in% names(dbf)) {
      str <- gsub("data-raw/yield_original/{1}", "\\1", filenames[i])
      t   <- c(0, as.numeric(diff(as.POSIXct(dbf$TIME, origin = "1970-01-01"))))
      print(
        sprintf(
          "Cor: %0.4f. %d rows with differences. %s",
          cor(t, dbf$TIMELAPSE), sum(t != dbf$TIMELAPSE), str
        )
      )
    }
  }

}
