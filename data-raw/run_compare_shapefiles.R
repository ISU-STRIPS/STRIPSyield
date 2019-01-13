library(shapefiles)

# Settings ----------------------------------------------------------------
path <- "/home/luis/Downloads/STRIPS1 Research - NSNWR/"
outf <- "outShapefileComparison.txt"

# Auxiliary functions -----------------------------------------------------
identify_yield_dbf_template_1 <- function(dbf) {
  all(
    c(
      "TIME", "FIELD", "CROP", "SWATH", "ID", "LONGITUDE", "LATITUDE", "ALTITUDE",
      "SPEED", "DISTANCE", "TIMELAPSE", "FLOW", "MOISTURE", "DRY_BU_AC"
    ) %in% names(dbf)
  )
}

identify_yield_dbf_template_2 <- function(dbf) {
  all(
    c(
      "Field", "Product", "Swth_Wdth_", "Obj__Id", "Date", "Elevation_",
      "Speed_mph_", "Track_deg_", "Distance_f", "Duration_s", "Crop_Flw_M",
      "Moisture__", "Yld_Vol_Dr"
    ) %in% names(dbf)
  )
}

# Here we go --------------------------------------------------------------
sink(outf, split = TRUE)

filenames <- dir(path, ".shp$", recursive = TRUE, full.names = TRUE)
filenoext <- gsub("\\.shp$", "", filenames)

# Identify yield shapefiles
fileInd   <- do.call(
  c,
  lapply(seq_along(filenoext), function(i) {
    cat(
      sprintf("[%2d/%2d] %s\n", i, length(filenoext), filenoext[i])
    )

    tryCatch({
      # shape <- read.shapefile(filenoext[i])
      dbf   <- shapefiles::read.dbf(paste0(filenoext[i], ".dbf"))$dbf
      out   <- if (identify_yield_dbf_template_1(dbf)) {
        1
      } else if (identify_yield_dbf_template_2(dbf)) {
        2
      } else {
        0
      }

      return(out)
    }, error = function(e) {
      cat(warning(e))
      return(0)
    })
  })
)

# Compare
compare_files <- function(indKey, columnKey) {
  fileList <- filenoext[fileInd == indKey]
  ord      <- order(
    paste(
      sapply(regmatches(fileList, regexec("(\\d{4})", fileList)), unique),
      fileList
    )
  )
  fileList <- fileList[ord]
  fileMd5  <- as.vector(tools::md5sum(paste0(fileList, ".dbf")))

  l <- lapply(fileList, function(f) {
    tryCatch({
      read.shapefile(f)
    }, error = function(e) {
      cat(sprintf("Couldn't read %s.\n", f))
      return(NULL)
    })
  })

  grid <- expand.grid(j = seq_along(l), k = seq_along(l), result = NA)
  grid$filej <- fileList[grid[, "j"]]
  grid$filek <- fileList[grid[, "k"]]

  for (i in 1:nrow(grid)) {
    if (fileMd5[grid[i, "j"]] == fileMd5[grid[i, "k"]]) {
      next()
    }

    equal <- all.equal(
      l[[grid[i, "j"]]]$dbf$dbf[[columnKey]],
      l[[grid[i, "k"]]]$dbf$dbf[[columnKey]]
    )

    if (isTRUE(equal)) {
      cat(
        sprintf(
          "\n  [A %2d] %s %s\n  [B %2d] %s %s\n\n",
          grid[i, "j"],
          fileMd5[grid[i, "j"]],
          grid[i, "filej"],
          grid[i, "k"],
          fileMd5[grid[i, "k"]],
          grid[i, "filek"]
        )
      )

      dbfIn    <- l[[grid[i, "j"]]]$dbf$dbf
      dbfOut   <- l[[grid[i, "k"]]]$dbf$dbf

      for (name in names(dbfIn)) {
        if (!isTRUE(all.equal(dbfIn[[name]], dbfOut[[name]]))) {
          ind <- dbfIn[[name]] != dbfOut[[name]]
          r   <- as.numeric(dbfOut[[name]]) / as.numeric(dbfIn[[name]])
          str <- sprintf(
            "%10s %6d different rows (%6.2f%%). Mean out/in ratio: %5.2f (all), %5.2f (diff rows only).\n",
            name, sum(ind), 100 * sum(ind) / nrow(dbfIn), mean(r), mean(r[ind])
          )

          cat(str)
        }
      }
    }
  }
}

compare_files(1, "LONGITUDE")
compare_files(2, "Elevation_")

sink()
browseURL(outf)
