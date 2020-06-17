#' Test whether coordinate points lie inside a polygon. Based on the PNPOLY
#' algorithm by W. Randolph Franklin (WRF) and rewritten in R. NOTE: The current
#' implementation does not test if the newData coordinate points lie exactly ON
#' the borders of the polygon.
#' https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
#' https://stackoverflow.com/a/2922778/2860744
#'
#' @param vertices A dtaa.frame with three named columns: x (numeric), y
#'   (numeric), label (factor). It should contain the verticies of a (possibly
#'   irregular) polygon. Note that you may not feed this function more than one
#'   polygon (i.e. the data.frame should contain only the information of only
#'   one watershed).
#' @param newData  A data.frame with two named columns: x (numeric), y
#'   (numeric).
#'
#' @return A vector with boolean elements taking the value TRUE when the row of
#'   the coordinate point corresponding to each row of the newData data.frame
#'   lies inside the vertices of the polygon.
#' @export
in_hull <- function(vertices, newData) {
  nVertices <- nrow(vertices)

  check_inside <- function(vertx, verty, testx, testy) {
    inside    <- FALSE
    for (i in 1:(nVertices)) {
      if (i == 1) { j = nVertices } else { j <- i - 1 }
      if (
        ((verty[i] > testy) != (verty[j] > testy)) &&
        (testx < (vertx[j] - vertx[i]) * (testy - verty[i]) / (verty[j] - verty[i]) + vertx[i])
      ) {
        inside = !inside;
      }
    }

    inside
  }

  sapply(1:nrow(newData), function(i) {
    check_inside(vertices$x, vertices$y, newData[i, ]$x, newData[i, ]$y)
  })
}

#' Assign point coordinates to watersheds based on boundaries. This is probably
#' one of the most ugly functions in the package, it's strongly recommended that
#' you do not take a look at it unless you really have to.
#'
#' @author Luis Damiano
#' @param dataset A data.frame with three columns: x (numeric), y (numeric),
#'   label (factor).
#' @param newData A data.frame with four columns: watershed (factor),
#'   watersheadVegetation (factor: Row crop or Perennial), watersheadArea
#'   (numeric), watersheadPolygon (numeric) to index the number of "subparts"
#'   inside a watershed.
#'
#' @return A vector with size equal to the number of rows in newData containing
#'   the watershed information each element of newData is assigned to.
#' @export
classify_watersheds <- function(yieldDF, boundaryDF) {
  # To gain some efficiency, we split the boundaries per site so that the yield
  # coordinates points are only checked against polygon boundaries in the same
  # site. For example, we don't check a yield coordinate point from the Orb site
  # against the Interim boundaries.
  boundaryDF$key <- paste(boundaryDF$watershed, boundaryDF$vegetation, boundaryDF$area, sep = "-")
  boundaryLists  <- split(boundaryDF[, c("x", "y")], boundaryDF$key)

  cl <- parallel::makeCluster(nCores)
  parallel::clusterExport(cl, c("boundaryLists", "yieldDF", "in_hull"), envir = environment())

  l <- parallel::parLapply(cl, X = boundaryLists, fun = function(boundary) {
    in_hull(
      vertices = boundary,
      newData  = yieldDF[, c("x", "y")]
    )
  })

  parallel::stopCluster(cl)

  mat <- do.call(cbind, l)
  mat <- cbind(
    mat,
    "OffBounds" = ifelse(rowSums(mat) < 1, TRUE, FALSE)
  )

  if (sum(rowSums(mat) > 1)) {
    ind <- which(rowSums(mat) > 1)

    warning(
      sprintf("Found %d yield coordinate points that could be assigned to more than one boundary. It's crazy, probably wrong. ", length(ind))
    )
  }

  assignment <- apply(mat, 1, function(x) { names(which(x))[1] })

  leftDF <- data.frame(
    id  = seq_along(assignment),
    key = assignment
  )

  rightDF <- rbind(
    unique(boundaryDF[, c("key", "watershed", "polygon", "vegetation", "area")]),
    data.frame(key = "OffBounds", watershed = "OffBounds", polygon = -1, vegetation = "Unknown", area = -1)
  )

  rightDF$polygon <- ave(rightDF$area, list(rightDF$watershed), FUN = seq_along)

  joinDF <- merge(leftDF, rightDF, all.x = TRUE)
  joinDF <- joinDF[order(joinDF$id), c("watershed", "vegetation", "area", "polygon")]
  colnames(joinDF) <- c(
    "watershed", "watersheadVegetation", "watersheadArea", "watersheadPolygon"
    )

  joinDF
}

build_extra <- function(yieldDF, boundaryDF) {
  # Assign data points to watersheds
  yieldDF     <- cbind(
    yieldDF,
    classify_watersheds(yieldDF, boundaryDF)
  )
  colnames(yieldDF)[colnames(yieldDF) == "watersheadVegetation"] <- "vegetation"

  # Add data watershed data
  metaDF      <- as.data.frame(
    STRIPSMeta::watersheds[,
      c("watershed", "size_ha", "slope_pct", "treatment", "prairie_pct",
        "prairie_location", "block")
    ]
  )
  metaDF[, 4] <- factor(metaDF[, 4])
  metaDF[, 5] <- factor(metaDF[, 5])
  metaDF[, 6] <- factor(metaDF[, 6])
  metaDF[, 7] <- factor(metaDF[, 7])
  colnames(metaDF) <- c(
    "watershed", "blockArea", "slope", "treatment", "prairiePercentage",
    "prairiePosition", "block"
  )

  extraDF    <- merge(
    x     = yieldDF,
    y     = metaDF,
    by    = "watershed",
    all.x = TRUE
  )

  # Add extra yield and mass variables
  extraDF$massWetLb     <- extraDF$flow * extraDF$cycle
  extraDF$massDryLb     <- extraDF$massWetLb / (1 + extraDF$moisture / 100)
  extraDF$massStdLb     <- extraDF$massDryLb *
    (1 + pmin(grain_market_moisture(extraDF$crop), extraDF$moisture) / 100)

  extraDF$massWetKg    <- lbs_to_kgs(extraDF$massWetLb)
  extraDF$massDryKg    <- lbs_to_kgs(extraDF$massDryLb)
  extraDF$massStdKg    <- lbs_to_kgs(extraDF$massStdLb)

  # extraDF$yieldDryBuAc comes from `yield`
  extraDF$yieldWetBuAc  <- extraDF$yieldDryBuAc * (1 + extraDF$moisture / 100)
  extraDF$yieldStdBuAc  <- extraDF$yieldDryBuAc *
    (1 + pmin(grain_market_moisture(extraDF$crop), extraDF$moisture) / 100)

  extraDF$yieldDryMgHa <- buac_to_kgha(extraDF$yieldDryBuAc, extraDF$crop)
  extraDF$yieldWetMgHa <- buac_to_kgha(extraDF$yieldWetBuAc, extraDF$crop)
  extraDF$yieldStdMgHa <- buac_to_kgha(extraDF$yieldStdBuAc, extraDF$crop)

  # Add coordinates in UTM
  coordinatesUTM <- coordinateLLtoUTM(extraDF$x, extraDF$y)
  extraDF$xUTM   <- coordinatesUTM[, "x"]
  extraDF$yUTM   <- coordinatesUTM[, "y"]

  # Order data frame
  rowOrder <- order(extraDF$site, extraDF$year, extraDF$record)
  colOrder <- c(
    "site", "watershed", "watersheadPolygon", "watersheadArea", "block",
    "blockArea", "treatment", "prairiePercentage", "prairiePosition", "slope",
    "year", "crop", "swath", "record", "pass", "date", "timelapse", "x", "y",
    "xUTM", "yUTM", "vegetation", "elevation", "speed", "direction", "distance",
    "cycle", "flow", "moisture",
    "massDryLb", "massStdLb", "massWetLb",
    "massDryKg", "massStdKg", "massWetKg",
    "yieldDryBuAc", "yieldStdBuAc", "yieldWetBuAc",
    "yieldDryMgHa", "yieldStdMgHa", "yieldWetMgHa"
  )

  extraDF[rowOrder, colOrder]
}
