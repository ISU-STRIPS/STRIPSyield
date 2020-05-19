# Compute UTM zone from LatLong coordinates
proj4stringUTMFromLL <- function(x) {
  # https://stackoverflow.com/a/9188972
  zone <- (floor((x + 180)/6) %% 60) + 1

  sprintf(
    "+proj=utm +zone=%d +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0",
    zone
  )
}

# Transform from and to original CRS
coordinatesTransf <- function(x, y, proj4stringIn, proj4stringOut) {
  isNA <- is.na(x[1])

  p    <- sp::SpatialPoints(
    cbind(na.omit(x), na.omit(y)),
    proj4string = sp::CRS(proj4stringIn)
  )
  p    <- sp::spTransform(p, sp::CRS(proj4stringOut))

  if (isNA)
    return(rbind(c(NA, NA), sp::coordinates(p)))

  sp::coordinates(p)
}

# Transform from LatLong to UTM
coordinateLLtoUTM <- function(x, y, p4s = "+proj=longlat +datum=WGS84") {
  p4sOut <- proj4stringUTMFromLL(x)

  coordsUTM <- cbind(x, y)
  for (str in unique(p4sOut))
    coordsUTM[p4sOut == str, ] <- coordinatesTransf(x, y, p4s, str)

  coordsUTM
}
