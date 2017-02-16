library(maptools)

Basswood <- readShapeSpatial("Basswood-2011-SBYield",
                      proj4string = CRS("+GCS_WGS_1984"))
Interim <- readShapeSpatial("Interim-2011-SBYield",
                             proj4string = CRS("+GCS_WGS_1984"))
Orbweaver <- readShapeSpatial("Orbweaver-2011-SBYield",
                              proj4string = CRS("+GCS_WGS_1984"))

yield2011 <- rbind(Basswood,Interim,Orbweaver)
