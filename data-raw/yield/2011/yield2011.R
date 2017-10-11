library("dplyr")

my_read_shape = function(f, into) {
  rgdal::readOGR(f, proj4string = CRS("+GCS_WGS_1984")) %>%
    mutate(file=f) %>%
    tidyr::separate(file, into)
}

read_dir = function(path, pattern, into) {
  files_w_ext = list.files(path = path,
                     pattern = pattern,
                     recursive = TRUE,
                     full.names = TRUE)
  files = gsub(".shp", "", files_w_ext)
  plyr::ldply(files, my_read_shape, into = into)
}


yield <- read_dir(path = ".",
                  pattern = "*.shp",
                  into = c("year","watershed","year2","yield","ext"))
