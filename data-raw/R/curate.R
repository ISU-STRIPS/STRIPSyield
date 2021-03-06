#' Remove non-alphabetic characters and modify to lowercase.
#'
#' @param string A character vector, or a vector of character vectors.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example string_simplify("F0:BASSWOOD")
string_simplify <- function(string) {
  gsub("[^A-Za-z0-9]", "", tolower(string))
}

#' Replace the whole string based on pattern matching. If one pattern matches, the whole string is replaced by the replacement. If one of more pattern match, a warning is raised and the whole string is replaced by the first replacement. If no pattern matches, a warning is raised and the original string is returned.
#'
#' @param x A character vector, or a vector of character vectors.
#' @param patterns A vector of character vectors with patterns.
#' @param replacements A vector of character vectors with the desired output if the `x` string matches any of the `patterns` string.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example string_replace("F0:BASSWOOD", c("bass", "int"), c("Basswood", "Interim"))
string_replace <- function(x, patterns, replacements) {
  string_replace_one <- function(x) {
    ord <- do.call(
      c,
      sapply(
        X   = patterns,
        FUN = grep,
        x   = string_simplify(x)
      )
    )

    pos <- which(patterns %in% names(ord))

    if (length(patterns) < length(names(ord)))
      stop(sprintf("%s %s %s ", x, paste(patterns), paste(names(ord))))

    if (length(ord) < 1) {
      warning(sprintf("The value %s (shapefile) is unknown.", x), call. = FALSE)
      return(x)
    }

    if (length(ord) > 1 & length(unique(replacements[pos])) > 1) {
      warning(
        sprintf(
          "The value %s (shapefile) cannot be uniquely matched and was replaced by %s.",
          x,
          replacements[pos[1]]
        ),
        call. = FALSE
      )
      return(replacements[pos[1]])
    }

    replacements[pos[1]]
  }

  ret <- NULL
  if (length(x) == 1) {
    ret <- string_replace_one(x)
  } else {
    u <- unique(x)
    if (length(u) == 1) {
      ret <- rep(string_replace_one(u), length(x))
    } else {
      ret <- sapply(x, string_replace_one)
    }
  }

  ret
}

#' Return a curated name of sites based on pattern matching.
#'
#' @param x A character vector, or a vector of character vectors.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example curate_sitename(c("F0:BASSWOOD", "ISU ORB NORTH", "F28:ISU", "WontMatch"))
#' @seealso \code{\link{string_replace}}
curate_sitename <- function(x) {
  patterns     <- c("bass",      "int",     "orb", "f27", "f28", "f29")
  replacements <- c("Basswood", "Interim", "Orbweaver", "Orbweaver", "Basswood", "Interim")
  string_replace(x, patterns, replacements)
}

#' Return a curated name of sites based on coordinate matching.
#'
#' @param x A numeric vector with the coordinate longitudes.
#' @param x A numeric vector with the coordinate latitudes.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example curate_sitename_by_coordinates(-93.27318, 41.55136)
#' @seealso \code{\link{curate_sitename}}
curate_sitename_by_coordinates <- function(x, y) {
  boundingBox <- list(
    "Basswood" = structure(
      c(
        -93.2772076728709, 41.5371017721844,
        -93.2709883469505, 41.5405923162667
      ),
      .Dim = c(2L, 2L),
      .Dimnames = list(c("x", "y"), c("min", "max"))
    ),
    "Interim" = structure(
      c(
        -93.2507355139719, 41.5551257323418,
        -93.2458203134386, 41.5586092404122
      ),
      .Dim = c(2L, 2L),
      .Dimnames = list(c("x", "y"), c("min", "max"))
    ),
    "Orbweaver north" = structure(
      c(
        -93.2752276820073, 41.5479136837798,
        -93.2724683716671, 41.5513883161976
      ),
      .Dim = c(2L, 2L),
      .Dimnames = list(c("x", "y"), c("min", "max"))
    ),
    "Orbweaver south" = structure(
      c(
        -93.2768816776079, 41.5429186837545,
        -93.2751003222553, 41.5447880162087
      ),
      .Dim = c(2L, 2L),
      .Dimnames = list(c("x", "y"), c("min", "max"))
    )
  )

  is_within <- function(x, y, bbox) {
    # https://stackoverflow.com/a/58438445
    isLongInRange <- x >= bbox[1, 1] & x <= bbox[1, 2]
    isLatInRange  <- y >= bbox[2, 1] & y <= bbox[2, 2]
    isLongInRange & isLatInRange
  }

  # checks <- do.call(cbind, lapply(boundingBox, is_within, x = x, y = y))
  checks <- data.frame(
    lapply(boundingBox, is_within, x = x, y = y),
    check.names = FALSE
  )

  if (sum(rowSums(checks) != 1))
    warning(
      sprintf(
        "Found %d coordinates that didn't match any site",
        sum(rowSums(checks) != 1)
      )
    )

  checks$OffBounds <- FALSE
  if (sum(rowSums(checks) == 0))
    checks$OffBounds[which(rowSums(checks) == 0)] <- TRUE

  colnames(checks)[apply(checks, 1, which)]
}

#' Return a curated name of watersheds based on pattern matching.
#'
#' @param x A character vector, or a vector of character vectors.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example curate_watershedname(c("interim_3", "WontMatch"))
#' @seealso \code{\link{string_replace}}
curate_watershedname <- function(x) {
  sitenames    <- c("basswood", "interim", "orbweaver")
  patterns     <- do.call(c, lapply(sitenames, function(x) { paste(x, 1:6, sep = "") } ))
  replacements <- sapply(patterns, function(x) {
    paste0(toupper(substr(x, 0, 1)), substr(x, 2, stop = nchar(x)))
  })
  string_replace(x, patterns, replacements)
}

#' Return a curated name of vegetation based on pattern matching.
#'
#' @param x A character vector, or a vector of character vectors.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example curate_vegetation(c("P", "C", "row", "WontMatch"))
#' @seealso \code{\link{string_replace}}
curate_vegetation <- function(x) {
  patterns     <- c("row",      "p",         "c",        "sediment")
  replacements <- c("Row crop", "Perennial", "Row crop", "Row crop")
  string_replace(x, patterns, replacements)
}

#' Return a curated name of crop types based on pattern matching.
#'
#' @param x A character vector, or a vector of character vectors.
#' @return A character vector, or a vector of character vectors.
#' @author Luis Damiano
#' @example curate_cropname(c("SOYBEANS", "SOY", "CORN", "WontMatch"))
#' @seealso \code{\link{string_replace}}
curate_cropname <- function(x) {
  patterns     <- c("corn", "soy")
  replacements <- c("Corn", "Soybeans")
  string_replace(x, patterns, replacements)
}
