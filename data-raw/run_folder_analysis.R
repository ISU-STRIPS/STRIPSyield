# Settings ----------------------------------------------------------------
root <- "/home/luis/Downloads/STRIPS1 Research - NSNWR"
excl <- c(".adf$") # Patterns to exclude
incl <- c(".dbf$")
outf <- "outDbfOnly.txt"

# Auxiliary functions -----------------------------------------------------
printy     <- function(x) { paste(x, collapse = ", ") }

split_path <- function(x) {
  # Credits to https://stackoverflow.com/a/29232017
  if (dirname(x) == x) {
    x
  } else {
    c(basename(x), split_path(dirname(x)))
  }
}

print_dup  <- function(DB, ind, by) {
  dupDB <- DB[ind, ]
  for (m in unique(dupDB[[by]])) {
    dupFullPath <- DB$fullPath[DB[[by]] == m]
    cat(
      sprintf(
        "  %s\n%s\n\n",
        m,
        paste(
          sprintf(
            "    [%2d] %s",
            seq_along(dupFullPath),
            dupFullPath
          ),
          collapse = "\n"
        )
      )
    )
  }
}

# We start! ---------------------------------------------------------------
while (!dir.exists(root)) {
  root <- readline(
    prompt = "Insert the path to the \"STRIPS1 Research - NSNWR\" folder: "
  )
}

if (outf != "")
  sink(file = outf, split = TRUE)

cat(
  sprintf(
    "Will scan %s for duplicated files matching %s and excluding %s.\nResults will be %s.\n\n",
    root,
    printy(incl),
    if (any(excl != "")) printy(excl) else "nothing",
    if (outf == "") "printed out" else sprintf("written to %s", outf)
  )
)

cat(Sys.info())

cat(format(Sys.time(), "\n%Y-%m-%d %X\n\n"))

# Identify files
cat(sprintf("Looking for filenames matching the patterns.\n"))
filenames <- unique(
  do.call(c, lapply(incl, function(i) {
    dir(
      path         = root,
      pattern      = i,
      full.names   = TRUE,
      recursive    = TRUE,
      include.dirs = TRUE
    )
  }))
)

if (any(excl != "")) {
  indExclude <- Reduce(`|`, lapply(excl, function(e) { grepl(e, filenames) }))
  filenames  <- filenames[!indExclude]
  cat(
    sprintf(
      "Excluded %d filenames matching %s.\n",
      sum(indExclude),
      printy(excl)
    )
  )
}

cat(sprintf("  A total of %d files will be analyzed.\n\n", length(filenames)))

# Create file database
cat("Building a database with information about the files.\n")
fileDB  <- data.frame(
  fullPath  = filenames,
  dirname   = dirname(filenames),
  filename  = basename(filenames),
  filenoext = tools::file_path_sans_ext(filenames),
  md5       = tools::md5sum(filenames),
  stringsAsFactors = FALSE
)

fileDB  <- fileDB[!is.na(fileDB$md5), ]

# List duplicated files
fileInd <- duplicated(fileDB$md5)

cat(
  sprintf(
    "  %d duplicated files, %d unique.\n\n",
    sum(fileInd),
    length(unique(fileDB$md5))
  )
)

# Create folder database
cat("Building a database with information about the folders.\n")

folderDB <- do.call(
  rbind,
  lapply(unique(fileDB$dirname[fileInd]), function(f) {
    filesMd5 <- tools::md5sum(dir(f, full.names = TRUE, include.dirs	= FALSE))
    data.frame(
      fullPath = f,
      md5      = digest::digest(as.vector(filesMd5)),
      stringsAsFactors = FALSE
    )
  })
)

# List of duplicated folders
folderInd <- duplicated(folderDB$md5)

cat(
  sprintf(
    "  %d duplicated folders, %d unique.\n\n",
    sum(folderInd),
    length(unique(folderDB$md5))
  )
)

# Full dump of duplicated folders
cat("List of duplicated folders.\n\n")

print_dup(folderDB, folderInd, "md5")

# Full dump of duplicated files

cat("List of duplicated files.\n\n")

print_dup(fileDB, fileInd, "md5")

cat(format(Sys.time(), "%Y-%m-%d %X\n"))

if (outf != "") {
  sink()
  browseURL(outf)
}
