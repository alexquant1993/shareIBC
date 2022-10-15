#' When files get uploaded, their new filenames are gibberish.
#' This function renames all uploaded files to their original names
#' @noRd
fixUploadedFilesNames <- function(x) {
  if (is.null(x)) {
    return()
  }
  oldNames = x$datapath
  newNames = file.path(dirname(x$datapath), x$name)
  file.rename(from = oldNames, to = newNames)
  x$datapath <- newNames
  x
}