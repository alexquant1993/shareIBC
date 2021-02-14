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


#' Converts column names into human readable versions
#' @noRd
humanFriendlyNames <- function(colnames) {
  paste0(toupper(substring(colnames, 1, 1)),
         substring(gsub("\\.|_", " ", colnames), 2))
}

dropNulls <- function (x){
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}