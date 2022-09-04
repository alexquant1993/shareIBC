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

#' Display debugging messages in R (if local) 
#' and in the console log (if running in shiny)
#' @noRd
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  in_shiny <- !is.null(shiny::getDefaultReactiveDomain())
  txt <- toString(list(...))
  if (is_local) message(txt)
  if (in_shiny) shinyjs::runjs(sprintf("console.debug(\"%s\")", txt))
}


