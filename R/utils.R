#' A random image
#' @description This function returns a random image
#' @usage random_image()
#' @return an image
#' @examples
#' random_image()
#' @noRd
random_pic <- function() {
  l <- list.files(system.file("img", package = "shareIBC"), 
                  full.names = TRUE)
  img <- normalizePath(sample(l, 1))
  tmpimg <- tempfile(fileext = ".jpg")
  file.copy(img, tmpimg)
  tmpimg
}

#' Get inputs from a headless app object from the shinytest2 package
#' @description It allows you to unpack the values given a vector of input names
#' @param app headless shiny app, an object from shinytest2 package
#' @param inputs a vector of input names
#' @return a vector of values given the provided input names
#' @noRd
get_inputs <- function(app, inputs){
  ls <- app$get_values()$input
  unname(unlist(ls[inputs]))
}
