#' A random image
#' @description This function returns a random image
#' @usage random_image()
#' @return an image
#' @examples
#' random_image()
#' @noRd
RandomPic <- function() {
  l <- list.files(system.file("img", package = "shinipsum"), 
                  full.names = TRUE)
  img <- normalizePath(sample(l, 1))
  tmpimg <- tempfile(fileext = ".jpg")
  file.copy(img, tmpimg)
  tmpimg
}

#' Populate posts database with random data
#' @usage RandomPosts()
#' @param n number of random posts
#' @return A data.frame
#' @noRd
RandomPosts <- function(n = 20) {
  # Create dataframe with random posts
  ID_POST <- paste0("POST_", sprintf("%09d", 1:n))
  NAME_POSTER <- charlatan::ch_name(n)
  EMAIL_POSTER <- rep(get_golem_config("testing_email"), n)
  TYPE_POST <- 
    sample(c("jobs", "services", "upcycle", "mix"), size = n, replace = TRUE)
  SUBJECT <-
    sample(
      c(charlatan::ch_taxonomic_epithet(n),
        charlatan::ch_color_name(n),
        charlatan::ch_company(n),
        charlatan::ch_name(n)),
      size = n, replace = TRUE
    )
  DESCRIPTION <- rep(shinipsum::random_text(nwords = 20), n)
  CONTACT_EMAIL <- rep("email@example.com", n)
  CONTACT_PHONE <- charlatan::ch_phone_number(n, locale = "es_ES")
  FILES_URL <- rep(NA, n)
  GDPR_ACCEPTANCE <- rep(TRUE, n)
  DATE_POST <- rep(Sys.time(), n)
  STATUS <- rep("Approved", n)
  ID_APPROVER <- rep("APV_01", n)
  COMMENTS_APPROVER <- rep(shinipsum::random_text(nwords = 10), n)
  DATE_REVISION <- rep(Sys.time(), n)
  CONDITION <- rep("Open", n)
  
  dt <-
    data.frame(
      ID_POST, NAME_POSTER, EMAIL_POSTER, TYPE_POST, SUBJECT, DESCRIPTION,
      CONTACT_EMAIL, CONTACT_PHONE, FILES_URL, GDPR_ACCEPTANCE, DATE_POST,
      STATUS, ID_APPROVER, COMMENTS_APPROVER, DATE_REVISION, CONDITION
    )
  
  # Connect to APIs
  ApiConnections("default")
  # Read posts database
  wb <- 
    googledrive::drive_get(
      get_golem_config("posts_path", config = "default")
    )
  # Populate the posts database for testing purposes
  googlesheets4::range_write(
    wb,
    data = dt,
    sheet = "DATABASE",
    range = "A2",
    col_names = FALSE
  )
  NULL
}

#####DISCLAIMER: Borrowing mapvalues from plyr package (Hadley WickHam) 
# in order to not add another dependency over just one function.

#' Replace specified values with new values, in a factor or character vector.
#'
#' If \code{x} is a factor, the named levels of the factor will be
#' replaced with the new values.
#'
#' This function works only on character vectors and factors, but the
#' related \code{mapvalues} function works on vectors of any type and factors,
#' and instead of a named vector specifying the original and replacement values,
#' it takes two separate vectors
#'
#' @param x factor or character vector to modify
#' @param replace named character vector, with new values as values, and
#'   old values as names.
#' @param warn_missing print a message if any of the old values are
#'   not actually present in \code{x}
#'
#' @seealso \code{\link{mapvalues}} to replace values with vectors of any type
#' @noRd
#' @examples
#' x <- c("a", "b", "c")
#' revalue(x, c(a = "A", c = "C"))
#' revalue(x, c("a" = "A", "c" = "C"))
#'
#' y <- factor(c("a", "b", "c", "a"))
#' revalue(y, c(a = "A", c = "C"))
revalue <- function(x, replace = NULL, warn_missing = TRUE) {
  if (!is.null(x) && !is.factor(x) && !is.character(x)) {
    stop("x is not a factor or a character vector.")
  }
  
  mapvalues(x, from = names(replace), to = replace, warn_missing = warn_missing)
}


#' Replace specified values with new values, in a vector or factor.
#'
#' Item in \code{x} that match items \code{from} will be replaced by
#' items in \code{to}, matched by position. For example, items in \code{x} that
#' match the first element in \code{from} will be replaced by the first
#' element of \code{to}.
#'
#' If \code{x} is a factor, the matching levels of the factor will be
#' replaced with the new values.
#'
#' The related \code{revalue} function works only on character vectors
#' and factors, but this function works on vectors of any type and factors.
#'
#' @param x the factor or vector to modify
#' @param from a vector of the items to replace
#' @param to a vector of replacement values
#' @param warn_missing print a message if any of the old values are
#'   not actually present in \code{x}
#'
#' @seealso \code{\link{revalue}} to do the same thing but with a single
#'   named vector instead of two separate vectors.
#' @noRd
#' @examples
#' x <- c("a", "b", "c")
#' mapvalues(x, c("a", "c"), c("A", "C"))
#'
#' # Works on factors
#' y <- factor(c("a", "b", "c", "a"))
#' mapvalues(y, c("a", "c"), c("A", "C"))
#'
#' # Works on numeric vectors
#' z <- c(1, 4, 5, 9)
#' mapvalues(z, from = c(1, 5, 9), to = c(10, 50, 90))
mapvalues <- function(x, from, to, warn_missing = TRUE) {
  if (length(from) != length(to)) {
    stop("`from` and `to` vectors are not the same length.")
  }
  if (!is.atomic(x) && !is.null(x)) {
    stop("`x` must be an atomic vector or NULL.")
  }
  
  if (is.factor(x)) {
    # If x is a factor, call self but operate on the levels
    levels(x) <- mapvalues(levels(x), from, to, warn_missing)
    return(x)
  }
  
  mapidx <- match(x, from)
  mapidxNA  <- is.na(mapidx)
  
  # index of items in `from` that were found in `x`
  from_found <- sort(unique(mapidx))
  if (warn_missing && length(from_found) != length(from)) {
    message("The following `from` values were not present in `x`: ",
            paste(from[!(1:length(from) %in% from_found) ], collapse = ", "))
  }
  
  x[!mapidxNA] <- to[mapidx[!mapidxNA]]
  x
}
