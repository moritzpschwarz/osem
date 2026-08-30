#' Apply an OSEM variable transformation
#'
#' @param x Numeric vector or matrix.
#' @param transformation Character. One of `"none"`, `"log"`, or `"asinh"`.
#'
#' @return The transformed values.
#'
#' @keywords internal
transform_osem_values <- function(x, transformation = "none") {
  if (is.na(transformation)) {
    transformation <- "none"
  }

  switch(
    transformation,
    none = x,
    log = log(x),
    asinh = asinh(x),
    stop("Unknown OSEM transformation: ", transformation, ".")
  )
}


#' Invert an OSEM variable transformation
#'
#' @inheritParams transform_osem_values
#'
#' @return Values on the original level scale.
#'
#' @keywords internal
inverse_transform_osem_values <- function(x, transformation = "none") {
  if (is.na(transformation)) {
    transformation <- "none"
  }

  switch(
    transformation,
    none = x,
    log = exp(x),
    asinh = sinh(x),
    stop("Unknown OSEM transformation: ", transformation, ".")
  )
}
