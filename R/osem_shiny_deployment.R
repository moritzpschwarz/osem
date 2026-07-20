#' Create the OSEM Shiny workspace
#'
#' Constructs the OSEM Shiny application without launching it. This is useful
#' for deployment to shinyapps.io, Posit Connect, or another Shiny server.
#'
#' @param model Optional fitted \link[=new_osem]{osem} object returned by
#'   \code{\link{run_model}}.
#' @param max_upload_mb Positive numeric value giving the maximum upload size
#'   in megabytes.
#'
#' @return A \code{shiny.appobj} object.
#'
#' @export
osem_app <- function(model = NULL,
                     max_upload_mb = 500) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The OSEM app requires the 'shiny' package.", call. = FALSE)
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("The OSEM app requires the 'DT' package.", call. = FALSE)
  }
  if (!is.null(model) && !inherits(model, "osem")) {
    stop("'model' must be NULL or an object of class 'osem'.", call. = FALSE)
  }

  max_upload_mb <- suppressWarnings(as.numeric(max_upload_mb))

  if (length(max_upload_mb) != 1L ||
      !is.finite(max_upload_mb) ||
      max_upload_mb <= 0) {
    stop(
      "'max_upload_mb' must be one positive numeric value.",
      call. = FALSE
    )
  }

  osem_shiny_create_app(
    initial_model = model,
    max_upload_mb = max_upload_mb
  )
}
