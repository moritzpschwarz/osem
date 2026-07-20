#' Launch the OSEM Shiny workspace
#'
#' Opens the complete modular OSEM application for project management, data
#' ingestion, dictionary and model specification, estimation, results
#' exploration, scenario forecasting, and reproducible-code export. When a
#' fitted model is supplied, the app opens it together with its stored
#' specification, dictionary, settings, and processed input-data snapshot.
#'
#' @param model Optional fitted \link[=new_osem]{osem} object returned by
#'   \code{\link{run_model}}.
#' @param max_upload_mb Positive numeric value giving the maximum upload size
#'   in megabytes for project, model, data, dictionary, and assumption files.
#' @param launch.browser Passed to \code{shiny::runApp()}. Use \code{FALSE}
#'   when launching the app on a server without opening a local browser.
#'
#' @return Launches a Shiny application and returns invisibly when it exits.
#'
#' @export
#'
run_shiny <- function(model = NULL,
                      max_upload_mb = 500,
                      launch.browser = getOption("shiny.launch.browser", interactive())) {
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
  if (length(max_upload_mb) != 1L || !is.finite(max_upload_mb) || max_upload_mb <= 0) {
    stop("'max_upload_mb' must be one positive numeric value.", call. = FALSE)
  }

  shiny::runApp(
    osem_shiny_create_app(
      initial_model = model,
      max_upload_mb = max_upload_mb
    ),
    launch.browser = launch.browser
  )
}
