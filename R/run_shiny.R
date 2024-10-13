#' Creates and runs the OSEM Shiny App
#'
#' Opens a window for the user to interact with the app. The user can upload
#' an \link[=new_osem]{osem} object returned by \code{\link{run_model}} and
#' produce graphical and tabular output.
#' @param model The model object that is passed by the 'present_model()' function.
#'

run_shiny <- function(model = NULL) {

  # Check for required packages
  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Shiny App requires package 'DT'.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny App requires package 'shiny'.")
  }

  # Find the app directory within the package
  appDir <- system.file("shiny-output", "shinyconfigmodel", package = "osem")

  # If the directory is not found, raise an error
  if (appDir == "") {
    stop("Could not find the Shiny app directory. Try re-installing the 'osem' package.", call. = FALSE)
  }

  # Set Shiny options to pass the model object to the app if provided
  if (!is.null(model)) {
    shiny::shinyOptions(osem_direct = model)
  }

  # Launch the Shiny app
  shiny::runApp(appDir)
}
