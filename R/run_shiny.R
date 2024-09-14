#' Creates and runs the OSEM Shiny App
#'
#' Opens a window for the user to interact with the app. The user can upload
#' an \link[=new_osem]{osem} object returned by \code{\link{run_model}} and
#' produce graphical and tabular output.
#' @param model The model object that is passed by the 'present_model()' function.
#'

run_shiny <- function(model = NULL) {

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Shiny App requires package 'DT'.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny App requires package 'shiny'.")
  }

  appDir <- system.file("shiny-output", "osem.shiny", package = "osem")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `osem`.", call. = FALSE)
  }

  # if(!is.null(model)){
  #   shiny::shinyAppDir(appDir, options = list(object = model))
  # } else {
  #   shiny::shinyAppDir(appDir)
  # }


  shiny::shinyOptions(osemel_direct = model)
  source(system.file("shiny-output", "shinyosemel/app.R", package = "osem"))$value

}
