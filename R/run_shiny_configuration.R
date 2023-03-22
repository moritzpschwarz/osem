#' Creates and runs the Shiny Configuration App "shinyconfigaggmodel"
#'
#' Opens a window for the user to interact with the app. The user can upload
#' an \link[=new_aggmod]{aggmod} object returned by \code{\link{run_model}} and
#' produce graphical and tabular output.
#' @param model The model object that is passed by the 'present_model()' function.
#'
#' @export

run_shiny_configuration <- function(model = NULL) {

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Shiny App requires package 'DT'.")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Shiny App requires package 'shiny'.")
  }

  appDir <- system.file("shiny-output", "shinyconfigaggmodel", package = "aggregate.model")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `aggregate.model`.", call. = FALSE)
  }

  # if(!is.null(model)){
  #   shiny::shinyAppDir(appDir, options = list(object = model))
  # } else {
  #   shiny::shinyAppDir(appDir)
  # }


  shiny::shinyOptions(aggmodel_direct = model)
  source(system.file("shiny-output", "shinyconfigaggmodel/app.R", package = "aggregate.model"))$value

}
