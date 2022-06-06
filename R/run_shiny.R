#' Creates and runs the Shiny App "shinyaggmodel"
#'
#' Opens a window for the user to interact with the app. The user can upload
#' an \link[=new_aggmod]{aggmod} object returned by \code{\link{run_model}} and
#' produce graphical and tabular output.
#'
#' @export

run_shiny <- function() {

  appDir <- system.file("shiny-output", "shinyaggmodel", package = "aggregate.model")

  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::shinyAppDir(appDir)

}
