#' Present the results of the Aggregate Model in a Shiny Dashboard
#'
#' @param model object. The output of the run_model command.
#'
#' @return Opens a shiny app.
#' @export
#'
present_model <- function(model) {
  requireNamespace("shiny", quietly = TRUE)
  # render a shiny/RMarkdown with module_collection

  run_shiny(model = model)
}
