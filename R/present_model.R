present_model <- function(model) {
  require(shiny, quietly = TRUE)
  # render a shiny/RMarkdown with module_collection

  run_shiny(model = model)
}
