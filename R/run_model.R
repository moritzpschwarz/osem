


# config_table_small <- tibble(
#   dependent = c("JL",
#                 "TOTS",
#                 "B"),
#   independent = c("TOTS - CP - CO - J - A",
#                   "YF + B",
#                   "CP + J"))
#
#
# specification <- config_table_small

run_model <- function(specification, inputdata_directory = paste0(getwd(),"/data/raw")){

  module_order <- check_config_table(specification)

  module_order_eurostatvars <- translate_variables(module_order)

  loaded_data <- load_or_download_variables(module_order_eurostatvars, inputdata_directory)

  module_order_eurostatvars %>%
    mutate(module_estimate = list(NA_complex_)) -> module_collection

  for(i in 1:module_order$order){
    cat(paste0("Estimating ",module_collection$dependent[i], " = ", module_collection$independent[i]))
    module_estimate <- run_module(specification = module_collection_eurostatvars[,module_collection_eurostatvars$order == i],
                                  data = loaded_data)

    module_collection[module_collection$order == i, "module_estimate"] <- tibble(module_estimate = list(module_estimate))
  }

  if(present){
    present_model(module_collection)
  }



  out <- list()
  out$args <- list(specification = specification, inputdata_directory = inputdata_directory)
  out$module_order_eurostatvars <- module_order_eurostatvars
  out$module_collection <- module_collection

  return(out)

}
