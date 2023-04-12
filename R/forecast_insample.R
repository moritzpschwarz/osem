
forecast_insample <- function(model) {

  # we first must identify the minimum sample across modules
  time_samples <- dplyr::tibble()
  for(i in 1:nrow(model$module_collection)){
    # i = 1
    model$module_collection$dataset[i][[1]] %>%
      dplyr::select("time") %>%
      dplyr::mutate(dep_var = model$module_collection$dependent[i]) %>%
      dplyr::bind_rows(time_samples,.) -> time_samples
  }

  time_samples %>%
    dplyr::group_by(.data$dep_var) %>%
    dplyr::summarise(min = min(.data$time),
                     max = max(.data$time)) %>%
    dplyr::distinct(across(c("max","min"))) -> time_minmax


  # With those times, we can now find the halfway point
  all_times <- seq(time_minmax$min,time_minmax$max, by = "quarter")
  time_to_use <- all_times[ceiling(length(all_times)/2):length(all_times)]

  all_models <- list()
  for(j in 1:length(time_to_use)){
    print(time_to_use[j])
    #browser()
    # now let's prepare the model object
    try(insample_model <- run_model(
      specification = model$args$specification,
      dictionary = model$args$dictionary,
      filter_list =  model$args$filter_list,
      trend = model$args$trend,
      max.lag = model$args$max.lag,
      max.block.size = model$args$max.block.size,
      ardl_or_ecm = model$args$ardl_or_ecm,
      use_logs = model$args$use_logs,
      saturation = model$args$saturation,
      saturation.tpval = model$args$saturation.tpval,
      gets_selection = model$args$gets_selection,
      selection.tpval = model$args$selection.tpval,

      download = FALSE,
      present = FALSE,
      quiet = TRUE,

      inputdata_directory = model$processed_input_data %>% dplyr::filter(.data$time <= as.Date(time_to_use[j]))
    ), silent = TRUE)

    if(exists("insample_model")){
      all_models[[j]] <- insample_model
    }


  }


  browser()




}
