#' Forecast model insample to evaluate performance
#'
#' @param model An aggregate model object of class 'aggmod'.
#' @param seed Integer. Seed for the uncertainty draws to be made. Default is 1234.
#'
#' @return List element.
#' @export
#'
#' @examples forecast_insample(model)
forecast_insample <- function(model, seed = 1234) {

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

  # all_models <- list()
  # for(j in 1:length(time_to_use)){
  #   #if(time_to_use[j] == "2019-04-01"){browser()}else{next}
  #   print(time_to_use[j])
  #   # now let's prepare the model object
  #   try(insample_model <- run_model(
  #     specification = model$args$specification,
  #     dictionary = model$args$dictionary,
  #     filter_list =  model$args$filter_list,
  #     trend = model$args$trend,
  #     #max.lag = model$args$max.lag,
  #     max.ar = model$args$max.ar,
  #     max.dl = model$args$max.dl,
  #     max.block.size = model$args$max.block.size,
  #     ardl_or_ecm = model$args$ardl_or_ecm,
  #     use_logs = model$args$use_logs,
  #     saturation = model$args$saturation,
  #     saturation.tpval = model$args$saturation.tpval,
  #     gets_selection = model$args$gets_selection,
  #     selection.tpval = model$args$selection.tpval,
  #
  #     download = FALSE,
  #     present = FALSE,
  #     quiet = TRUE,
  #
  #     inputdata_directory = model$processed_input_data %>% dplyr::filter(.data$time <= as.Date(time_to_use[j]))
  #   ), silent = TRUE)
  #
  #   if(exists("insample_model")){
  #     all_models[[j]] <- insample_model
  #   }
  # }

  #save(all_models, file = "all_models_2.RData")
  load("all_models_2.RData")

  model$processed_input_data %>%
    dplyr::distinct(across("na_item")) %>%
    dplyr::pull("na_item") -> all_data_vars

  exog_vars <- all_data_vars[!all_data_vars %in% model$module_collection$dependent]

  model$processed_input_data %>%
    dplyr::filter(.data$na_item %in% exog_vars) -> exog_data


  forecasted_unknownexogvalues <- list()
  forecasted_knownexogvalues <- list()

  for(i in 1:length(all_models)){
    #i = 20
    if(is.null(all_models[[i]])){next}

    start <- time_to_use[i]
    end <- time_to_use[length(time_to_use)]
    nsteps <- length(seq.Date(from = as.Date(start), to = as.Date(end), by = "quarter"))

    print(paste0("Forecast ", i, " from ", start, " to ", end))

    forecasted_unknownexogvalues[[i]] <- forecast_model(model = all_models[[i]],
                                                        n.ahead = nsteps, seed = seed,
                                                        plot.forecast = FALSE,
                                                        exog_fill_method = "AR")

    # forecasted_knownexogvalues[[i]] <- forecast_model(
    #   all_models[[i]], n.ahead = nsteps, seed = seed, plot.forecast = FALSE,
    #   exog_predictions = exog_data %>% dplyr::filter(.data$time >= as.Date(start),
    #                                                  .data$time <= as.Date(end)))
  }

  save(forecasted_unknownexogvalues, file = "forecasted_models.RData")

  browser()

  overall_to_plot_central <- dplyr::tibble()
  overall_to_plot_alls <- dplyr::tibble()


  for(i in 1:length(forecasted_unknownexogvalues)){
    # i = 3
    if(is.null(forecasted_unknownexogvalues[[i]])){next}
    print(i)

    #plot(forecasted_unknownexogvalues[[i]])

    forecasted_unknownexogvalues[[i]]$forecast %>%
      dplyr::select("dep_var","central.estimate") %>%
      dplyr::mutate(start = time_to_use[i]) %>%
      tidyr::unnest(.data$central.estimate) %>%
      tidyr::pivot_longer(-c("dep_var", "start", "time")) %>%
      tidyr::drop_na("value") -> centrals

    forecasted_unknownexogvalues[[i]]$forecast %>%
      dplyr::select("dep_var","all.estimates") %>%
      dplyr::mutate(start = time_to_use[i]) %>%
      tidyr::unnest(.data$all.estimates) %>%
      tidyr::pivot_longer(-c("dep_var", "start", "time")) %>%
      tidyr::drop_na("value") -> alls

    dplyr::bind_rows(overall_to_plot_central, centrals) -> overall_to_plot_central

  }

  overall_to_plot_central %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = ifelse(grepl("^ln.", .data$name), exp(.data$value), .data$value)) %>%
    dplyr::ungroup() -> overall_to_plot_central


  forecasted_unknownexogvalues[[length(forecasted_unknownexogvalues)]]$orig_model$full_data %>%
    dplyr::rename(dep_var = .data$na_item) %>%
    dplyr::filter(.data$dep_var %in% overall_to_plot_central$dep_var,
                  #as.Date(min(time_to_use)) < .data$time) -> full_data
                  .data$time > as.Date("2017-04-01")) -> full_data


  overall_to_plot_central %>%
    dplyr::filter(.data$start > as.Date("2017-04-01")) %>%
    ggplot(aes(y = value, x = time, color = as.factor(start))) +
    geom_line() +
    facet_wrap(~dep_var, scales = "free") +
    theme_minimal() +
    theme(legend.position = "none") +

    geom_line(data = full_data,
              aes(x = time, y= values), color = "black") -> plt


  plotly::ggplotly(plt)

}
