#' Forecast model insample to evaluate forecasting performance
#'
#' @inheritParams forecast_model
#'
#' @return List element.
#' @export
#'
#' @examples forecast_insample(model)
#'
forecast_insample <- function(model, seed = 1234, uncertainty_sample = 100, exog_fill_method = "AR", plot = TRUE) {

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
    # now let's prepare the model object

    try(insample_model <- run_model(

      specification = model$args$specification,
      dictionary = model$args$dictionary,
      filter_list =  model$args$filter_list,
      trend = model$args$trend,

      max.ar = model$args$max.ar,
      max.dl = model$args$max.dl,
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

  #save(all_models, file = "all_models_3.RData")
  #load("all_models_2.RData")

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
    if(start == end){next}
    nsteps <- length(seq.Date(from = as.Date(start), to = as.Date(end), by = "quarter"))


    print(paste0("Forecast ", i, " from ", start, " to ", end))

    forecasted_unknownexogvalues[[i]] <- forecast_model(model = all_models[[i]],
                                                        n.ahead = nsteps,
                                                        uncertainty_sample = uncertainty_sample,
                                                        plot.forecast = FALSE,
                                                        exog_fill_method = exog_fill_method,
                                                        seed = seed,
                                                        quiet = TRUE)

    # forecasted_knownexogvalues[[i]] <- forecast_model(
    #   all_models[[i]], n.ahead = nsteps, uncertainty_sample = uncertainty_sample, plot.forecast = FALSE,
    #   exog_predictions = exog_data %>% dplyr::filter(.data$time >= as.Date(start),
    #                                                  .data$time <= as.Date(end)))
  }

  # save(forecasted_unknownexogvalues, file = "forecasted_models.RData")
  # load("forecasted_models.RData")

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
      tidyr::unnest("central.estimate") %>%
      tidyr::pivot_longer(-c("dep_var", "start", "time")) %>%
      tidyr::drop_na("value") -> centrals

    forecasted_unknownexogvalues[[i]]$forecast %>%
      dplyr::select("dep_var","all.estimates") %>%
      dplyr::mutate(start = time_to_use[i]) %>%
      dplyr::mutate(quantiles = purrr::map(all.estimates, function(x){
        tidyr::pivot_longer(x, -"time") %>%
          dplyr::summarise(max = max(value),
                           min = min(value),
                           p975 = quantile(value, probs = 0.975),
                           p025 = quantile(value, probs = 0.025),
                           p75 = quantile(value, probs = 0.75),
                           p25 = quantile(value, probs = 0.25), .by = time) %>%
          tidyr::pivot_longer(-"time", names_to = "quantile")
      })) %>%
      dplyr::select(-"all.estimates") %>%
      dplyr::full_join(centrals %>% dplyr::distinct(dep_var, name), by = "dep_var") %>%
      tidyr::unnest(quantiles) -> alls

    dplyr::bind_rows(overall_to_plot_central, centrals) -> overall_to_plot_central
    dplyr::bind_rows(overall_to_plot_alls, alls) -> overall_to_plot_alls

  }

  overall_to_plot_central %>%
    dplyr::mutate(value = ifelse(grepl("^ln.", .data$name), exp(.data$value), .data$value)) -> overall_to_plot_central_exp

  overall_to_plot_alls %>%
    dplyr::mutate(value = ifelse(grepl("^ln.", .data$name), exp(.data$value), .data$value)) %>%
    tidyr::pivot_wider(id_cols = c("dep_var","name","start","time"), names_from = "quantile", values_from = "value") -> overall_to_plot_alls_exp

  forecasted_unknownexogvalues[[length(forecasted_unknownexogvalues)]]$orig_model$full_data %>%
    dplyr::rename(dep_var = "na_item") %>%
    dplyr::filter(.data$dep_var %in% overall_to_plot_central_exp$dep_var,
                  .data$time > min(overall_to_plot_central$start)) -> full_data


  overall_to_plot_central_exp %>%
    dplyr::filter(.data$start > min(overall_to_plot_central$start)) %>%
    ggplot2::ggplot() +

    ggplot2::facet_wrap(~dep_var, scales = "free") +
    ggplot2::geom_ribbon(data = overall_to_plot_alls_exp, ggplot2::aes(ymin = min, x = time, ymax = max, fill = as.factor(start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = overall_to_plot_alls_exp, ggplot2::aes(ymin = p025, x = time, ymax = p975, fill = as.factor(start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = overall_to_plot_alls_exp, ggplot2::aes(ymin = p25, x = time, ymax = p75, fill = as.factor(start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +

    ggplot2::geom_line(ggplot2::aes(y = value, x = time, color = as.factor(start)), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~dep_var, scales = "free") +
    #ggplot2::scale_color_brewer(palette = "PRGn") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_cartesian(expand = TRUE) +

    ggplot2::labs(x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) +

    ggplot2::geom_line(data = full_data,
                       ggplot2::aes(x = time, y= values), color = "black", linewidth = 2) -> plt


  plotly::ggplotly(plt)


  if(plot){
    plt
  }


  out <- list()
  out$plot <- plt
  out$central <- overall_to_plot_central_exp
  out$uncertainty <- overall_to_plot_alls_exp_q
  return(out)
}
