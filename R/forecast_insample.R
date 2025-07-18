#' Forecast model insample to evaluate forecasting performance
#' @param sample_share Share of the sample that should be used for the insample forecasting. Must be a numeric and must be either 1 or smaller but larger than 0.
#' @param exog_fill_method Character vector that contains the methods to fill the exogenous variables. Default is "AR" but can also contain multiple methods, e.g. c("AR","auto").
#' @inheritParams forecast_model
#'
#' @return An object (list) with the class \code{osem.forecast.insample}. This object contains the central estimates and the uncertainty estimates of the forecasted values as well as the original data.
#' @export
#'
#'
forecast_insample <- function(model, sample_share = 0.5, uncertainty_sample = 100, exog_fill_method = "AR", plot = TRUE, quiet = FALSE) {

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
    #dplyr::group_by(.data$dep_var) %>%
    dplyr::summarise(min = min(.data$time),
                     max = max(.data$time)) %>%
    dplyr::distinct(dplyr::across(c("max","min"))) -> time_minmax

  # With those times, we can now find the share to forecast
  all_times <- seq(time_minmax$min,time_minmax$max, by = "quarter")
  time_to_use <- all_times[ceiling(length(all_times)*sample_share):length(all_times)]


  # Run the models -----------------------------------------------------------

  all_models <- list()
  for(j in 1:length(time_to_use)){
    if(!quiet){print(paste0("Model Run ",j," up to ",time_to_use[j]))}
    # now let's prepare the model object
    suppressWarnings(
      try(insample_model <- run_model(

        specification = model$args$specification,
        dictionary = model$args$dictionary,
        trend = model$args$trend,
        primary_source = "local",

        max.ar = model$args$max.ar,
        max.dl = model$args$max.dl,
        max.block.size = model$args$max.block.size,

        ardl_or_ecm = model$args$ardl_or_ecm,
        use_logs = model$args$use_logs,
        saturation = model$args$saturation,
        saturation.tpval = model$args$saturation.tpval,
        gets_selection = model$args$gets_selection,
        selection.tpval = model$args$selection.tpval,
        constrain.to.minimum.sample = model$args$constrain.to.minimum.sample,

        pretest_steps = model$args$pretest_steps,

        present = FALSE,
        quiet = TRUE,
        plot = FALSE,

        input = model$processed_input_data %>% dplyr::filter(.data$time <= as.Date(time_to_use[j]))
      ), silent = TRUE))

    if(exists("insample_model")){
      all_models[[j]] <- insample_model
      rm(insample_model)
    }
  }

  model$processed_input_data %>%
    dplyr::distinct(dplyr::across("na_item")) %>%
    dplyr::pull("na_item") -> all_data_vars

  exog_vars <- all_data_vars[!all_data_vars %in% model$module_collection$dependent]

  model$processed_input_data %>%
    dplyr::filter(.data$na_item %in% exog_vars) -> exog_data


  # Forecast Loop ----------------------------------------------------------------
  forecasted_unknownexogvalues <- list()
  rmsfe_values <- list()
  forecast_failures <- list()

  for(i in 1:length(all_models)){
    if(is.null(all_models[[i]])){next}

    start <- time_to_use[i]
    end <- time_to_use[length(time_to_use)]
    if(start == end){next}
    nsteps <- length(seq.Date(from = as.Date(start), to = as.Date(end), by = "quarter")) - 1

    if(!quiet){print(paste0("Forecast ", i, " from ", start, " to ", end))}

    # here starts the loop for the different exog_fill_methods
    intermed_list <- vector(mode = "list", length = length(exog_fill_method))
    names(intermed_list) <- exog_fill_method

    intermed_rmsfe <- vector(mode = "list", length = length(exog_fill_method))
    names(intermed_rmsfe) <- exog_fill_method

    intermed_failure <- vector(mode = "list", length = length(exog_fill_method))
    names(intermed_failure) <- exog_fill_method

    for(forecast_method in exog_fill_method){
      #forecasted_unknownexogvalues[[i]]

      # Run the forecast --------------------------------------------------------


      intermed_list[[forecast_method]] <- forecast_model(model = all_models[[i]],
                                                         n.ahead = nsteps,
                                                         uncertainty_sample = uncertainty_sample,
                                                         plot = FALSE,
                                                         exog_fill_method = forecast_method,
                                                         quiet = TRUE)
      # RMSFE
      intermed_rmsfe[[forecast_method]] <- rmsfe(forecast = intermed_list[[forecast_method]],
                                                 data = model$processed_input_data) %>%
        dplyr::bind_cols(dplyr::tibble(start = start, end = end, method = forecast_method))

      # Forecast Failures
      intermed_failure[[forecast_method]] <- forecast_failure(forecast = intermed_list[[forecast_method]],
                                                              data = model$processed_input_data) %>%
        dplyr::bind_cols(dplyr::tibble(start = start, end = end, method = forecast_method))
    }

    forecasted_unknownexogvalues[[i]] <- intermed_list
    rmsfe_values[[i]] <- dplyr::bind_rows(intermed_rmsfe)
    forecast_failures[[i]] <- dplyr::bind_rows(intermed_failure)
  }

  if(identical(list(),forecasted_unknownexogvalues)){
    stop("Not enough models available for insample comparison. Increase the 'sample_share'.")
  }

  overall_to_plot_central <- dplyr::tibble()
  overall_to_plot_alls <- dplyr::tibble()

  # Prepare output ----------------------------------------------------------

  for(i in 1:length(forecasted_unknownexogvalues)){
    # i = 3
    for (forecast_method in exog_fill_method) {
      if(is.null(forecasted_unknownexogvalues[[i]])){next}

      # get log information
      opts_df <- forecasted_unknownexogvalues[[i]][[forecast_method]]$orig_model$opts_df
      if(!is.null(opts_df[["log_opts"]])){
        opts_df %>%
          dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent, function(opts,dep){
            opts[,dep, drop = TRUE]
          })) %>%
          tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
          tidyr::replace_na(list(log_opts_dependent = "none")) %>%
          dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
      } else {
        log_opts_processed <- dplyr::tibble(dep_var = forecasted_unknownexogvalues[[i]][[forecast_method]]$orig_model$opts_df$dependent, log_opt = "none")
      }


      forecasted_unknownexogvalues[[i]][[forecast_method]]$forecast %>%
        dplyr::select("dep_var","central.estimate") %>%
        dplyr::mutate(start = time_to_use[i]) %>%
        dplyr::full_join(log_opts_processed, by = "dep_var") %>%
        tidyr::unnest("central.estimate") %>%
        tidyr::pivot_longer(-c("dep_var", "start", "time","log_opt")) %>%
        tidyr::drop_na("value") %>%
        dplyr::mutate(value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                               .data$log_opt == "asinh" ~ sinh(.data$value),
                                               .data$log_opt == "none" ~ .data$value)) %>%
        dplyr::select(-c("name","log_opt"))-> centrals

      # this should also work if there are no estimates for uncertainty
      # so this could result in alls remaining empty (no rows)
      # this might happen, if the model only consists of identities and hence
      # there would not be any uncertainty
      forecasted_unknownexogvalues[[i]][[forecast_method]]$forecast %>%
        dplyr::select("dep_var","all.estimates") %>%
        dplyr::mutate(start = time_to_use[i]) %>%
        dplyr::full_join(log_opts_processed, by = "dep_var") %>%

        dplyr::mutate(quantiles = purrr::map(.data$all.estimates, function(x){
          if(!is.null(x)){
            tidyr::pivot_longer(x, -"time") %>%
              dplyr::summarise(max = max(.data$value),
                               min = min(.data$value),
                               p975 = stats::quantile(.data$value, probs = 0.975),
                               p025 = stats::quantile(.data$value, probs = 0.025),
                               p75 = stats::quantile(.data$value, probs = 0.75),
                               p25 = stats::quantile(.data$value, probs = 0.25), .by = "time") %>%
              tidyr::pivot_longer(-"time", names_to = "quantile")
          }
        })) %>%
        dplyr::select(-"all.estimates") %>%

        tidyr::unnest("quantiles") %>%

        {if(nrow(.) > 0){
          dplyr::mutate(.,value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                                   .data$log_opt == "asinh" ~ sinh(.data$value),
                                                   .data$log_opt == "none" ~ .data$value))} else {.}} %>%
        dplyr::select(-"log_opt") -> alls

      dplyr::bind_rows(overall_to_plot_central, centrals %>% dplyr::mutate(method = forecast_method)) -> overall_to_plot_central
      dplyr::bind_rows(overall_to_plot_alls, alls%>% dplyr::mutate(method = forecast_method)) -> overall_to_plot_alls
    }
  }

  if(nrow(overall_to_plot_alls)>0){
    overall_to_plot_alls %>%
      tidyr::pivot_wider(id_cols = c("dep_var","start","time", "method"), names_from = "quantile", values_from = "value") -> overall_to_plot_alls_exp
  } else {
    overall_to_plot_alls_exp <- NULL
  }

  model$full_data %>%
    dplyr::rename(dep_var = "na_item") %>%
    dplyr::filter(.data$dep_var %in% overall_to_plot_central$dep_var,
                  .data$time > min(overall_to_plot_central$start)) -> full_data

  out <- list()
  out$central <- overall_to_plot_central %>%
    dplyr::rename(values = "value") %>%
    dplyr::mutate(end = end, .after = "start")

  out$uncertainty <- overall_to_plot_alls_exp
  out$hist_data <- full_data
  out$rmsfe <- dplyr::bind_rows(rmsfe_values)
  out$forecast_failures <- dplyr::bind_rows(forecast_failures)

  out$args <- list(sample_share = sample_share,
                   time_to_use = time_to_use,
                   all_times = all_times,
                   dep_vars = overall_to_plot_central$dep_var,
                   model = model,
                   exog_fill_method = exog_fill_method)

  out$all_models <- all_models
  out$forecasted_unknownexogvalues <- forecasted_unknownexogvalues

  class(out) <- "osem.forecast.insample"

  if(plot){
    plot(out)
  }

  return(out)
}
