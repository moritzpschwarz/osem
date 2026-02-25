#' Evaluate the sensitivity of forecasts to varying exogenous values
#'
#' This function evaluates the sensitivity of forecasts to varying exogenous values by modifying the exogenous values in the forecast horizon and comparing the resulting forecasts to the original forecasts.
#' The function allows for both percentage-based and unit-based modifications of the exogenous values, and can also calculate impulse response functions if desired.
#'
#' @param size Numeric. If 'size_type' = 'pct' then 'size' must be larger than -1 as it represents the percentage value that the exogenous values will be multiplied with. Default is 0.5 (i.e. 50%).
#' If 'size_type' = 'unit', then 'size' represents the unit value that will be added to the exogenous values (e.g. to run a one-unit IRF, use 'size = 1' and size_type = 'unit').
#' @param size_type Character. Either "pct" or "unit". If "pct", the exogenous values will be multiplied with (1 + size). If "unit", the exogenous values will be increased by size. Default is "pct".
#' @param impulse_response Logical. Should an impulse response function be calculated. Main reason to deactivate this might be improving the speed of this function.
#' @param grepl_variables Regular Expression Character. Can be used to select variables to be included in the sensitivity procedure.
#' @param exclude_zero_change Logical. Should cases where the change in forecasts is exactly zero be excluded from the output (i.e. set to NA).
#' This can be useful if there are many variables and many of them have no effect on the forecasts, which can make the plot easier to read.
#' This can happen if e.g. the modified variable is not part of the specification of the module or has been removed due to model selection.
#' @param include_uncertainty Logical. Should the uncertainty of the forecasts be included in the plot. If TRUE, the function will calculate the difference in all forecast runs (not just the central estimate) between the modified and original forecasts and show the 5-95% and 25-75% quantiles of these differences in the plot as shaded areas.
#'
#' @inheritParams forecast_model
#'
#' @return A list that contains a tibble with the original forecasts and the difference to the modified forecasts as well as plot(s).
#' @export
#'
#'
forecast_sensitivity <- function(
    model,
    size = 0.5,
    size_type = "pct",
    grepl_variables = NULL,
    quiet = FALSE,
    impulse_response = TRUE,
    exog_fill_method = "AR",
    exclude_zero_change = TRUE,
    include_uncertainty = TRUE
){

  if(!size_type %in% c("pct", "unit")){
    stop("size_type must be either 'pct' or 'unit'.")
  }

  if(size_type == "pct" & size <= -1){
    stop("When size_type is 'pct', size must be larger than -1.")
  }

  inital_forecast <- forecast_model(model, quiet = TRUE, exog_fill_method = exog_fill_method, plot = FALSE)

  forecast_list <- list()
  forecast_list_impulse <- list()

  to_manipulate <- inital_forecast$exog_data_nowcast %>%
    dplyr::select(-"time", -dplyr::starts_with("q_")) %>% names

  if(!is.null(grepl_variables)){
    to_manipulate <- to_manipulate[grepl(grepl_variables, to_manipulate)]
  }

  # Forecast Loop -----------------------------------------------------------

  for(var in to_manipulate){
    # var <- to_manipulate[1]

    if(!quiet){
      size_msg <- paste0(if(size_type == "pct") {
        c(size*100, "%.\n")
      } else {
        paste0(size,".\n")
      }, collapse = "")

      cat(paste0("Testing forecast sensitivity by modifiying the exogenous values of ",
                 var," by ",size_msg))
    }

    exog_data_current <- inital_forecast$exog_data_nowcast %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(var), ~  if(size_type == "pct"){. * (1 + size)} else if(size_type == "unit") {. + size}))

    cur_forecast <- forecast_model(model, quiet = TRUE,
                                   exog_predictions = exog_data_current, plot = FALSE)

    forecast_list[[var]] <- cur_forecast

    # impulse response on the first exogenous value of the forecast horizon
    if(impulse_response){
      exog_data_impulse <- inital_forecast$exog_data_nowcast %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(var), ~ dplyr::case_when(
          time == min(inital_forecast$exog_data$time) ~ if(size_type == "pct"){. * (1 + size)} else if(size_type == "unit") {. + size},
          TRUE ~ .)))

      cur_forecast_impulse <- forecast_model(model, quiet = TRUE,
                                             exog_predictions = exog_data_impulse, plot = FALSE)

      forecast_list_impulse[[var]] <- cur_forecast_impulse
    }
  }

  # Process functions -------------------------------------------------------

  process_forecasts <- function(x){

    # get log information
    if(!is.null(x$orig_model$opts_df[["log_opts"]])){
      x$orig_model$opts_df %>%
        tidyr::nest(dependent = "dependent") %>%
        # separate any comma separated rows in dependent
        dplyr::mutate(dependent = purrr::map(.data$dependent, function(dep_row){
          dep_val <- dep_row$dependent
          if(grepl(",",  dep_val)){
            dep_vars <- trimws(unlist(strsplit(dep_val, ",")))
            return(dep_vars)
          } else {
            return(dep_val)
          }
        })) %>%
        tidyr::unnest("dependent") %>%

        dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent, function(opts,dep){as.character(opts[,dep, drop = TRUE])})) %>%
        tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
        tidyr::replace_na(list(log_opts_dependent = "none")) %>%
        dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
    } else {
      log_opts_processed <- dplyr::tibble(dep_var = x$orig_model$opts_df$dependent, log_opt = "none")
    }


    x$forecast %>%
      dplyr::select("dep_var", "central.estimate") %>%
      tidyr::unnest("central.estimate") %>%
      dplyr::rename(na_item = "dep_var") %>%
      #dplyr::select(-"dep_var") %>%
      tidyr::pivot_longer(-c("time", "na_item"), names_to = "dep_var") %>%
      dplyr::mutate(dep_var = gsub("^ln\\.|^D\\.|^D\\.ln\\.|^ln\\.D\\.","",.data$dep_var)) %>%
      tidyr::drop_na("value") %>%
      # dplyr::mutate(dep_var = .data$na_item,
      #               na_item = NULL) %>%
      dplyr::select(-"na_item") %>%
      dplyr::full_join(log_opts_processed, by = "dep_var") %>%
      dplyr::mutate(value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                             .data$log_opt == "asinh" ~ sinh(.data$value),
                                             .data$log_opt == "none" ~ .data$value)) %>%
      dplyr::select(-c("log_opt")) %>%
      dplyr::rename(values = "value",
                    na_item = "dep_var") %>%
      tidyr::drop_na("values") %>%
      dplyr::mutate(fit = "forecast") -> forecasts_processed

    out <- list()
    out$forecasts_processed <- forecasts_processed
    return(out)
  }
  process_forecasts_all_diff <- function(mod, init){
    # get log information
    if(!is.null(init$orig_model$opts_df[["log_opts"]])){
      init$orig_model$opts_df %>%
        tidyr::nest(dependent = "dependent") %>%
        # separate any comma separated rows in dependent
        dplyr::mutate(dependent = purrr::map(.data$dependent, function(dep_row){
          dep_val <- dep_row$dependent
          if(grepl(",",  dep_val)){
            dep_vars <- trimws(unlist(strsplit(dep_val, ",")))
            return(dep_vars)
          } else {
            return(dep_val)
          }
        })) %>%
        tidyr::unnest("dependent") %>%

        dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent,
                                                       function(opts,dep){as.character(opts[,dep, drop = TRUE])})) %>%
        tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
        tidyr::replace_na(list(log_opts_dependent = "none")) %>%
        dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
    } else {
      log_opts_processed <- dplyr::tibble(dep_var = init$orig_model$opts_df$dependent, log_opt = "none")
    }

    # ALL FORECASTS ---
    # Dealing with uncertainty (all forecasts)
    # unnest the set of all estimates from the original object
    mod$forecast %>%
      dplyr::select("na_item" = "dep_var", "all.estimates") %>%
      tidyr::unnest("all.estimates") %>%

      # check if dep_var column exists
      {if("dep_var" %in% names(.)){
        # if the dep_var column exists, combine with na_item - use dep_var over na_item
        dplyr::mutate(.,na_item = dplyr::case_when(
          !is.na(.data$dep_var) ~ gsub("ln.|D.|D.ln.|ln.D.","",.data$dep_var), TRUE ~ .data$na_item)) %>%
          dplyr::select(-"dep_var")} else {.}} %>%

      dplyr::rename("dep_var" = "na_item") %>%
      dplyr::full_join(log_opts_processed, by = "dep_var") %>%

      dplyr::mutate(dplyr::across(dplyr::starts_with("run_"), ~dplyr::case_when(.data$log_opt == "log" ~ exp(.),
                                                                                .data$log_opt == "asinh" ~ sinh(.),
                                                                                .data$log_opt == "none" ~ .))) %>%
      dplyr::select(-"log_opt") -> all_forecasts_unnested_mod

    init$forecast %>%
      dplyr::select("na_item" = "dep_var", "all.estimates") %>%
      tidyr::unnest("all.estimates") %>%

      # check if dep_var column exists
      {if("dep_var" %in% names(.)){
        # if the dep_var column exists, combine with na_item - use dep_var over na_item
        dplyr::mutate(.,na_item = dplyr::case_when(
          !is.na(.data$dep_var) ~ gsub("ln.|D.|D.ln.|ln.D.","",.data$dep_var), TRUE ~ .data$na_item)) %>%
          dplyr::select(-"dep_var")} else {.}} %>%

      dplyr::rename("dep_var" = "na_item") %>%
      dplyr::full_join(log_opts_processed, by = "dep_var") %>%

      dplyr::mutate(dplyr::across(dplyr::starts_with("run_"), ~dplyr::case_when(.data$log_opt == "log" ~ exp(.),
                                                                                .data$log_opt == "asinh" ~ sinh(.),
                                                                                .data$log_opt == "none" ~ .))) %>%
      dplyr::select(-"log_opt") -> all_forecasts_unnested_init



    id <- all_forecasts_unnested_mod %>%
      dplyr::select("dep_var", "time")

    mod_alls <- all_forecasts_unnested_mod %>%
      dplyr::select(-c("dep_var", "time"))

    init_alls <- all_forecasts_unnested_init %>%
      dplyr::select(-c("dep_var", "time"))

    all_diffs <- (mod_alls - init_alls) %>% dplyr::bind_cols(id, .)

    if(nrow(all_diffs) > 0){
      all_diffs %>%
        tidyr::pivot_longer(-c("time", "dep_var")) %>%
        dplyr::rename(na_item = "dep_var", values = "value") %>%

        dplyr::mutate(fit = "Forecast Uncertainty")  %>%

        tidyr::drop_na("time") %>%
        dplyr::group_by(.data$na_item, .data$time, .data$fit) %>%
        dplyr::summarise(
          p95 = stats::quantile(.data$values, probs = 0.95),
          p05 = stats::quantile(.data$values, probs = 0.05),
          p975 = stats::quantile(.data$values, probs = 0.975),
          p025 = stats::quantile(.data$values, probs = 0.025),
          p75 = stats::quantile(.data$values, probs = 0.75),
          p25 = stats::quantile(.data$values, probs = 0.25)) %>%
        dplyr::ungroup() -> all_forecasts_processed_q
    }

    return(all_forecasts_processed_q)
  }
  process_wrapper <- function(forecast_list, inital_forecast,exclude_zero_change){

    # Process modified central estimates
    dplyr::tibble(modified = names(forecast_list),
                  forecasts = forecast_list) %>%
      dplyr::mutate(forecasts_processed = purrr::map(.data$forecasts, process_forecasts)) %>%
      dplyr::mutate(central = purrr::map(.data$forecasts_processed, function(x){x$forecasts_processed}),
                    all = purrr::map(.data$forecasts_processed, function(x){x$all_forecasts_processed_q})) %>%
      dplyr::select(-c("forecasts", "forecasts_processed", "all")) %>%
      tidyr::unnest("central") -> central_modified

    # Process initial central estimates
    dplyr::tibble(forecasts = list(inital_forecast)) %>%
      dplyr::mutate(inital_forecast = purrr::map(.data$forecasts, process_forecasts)) %>%
      dplyr::mutate(central = purrr::map(.data$inital_forecast, function(x){x$forecasts_processed}),
                    all = purrr::map(.data$inital_forecast, function(x){x$all_forecasts_processed_q})) %>%
      tidyr::unnest("central") %>%
      dplyr::select(-c("forecasts", "fit", "inital_forecast", "all")) %>%
      dplyr::rename(init = .data$values) -> inital_forecast_tib_central

    dplyr::tibble(modified = names(forecast_list),
                  mod = forecast_list,
                  init = list(inital_forecast)) %>%
      dplyr::mutate(all = purrr::map2(.data$mod, .data$init, process_forecasts_all_diff)) %>%
      dplyr::select(-c("mod", "init")) %>%
      tidyr::unnest("all") -> uncertainties

    central_modified %>%
      dplyr::full_join(inital_forecast_tib_central, by = c("time","na_item")) %>%
      dplyr::mutate(diff = .data$values - .data$init) -> final_forecast_sensitivity_central

    if(exclude_zero_change){
      final_forecast_sensitivity_central %>%
        # if diff is 0 and exclude_zero_change is TRUE, set diff to NA
        dplyr::summarise(all_zero = all(.data$diff == 0, na.rm = TRUE),
                         .by = c("na_item", "modified")) -> all_zero_na_items_df

      final_forecast_sensitivity_central %>%
        dplyr::full_join(all_zero_na_items_df, by = c("na_item", "modified")) %>%
        dplyr::filter(!(.data$all_zero == TRUE)) %>%
        dplyr::select(-"all_zero") -> final_forecast_sensitivity_central

      uncertainties  %>%
        dplyr::full_join(all_zero_na_items_df, by = c("na_item", "modified")) %>%
        dplyr::filter(!(.data$all_zero == TRUE)) %>%
        dplyr::select(-"all_zero") -> uncertainties
    }

    out <- list()
    out$final <- final_forecast_sensitivity_central
    out$uncertainties <- uncertainties
    return(out)

  }

  # Applying Process functions -------------------------------------------------------
  process_wrapper(forecast_list, inital_forecast, exclude_zero_change = exclude_zero_change) -> processed_forecasts
  if(impulse_response) {process_wrapper(forecast_list_impulse, inital_forecast, exclude_zero_change = exclude_zero_change) -> processed_forecasts_impulse}


  # Figures -----------------------------------------------------------------


  ## Full forecast Figure ------------

  if(include_uncertainty){
    forecast_uncertainty <- list(
      ggplot2::geom_ribbon(data = processed_forecasts$uncertainties, ggplot2::aes(x = .data$time, ymin = .data$p025, ymax = .data$p975), fill = "blue", alpha = 0.1, inherit.aes = FALSE),
      ggplot2::geom_ribbon(data = processed_forecasts$uncertainties, ggplot2::aes(x = .data$time, ymin = .data$p05, ymax = .data$p95), fill = "blue", alpha = 0.1, inherit.aes = FALSE),
      ggplot2::geom_ribbon(data = processed_forecasts$uncertainties, ggplot2::aes(x = .data$time, ymin = .data$p25, ymax = .data$p75), fill = "blue", alpha = 0.1, inherit.aes = FALSE))
  } else {
    forecast_uncertainty <- NULL
  }

  processed_forecasts$final %>%

    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$diff)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "black") +

    forecast_uncertainty +

    ggplot2::geom_line(linewidth = 1, color = "darkred") +

    #ggplot2::facet_wrap(~.data$na_item, scales = "free") +
    ggplot2::facet_grid(.data$na_item ~ .data$modified, scales = "free") +

    #ggplot2::scale_color_brewer(palette = "Spectral", name = "Exogenous Variable modified") +
    ggplot2::labs(#title = paste0("Effect of modifying exogenous values by ",size*100,"%."),
      title = paste0("Effect of modifying exogenous variables (Columns) by ",if(size_type == "pct") {
        paste0(c(size*100, "%.\n"), collapse = "")
      } else {
        paste0(size,".\n")
      }, collapse = ""),
      x = NULL,
      y = NULL) +


    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank()) -> p



  ## Impulse Figure ------------

  if(impulse_response){

    if(include_uncertainty){
      impulse_uncertainty <- list(ggplot2::geom_ribbon(data = processed_forecasts_impulse$uncertainties, ggplot2::aes(x = .data$time, ymin = .data$p025, ymax = .data$p975), fill = "blue", alpha = 0.1, inherit.aes = FALSE),
                                  ggplot2::geom_ribbon(data = processed_forecasts_impulse$uncertainties, ggplot2::aes(x = .data$time, ymin = .data$p05, ymax = .data$p95), fill = "blue", alpha = 0.1, inherit.aes = FALSE),
                                  ggplot2::geom_ribbon(data = processed_forecasts_impulse$uncertainties, ggplot2::aes(x = .data$time, ymin = .data$p25, ymax = .data$p75), fill = "blue", alpha = 0.1, inherit.aes = FALSE))
    } else {
      impulse_uncertainty <- NULL
    }

    processed_forecasts_impulse$final %>%

      ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$diff, color = .data$modified)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "black") +

      impulse_uncertainty +

      ggplot2::geom_line(linewidth = 1, color = "darkred") +

      ggplot2::facet_grid(.data$na_item ~ .data$modified, scales = "free") +

      ggplot2::scale_color_brewer(palette = "Spectral", name = "Exogenous Variable modified") +
      ggplot2::labs(#title = paste0("Effect of modifying exogenous values by ",size*100,"%."),
        title = paste0("Impulse response of modifying exogenous variables (Columns) by ",if(size_type == "pct") {
          paste0(c(size*100, "%.\n"), collapse = "")
        } else {
          paste0(size,".\n")
        }, collapse = ""),
        x = NULL,
        y = NULL) +


      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank()) -> p_impulse

  }

  lapply(forecast_list, FUN = function(x){
    x$full_forecast_data}) %>%
    dplyr::bind_rows(.id = "modified") %>%
    dplyr::mutate(modified = factor(.data$modified, levels = names(forecast_list))) -> full_forecast_data

  out <- list()
  out$forecast_sensitivity <- processed_forecasts$final
  out$plot <- p
  if(impulse_response){
    out$forecast_sensitivity_impulse_response <- processed_forecasts_impulse$final
    out$plot_impulse_response <- p_impulse
  }
  if(include_uncertainty){
    out$uncertainties <- processed_forecasts$uncertainties
    if(impulse_response){
      out$uncertainties_impulse_response <- processed_forecasts_impulse$uncertainties
    }
  }
  full_forecast_data <- inital_forecast$full_forecast_data %>%
    dplyr::mutate(type = "Initial Forecast")


  return(out)
}
