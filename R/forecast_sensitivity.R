#' Evaluate the sensitivity of forecasts to varying exogenous values
#'
#' @param size Numeric, must be larger than -1. The percentage value that the exogenous values will be multiplied with. Default is 0.5 (i.e. 50%).
#' @param impulse_response Logical. Should an impulse response function be calculated. Main reason to deactivate this might be improving the speed of this function.
#' @inheritParams forecast_model
#'
#' @return A list that contains a tibble with the original forecasts and the difference to the modified forecasts as well as plot(s).
#' @export
#'
#'
forecast_sensitivity <- function(model, size = 0.5, quiet = FALSE, impulse_response = TRUE){

  inital_forecast <- forecast_model(model, quiet = TRUE, exog_fill_method = "AR", plot.forecast = FALSE)

  forecast_list <- list()
  forecast_list_impulse <- list()

  to_manipulate <- inital_forecast$exog_data_nowcast %>%
    dplyr::select(-"time", -dplyr::starts_with("q_")) %>% names

  for(var in to_manipulate){
    # var <- to_manipulate[1]

    if(!quiet){
      cat(paste0("Testing forecast sensitivity by modifiying the exogenous values of ",
                 var," by ",size*100,"%.\n"))
    }


    exog_data_current <- inital_forecast$exog_data_nowcast %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(var), ~ . * (1 + size)))

    cur_forecast <- forecast_model(model, quiet = TRUE,
                                   exog_predictions = exog_data_current, plot.forecast = FALSE)

    forecast_list[[var]] <- cur_forecast

    # impulse response on the first exogenous value of the forecast horizon
    if(impulse_response){
      exog_data_impulse <- inital_forecast$exog_data_nowcast %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(var), ~ dplyr::case_when(time == min(inital_forecast$exog_data$time) ~ . * (1 + size),
                                                                    TRUE ~ .)))

      cur_forecast_impulse <- forecast_model(model, quiet = TRUE,
                                             exog_predictions = exog_data_impulse, plot.forecast = FALSE)

      forecast_list_impulse[[var]] <- cur_forecast_impulse
    }
  }

  process_forecasts <- function(x){
    x$forecast %>%
      dplyr::select("central.estimate") %>%
      tidyr::unnest("central.estimate") %>%
      tidyr::pivot_longer(-"time") %>%
      tidyr::drop_na() %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "name", values_from = "value") -> central_forecasts

    central_forecasts %>%
      names %>%
      stringr::str_detect(., "^ln.") -> to_exponentiate

    central_forecasts %>%
      dplyr::mutate(dplyr::across(.cols = dplyr::all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
      dplyr::rename_with(.fn = ~gsub("ln.","",.)) %>%

      tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
      dplyr::full_join(x$orig_model$module_order %>%
                         dplyr::select("dependent") %>%
                         dplyr::rename(na_item = "dependent"), by = "na_item") %>%

      dplyr::mutate(fit = "forecast") -> forecasts_processed

    return(forecasts_processed)
  }

  dplyr::tibble(modified = names(forecast_list),
                forecasts = forecast_list) %>%
    dplyr::mutate(forecasts_processed = purrr::map(forecasts, process_forecasts)) %>%
    dplyr::select(-forecasts) %>%
    tidyr::unnest(forecasts_processed) -> all_modified

  dplyr::tibble(forecasts = list(inital_forecast)) %>%
    dplyr::mutate(inital_forecast = purrr::map(forecasts, process_forecasts)) %>%
    tidyr::unnest(inital_forecast) %>%
    dplyr::select(-"forecasts", -"fit") %>%
    dplyr::rename(init = values) -> inital_forecast_tib


  all_modified %>%
    dplyr::full_join(inital_forecast_tib, by = c("time","na_item")) %>%
    dplyr::mutate(diff = (values - init)) -> final_forecast_sensitivity

  final_forecast_sensitivity %>%

    ggplot2::ggplot(ggplot2::aes(x = time, y = diff, color = modified)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "black", linewidth = 1) +
    ggplot2::geom_line() +

    ggplot2::facet_wrap(~na_item, scales = "free") +

    ggplot2::scale_color_brewer(palette = "Spectral", name = "Exogenous Variable modified") +
    ggplot2::labs(title = paste0("Effect of modifying exogenous values by ",size*100,"%."),
                  x = NULL,
                  y = NULL) +


    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank()) -> p




  if(impulse_response){
    dplyr::tibble(modified = names(forecast_list_impulse),
                  forecasts = forecast_list_impulse) %>%
      dplyr::mutate(forecasts_processed = purrr::map(forecasts, process_forecasts)) %>%
      dplyr::select(-forecasts) %>%
      tidyr::unnest(forecasts_processed) -> all_modified_impulse


    all_modified_impulse %>%
      dplyr::full_join(inital_forecast_tib, by = c("time","na_item")) %>%
      dplyr::mutate(diff = (values - init)) -> final_forecast_sensitivity_impulse

    final_forecast_sensitivity_impulse %>%

      ggplot2::ggplot(ggplot2::aes(x = time, y = diff, color = modified)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0), colour = "black", linewidth = 1) +
      ggplot2::geom_line() +

      ggplot2::facet_wrap(~na_item, scales = "free") +

      ggplot2::scale_color_brewer(palette = "Spectral", name = "Exogenous Variable modified") +
      ggplot2::labs(title = paste0("Effect of modifying exogenous values by ",size*100,"%."),
                    x = NULL,
                    y = NULL) +


      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major.x = ggplot2::element_blank()) -> p_impulse

  }


  out <- list()
  out$forecast_sensitivity <- final_forecast_sensitivity
  out$plot <- p
  if(impulse_response){
    out$forecast_sensitivity_impulse_response <- final_forecast_sensitivity_impulse
    out$plot_impulse_response <- p_impulse
  }


  return(out)
}
