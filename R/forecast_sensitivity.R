#' Evaluate the sensitivity of forecasts to varying exogenous values
#'
#' @param size Numeric, must be larger than -1. The percentage value that the exogenous values will be multiplied with. Default is 0.5 (i.e. 50%).
#' @inheritParams forecast_model
#'
#' @return A tibble with the original forecasts and the difference to the modified forecasts.
#' @export
#'
#'
forecast_sensitivity <- function(model, size = 0.5, quiet = FALSE){

  inital_forecast <- forecast_model(model, quiet = TRUE, exog_fill_method = "AR", plot = FALSE)

  forecast_list <- list()

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
                                   exog_predictions = exog_data_current, plot = FALSE)
    forecast_list[[var]] <- cur_forecast

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

  dplyr::tibble(#modified = "Original",
    forecasts = list(inital_forecast)) %>%
    dplyr::mutate(inital_forecast = purrr::map(forecasts, process_forecasts)) %>%
    tidyr::unnest(inital_forecast) %>%
    dplyr::select(-"forecasts", -"fit") %>%
    dplyr::rename(init = values) -> inital_forecast_tib


  all_modified %>%
    dplyr::full_join(inital_forecast_tib, by = c("time","na_item")) %>%
    dplyr::mutate(diff = (values - init)) -> final_forecast_sensitivity

  final_forecast_sensitivity %>%

    ggplot2::ggplot(aes(x = time, y = diff, color = modified)) +
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


  out <- list()
  out$final_forecast_sensitivity
  out$plot <- p

  return(out)
}
