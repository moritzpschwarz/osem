#' @title Determine the number of Forecast Failures in Insample Forecasting
#'
#' @param forecast A forecast object as returned by \code{\link{forecast_model}}.
#' @param data A tibble or data.frame containing the original data used for estimation.
#'
#' @returns A tibble representing information about the success and failures of forecasting.
#'
#' @examples
#'
#'specification <- dplyr::tibble(
#'  type = c(
#'    "n"
#'  ),
#'  dependent = c(
#'    "FinConsExpHH"
#'  ),
#'  independent = c(
#'    "FinConsExpGov + HICP_Gas"
#'  ),
#'  lag = c(
#'    ""
#'  ),
#'  cvar = c(
#'    ""
#'  )
#')
#'
#'set.seed(123)
#'testdata <- dplyr::tibble(
#'  time = seq.Date(from = as.Date("2005-01-01"),
#'                  to = as.Date("2023-10-01"),
#'                  by = "quarter"),
#'  FinConsExpGov = rnorm(mean = 100, n = length(time)),
#'  HICP_Gas = rnorm(mean = 200, n = length(time)),
#'  FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 *
#'    HICP_Gas + rnorm(length(time), mean = 0, sd = 0.2))
#'
#'testdata <- tidyr::pivot_longer(testdata,
#'                                cols = -time,
#'                                names_to = "na_item",
#'                                values_to = "values")
#'
#'model <- run_model(specification = specification,
#'                   dictionary = dict,
#'                   input = testdata,
#'                   primary_source = "local",
#'                   present = FALSE,
#'                   quiet = TRUE,
#'                   saturation = "IIS")
#'
#'insample_output <- forecast_insample(model, sample_share = 0.97)
#'insample_output$forecast_failures

forecast_failure <- function(forecast, data){

  if(!isa(forecast, "osem.forecast")){
    stop("Input object not of type 'osem.forecast'. Run 'forecast_model' again and use the output of that function.")
  }

  # get log information
  if(!is.null(forecast$orig_model$opts_df[["log_opts"]])){
    forecast$orig_model$opts_df %>%
      dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent, function(opts,dep){
        opts[,dep, drop = TRUE]
      })) %>%
      tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
      tidyr::replace_na(list(log_opts_dependent = "none")) %>%
      dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
  } else {
    log_opts_processed <- dplyr::tibble(dep_var = forecast$orig_model$opts_df$dependent, log_opt = "none")
  }

  # # CENTRAL FORECASTS --------
  # # get the central forecasts
  forecast$forecast %>%
    dplyr::select("dep_var", "central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-c("time","dep_var")) %>%
    tidyr::drop_na() %>%
    dplyr::full_join(log_opts_processed, by = "dep_var") %>%
    dplyr::mutate(value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                           .data$log_opt == "asinh" ~ sinh(.data$value),
                                           .data$log_opt == "none" ~ .data$value)) %>%
    dplyr::select(-c("name", "log_opt")) %>%
    dplyr::rename(forecast = "value") -> forecasts_processed

  # ALL FORECASTS --------
  # Dealing with uncertainty (all forecasts)
  # unnest the set of all estimates from the original object
  forecast$forecast %>%
    dplyr::select("dep_var", "all.estimates") %>%
    dplyr::full_join(log_opts_processed, by = "dep_var") %>%
    tidyr::unnest("all.estimates") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("run_"), ~dplyr::case_when(.data$log_opt == "log" ~ exp(.),
                                                                              .data$log_opt == "asinh" ~ sinh(.),
                                                                              .data$log_opt == "none" ~ .))) %>%
    dplyr::select(-"log_opt") -> all_forecasts_unnested

  if(nrow(all_forecasts_unnested) > 0){
    all_forecasts_unnested %>%
      tidyr::pivot_longer(-c("time", "dep_var"))  %>%

      dplyr::rename(na_item = "dep_var", values = "value") %>%
      dplyr::full_join(forecast$orig_model$module_order %>%
                         dplyr::select("dependent") %>%
                         dplyr::rename(na_item = "dependent"), by = "na_item") -> all_forecasts_processed

    all_forecasts_processed %>%
      tidyr::drop_na("time") %>%
      dplyr::group_by(.data$na_item, .data$time) %>%
      dplyr::summarise(
        min = min(.data$values),
        max = max(.data$values),
        p025 = stats::quantile(.data$values, probs = 0.025),
        p975 = stats::quantile(.data$values, probs = 0.975),
        p05 = stats::quantile(.data$values, probs = 0.05),
        p95 = stats::quantile(.data$values, probs = 0.95),
        p25 = stats::quantile(.data$values, probs = 0.25),
        p75 = stats::quantile(.data$values, probs = 0.75)) %>%
      dplyr::ungroup() -> all_forecasts_processed_q

    data %>%
      dplyr::filter(.data$time %in% unique(all_forecasts_processed_q$time),
                    .data$na_item %in% unique(all_forecasts_processed_q$na_item)) %>%
      dplyr::full_join(all_forecasts_processed_q, by = c("time", "na_item")) %>%
      dplyr::mutate(failure_all = dplyr::case_when(values >= min & values <= max  ~ "Success", TRUE ~ "Failure"),
                    failure_95 = dplyr::case_when(values >= p025 & values <= p975 ~ "Success", TRUE ~ "Failure"),
                    failure_90 = dplyr::case_when(values >= p05 & values <= p95 ~ "Success", TRUE ~ "Failure"),
                    failure_50 = dplyr::case_when(values >= p25 & values <= p75 ~ "Success", TRUE ~ "Failure")) %>%
      dplyr::select(c("time","na_item", "values", "failure_all", "failure_95", "failure_90", "failure_50")) -> failures_intermed

    return(failures_intermed)
  }
}
