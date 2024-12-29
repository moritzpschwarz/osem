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
#'                   inputdata_directory = testdata,
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

  # # CENTRAL FORECASTS --------
  # # get the central forecasts
  forecast$forecast %>%
    dplyr::select("central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-"time") %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "name", values_from = "value") -> central_forecasts

  # find out which of the central forecasts should be exponentiated (because they were run in ln)
  central_forecasts %>%
    names %>%
    stringr::str_detect(., "^ln.") -> to_exponentiate


  central_forecasts %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
    dplyr::rename_with(.fn = ~gsub("ln.","",.)) %>%

    tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
    dplyr::full_join(forecast$orig_model$module_order %>%
                       dplyr::select("dependent") %>%
                       dplyr::rename(na_item = "dependent"), by = "na_item") %>%

    dplyr::rename(forecast = "values") -> forecasts_processed

  # ALL FORECASTS --------
  # Dealing with uncertainty (all forecasts)
  # unnest the set of all estimates from the original object
  forecast$forecast %>%
    dplyr::select("dep_var", "all.estimates") %>%
    tidyr::unnest("all.estimates") -> all_forecasts_unnested

  if(nrow(all_forecasts_unnested) > 0){
    all_forecasts_unnested %>%
      tidyr::pivot_longer(-c("time", "dep_var")) -> all_forecasts

    dplyr::tibble(dep_var = names(central_forecasts),
                  expo = to_exponentiate,
                  all = c("time",unique(forecast$forecast$dep_var))) -> to_exponentiate_tibble

    all_forecasts %>%
      dplyr::mutate(value = dplyr::case_when(.data$dep_var %in% to_exponentiate_tibble$all[to_exponentiate_tibble$expo == TRUE] ~ exp(.data$value),
                                             TRUE ~ .data$value)) %>%
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
