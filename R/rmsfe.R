#' @title Root Mean Squared Forecast Error
#'
#' @param forecast A forecast object as returned by \code{\link{forecast_model}}.
#' @param data A tibble or data.frame containing the original data used for estimation.
#'
#' @returns A tibble containing the root mean squared forecast error estimates.
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
#'insample_output$rmsfe
rmsfe <- function(forecast, data){

  if(!isa(forecast, "osem.forecast")){
    stop("Input object not of type 'osem.forecast'. Run 'forecast_model' again and use the output of that function.")
  }

  # CENTRAL FORECASTS --------
  # get the central forecasts
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


  data %>%
    dplyr::filter(.data$time %in% forecasts_processed$time) %>%

    dplyr::right_join(forecasts_processed, by = c("time","na_item")) %>%
    dplyr::mutate(residuals = .data$values - .data$forecast) %>%
    tidyr::drop_na(.data$residuals) %>%
    dplyr::summarise(rmsfe = sqrt(mean(.data$residuals^2)), .by = "na_item") -> rmsfe_indv

  data %>%
    dplyr::filter(.data$time %in% forecasts_processed$time) %>%

    dplyr::right_join(forecasts_processed, by = c("time","na_item")) %>%
    dplyr::mutate(residuals = .data$values - .data$forecast) %>%
    tidyr::drop_na(.data$residuals) %>%
    dplyr::summarise(na_item = "Total RMSFE",
                     rmsfe = sqrt(mean(.data$residuals^2))) -> rmsfe_total

  rmsfe_out <- dplyr::bind_rows(rmsfe_total, rmsfe_indv)

  return(rmsfe_out)
}
