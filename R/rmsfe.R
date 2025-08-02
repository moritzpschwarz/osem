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
    dplyr::rename(forecast = "value",
                  na_item = "dep_var") -> forecasts_processed


  data %>%
    dplyr::filter(.data$time %in% forecasts_processed$time) %>%

    dplyr::right_join(forecasts_processed, by = c("time","na_item")) %>%
    dplyr::mutate(residuals = .data$values - .data$forecast) %>%
    tidyr::drop_na("residuals") %>%
    dplyr::summarise(rmsfe = sqrt(mean(.data$residuals^2)), .by = "na_item") -> rmsfe_indv

  data %>%
    dplyr::filter(.data$time %in% forecasts_processed$time) %>%

    dplyr::right_join(forecasts_processed, by = c("time","na_item")) %>%
    dplyr::mutate(residuals = .data$values - .data$forecast) %>%
    tidyr::drop_na("residuals") %>%
    dplyr::summarise(na_item = "Total RMSFE",
                     rmsfe = sqrt(mean(.data$residuals^2))) -> rmsfe_total

  rmsfe_out <- dplyr::bind_rows(rmsfe_total, rmsfe_indv)

  return(rmsfe_out)
}
