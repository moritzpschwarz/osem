mincer.zarnowitz <- function(forecast,
                             data){

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


  # # ACTUALS --------
  # get the actuals
  data %>%
    dplyr::filter(.data$time %in% forecasts_processed$time) %>%

    dplyr::right_join(forecasts_processed, by = c("time","na_item")) %>%
    dplyr::select(-c("na_item")) %>%
    dplyr::rename(actual = "value") -> data_forecasts

 # use this set of instructions and implement
  # ## Estimate the Mincer-Zarnowitz regression
  # fit <- lm(y ~ yhat)
  #
  # # Do a Wald test
  # # Note: depending on the data you might want to use a
  # # robust variance-covariance estimator
  # b <- coef(fit)
  # cov <- vcov(fit)
  # s <- (b - c(0, 1)) %*% solve(cov) %*% c(b - c(0, 1))
  #
  # # p-value for this test
  # pchisq(s, 2, lower.tail = FALSE)
  # now implement this
  # # Estimate the Mincer-Zarnowitz regression
  fit <- lm(actual ~ forecast, data = data_forecasts)



}
