#' Creates baseline forecasts for comparison with OSEM
#' @inheritParams forecast_model
#' @param forecast_type Type of forecast. Either "AR" for autoregressive or "RW" for random walk.
#' @param lags Number of lags to include in the AR model. Ignore for RW model. Default chooses the same as the max.ar setting of the model object.
#' @param mc Logical value whether to include in intercept in the AR model or not.
#'
#' @return Returns a data frame with the point forecasts.
#'
#' @details
#' The function first determines the maximum forecast horizon by adding the n.ahead argument to the most recent data observation across all modules. For variables
#' whose forecast origin is before that, it creates additional forecasts up to the forecast origin. Hence, the actual number of forecasted values may differ across
#' variables.
#'
#' When the forecast type is "AR", the function first transforms the variable into logs (if only positive values observed), otherwise using the asinh transformation.
#' Reported forecast values are after conversion back to the level of the variable. Also, the AR model is estimated on the same (sub)sample as the OSEM model. Since
#' OSEM modules may contain explanatory variables whose availability restricts the sample, we ensure that the AR model is estimated on the same data as OSEM to ensure
#' a fair comparison.
#'


forecast_comparison <- function(model, n.ahead, forecast_type = c("AR", "RW"), lags = NULL, mc = TRUE) {

  # extract model info
  lags <- if (is.null(lags)) {model$args$max.ar} else {lags}
  modules <- model$module_collection %>% dplyr::arrange(order)
  transformations <- model$opts_df %>% dplyr::select("dependent", "log_opts")
  fulldata <- model$full_data
  maxtime <- fulldata %>% dplyr::filter(!grepl("\\.hat$", .data$na_item)) %>% tidyr::drop_na() %>% dplyr::pull("time") %>% max()
  maxhorizon <- seq.Date(maxtime, by = "quarter", length.out = (n.ahead + 1))[n.ahead + 1]

  # set up forecast output
  out <- data.frame()

  for (i in 1:NROW(modules)) {

    type <- modules[i, "type"][[1]]
    depvar <- modules[i, "dependent"][[1]]

    if (forecast_type == "AR") {

      if (type == "n") { # can extract data from module
        estimated_model <- modules[i, "model"][[1]][[1]]
        data <- estimated_model$aux$y
        maxtime_model <- max(estimated_model$aux$y.index)
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        ar_model <- gets::arx(y = data, mc = mc, ar = 1:lags)
        ar_model$call$mc <- mc
        ar_model$call$ar <- 1:lags
        fc <- gets::predict.arx(ar_model, n.ahead = n.ahead_model)
        transformations_model <- transformations %>% dplyr::filter(.data$dependent == depvar) %>% dplyr::pull("log_opts") %>% purrr::pluck(1) %>% dplyr::pull("depvar")
        stopifnot(transformations_model %in% c("log", "asinh", "level"))
        if (transformations_model == "log") {
          fc_level <- exp(fc)
        } else if (transformations_model == "asinh") {
          fc_level <- sinh(fc)
        } else {
          fc_level <- fc
        }
      } else if (type == "d") { # do model object to extract data from
        mintime_model <- fulldata %>% dplyr::filter(.data$na_item == paste0(depvar, ".hat")) %>% tidyr::drop_na() %>% dplyr::pull("time") %>% min() # first model value
        data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% dplyr::filter(.data$time >= mintime_model)
        maxtime_model <- data %>% tidyr::drop_na() %>% dplyr::pull("time") %>% max()
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        if (any(data$values <= 0, na.rm = TRUE)) {
          data <- data %>% dplyr::mutate(values_trans = asinh(.data$values))
          ar_model <- gets::arx(y = data$values_trans, mc = mc, ar = 1:lags)
          ar_model$call$mc <- mc
          ar_model$call$ar <- 1:lags
          fc <- gets::predict.arx(ar_model, n.ahead = n.ahead_model)
          fc_level <- sinh(fc)
        } else {
          data <- data %>% dplyr::mutate(values_trans = log(.data$values))
          ar_model <- gets::arx(y = data$values_trans, mc = mc, ar = 1:lags)
          ar_model$call$mc <- mc
          ar_model$call$ar <- 1:lags
          fc <- gets::predict.arx(ar_model, n.ahead = n.ahead_model)
          fc_level <- exp(fc)
        }
      } else {
        stop("type not recognized")
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "RW") {

      data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% tidyr::drop_na() %>% dplyr::arrange("time") %>% dplyr::slice_tail(n = 1)
      maxtime_model <- data %>% dplyr::pull("time")
      n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = data$values)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "auto"){

      if (type == "n") { # can extract data from module
        estimated_model <- modules[i, "model"][[1]][[1]]
        data <- estimated_model$aux$y
        maxtime_model <- max(estimated_model$aux$y.index)
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1

        arima_model <- forecast::auto.arima(data)
        fc <- forecast::forecast(arima_model, h = n.ahead_model)

        transformations_model <- transformations %>% dplyr::filter(.data$dependent == depvar) %>% dplyr::pull("log_opts") %>% purrr::pluck(1) %>% dplyr::pull("depvar")
        stopifnot(transformations_model %in% c("log", "asinh", "level"))
        if (transformations_model == "log") {
          fc_level <- exp(as.numeric(fc$mean))
        } else if (transformations_model == "asinh") {
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          fc_level <- fc
        }
      } else if (type == "d") { # do model object to extract data from
        mintime_model <- fulldata %>% dplyr::filter(.data$na_item == paste0(depvar, ".hat")) %>% tidyr::drop_na() %>% dplyr::pull("time") %>% min() # first model value
        data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% dplyr::filter(.data$time >= mintime_model)
        maxtime_model <- data %>% tidyr::drop_na() %>% dplyr::pull("time") %>% max()
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        if (any(data$values <= 0, na.rm = TRUE)) {
          data <- data %>% dplyr::mutate(values_trans = asinh(.data$values))
          arima_model <- forecast::auto.arima(data$values_trans)
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          data <- data %>% dplyr::mutate(values_trans = log(.data$values))
          arima_model <- forecast::auto.arima(data$values_trans)
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- exp(as.numeric(fc$mean))
        }
      } else {
        stop("type not recognized")
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "ets"){

      if (type == "n") { # can extract data from module
        estimated_model <- modules[i, "model"][[1]][[1]]
        data <- estimated_model$aux$y
        maxtime_model <- max(estimated_model$aux$y.index)
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1

        arima_model <- forecast::ets(data, model = "ZZZ")
        fc <- forecast::forecast(arima_model, h = n.ahead_model)

        transformations_model <- transformations %>% dplyr::filter(.data$dependent == depvar) %>% dplyr::pull("log_opts") %>% purrr::pluck(1) %>% dplyr::pull("depvar")
        stopifnot(transformations_model %in% c("log", "asinh", "level"))
        if (transformations_model == "log") {
          fc_level <- exp(as.numeric(fc$mean))
        } else if (transformations_model == "asinh") {
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          fc_level <- fc
        }
      } else if (type == "d") { # do model object to extract data from
        mintime_model <- fulldata %>% dplyr::filter(.data$na_item == paste0(depvar, ".hat")) %>% tidyr::drop_na() %>% dplyr::pull("time") %>% min() # first model value
        data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% dplyr::filter(.data$time >= mintime_model)
        maxtime_model <- data %>% tidyr::drop_na() %>% dplyr::pull("time") %>% max()
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        if (any(data$values <= 0, na.rm = TRUE)) {
          data <- data %>% dplyr::mutate(values_trans = asinh(.data$values))
          arima_model <- forecast::ets(data$values_trans, model = "ZZZ")
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          data <- data %>% dplyr::mutate(values_trans = log(.data$values))
          arima_model <- forecast::ets(data$values_trans, model = "ZZZ")
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- exp(as.numeric(fc$mean))
        }
      } else {
        stop("type not recognized")
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else {
      stop("unknown model type")
    }

  } # end loop across modules

  out$forecast_type <- forecast_type
  rownames(out) <- NULL
  return(out)

}



#' Creates baseline forecasts for comparison with OSEM
#' @inheritParams forecast_comparison
#'
#' @return Returns a data frame with the point forecasts.
#'
#' @details
#' The function first determines the maximum forecast horizon by adding the n.ahead argument to the most recent data
#' observation across all modules. For variables whose forecast origin is before that, it creates additional forecasts
#' up to the forecast origin. Hence, the actual number of forecasted values may differ across variables.
#'
#' When the forecast type is "AR", the function first transforms the variable into logs (if only positive values
#' observed), otherwise using the asinh transformation. Reported forecast values are after conversion back to the level
#' of the variable.
#'
#' In contrast to forecast_comparison(), we use the maximum available data for the univariate forecasts rather than ensuring that the same subsample is used on OSEM and the univariate models.


forecast_comparison2 <- function(model, n.ahead, forecast_type = c("AR", "RW"), lags = NULL, mc = TRUE) {

  # extract model info
  lags <- if (is.null(lags)) {model$args$max.ar} else {lags}
  modules <- model$module_collection %>% dplyr::arrange(.data$order)
  transformations <- model$opts_df %>% dplyr::select("dependent", "log_opts")
  fulldata <- model$full_data
  maxtime <- fulldata %>% dplyr::filter(!grepl("\\.hat$", .data$na_item)) %>% tidyr::drop_na() %>% dplyr::pull("time") %>% max()
  maxhorizon <- seq.Date(maxtime, by = "quarter", length.out = (n.ahead + 1))[n.ahead + 1]

  # check whether forecast origin same for all
  fulldata %>%
    dplyr::filter(!grepl("\\.hat$", .data$na_item)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::summarise(maxtime = max(.data$time)) %>%
    dplyr::pull("maxtime") %>%
    unique() -> maxtimes
  stopifnot(length(maxtimes) == 1L)

  # set up forecast output
  out <- data.frame()

  for (i in 1:NROW(modules)) {

    type <- modules[i, "type"][[1]]
    depvar <- modules[i, "dependent"][[1]]
    # now use all available data
    data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% tidyr::drop_na() %>% dplyr::arrange("time")
    # model identities in levels
    transformations_model <- if (type == "n") {
      transformations %>% dplyr::filter(.data$dependent == depvar) %>% dplyr::pull("log_opts") %>% purrr::pluck(1) %>% dplyr::pull("depvar")
    } else {
      "level"
    }
    stopifnot(transformations_model %in% c("log", "asinh", "level"))
    if (transformations_model == "log") {
      data <- data %>% dplyr::mutate(values_trans = log(.data$values))
    } else if (transformations_model == "asinh") {
      data <- data %>% dplyr::mutate(values_trans = asinh(.data$values))
    } else if (transformations_model == "level") {
      data <- data %>% dplyr::mutate(values_trans = .data$values)
    } else {
      stop("unknown transformation")
    }
    # does variable exist for maxtime in the data?
    stopifnot(max(data$time) == maxtime)
    # extract y variable
    y <- zoo::zoo(x = data$values_trans, order.by = data$time)

    if (forecast_type == "AR") {

      ar_model <- gets::arx(y = y, mc = mc, ar = 1:lags)
      ar_model$call$mc <- mc
      ar_model$call$ar <- 1:lags
      fc <- gets::predict.arx(ar_model, n.ahead = n.ahead)
      if (transformations_model == "log") {
        fc_level <- exp(fc)
      } else if (transformations_model == "asinh") {
        fc_level <- sinh(fc)
      } else {
        fc_level <- fc
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(from = maxtime, to = maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "RW") {

      fc_level <- data %>% tidyr::drop_na() %>% dplyr::arrange("time") %>% dplyr::slice_tail(n = 1) %>% dplyr::pull("values")
      out_model <- data.frame(na_item = depvar, time = seq.Date(from = maxtime, to = maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "auto") {

      arima_model <- forecast::auto.arima(y = as.numeric(y))
      fc <- forecast::forecast(arima_model, h = n.ahead)
      if (transformations_model == "log") {
        fc_level <- exp(as.numeric(fc$mean))
      } else if (transformations_model == "asinh") {
        fc_level <- sinh(as.numeric(fc$mean))
      } else {
        fc_level <- fc$mean
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(from = maxtime, to = maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "ets"){

      ets_model <- forecast::ets(y = as.numeric(y), model = "ZZZ")
      fc <- forecast::forecast(ets_model, h = n.ahead)
      if (transformations_model == "log") {
        fc_level <- exp(as.numeric(fc$mean))
      } else if (transformations_model == "asinh") {
        fc_level <- sinh(as.numeric(fc$mean))
      } else {
        fc_level <- fc$mean
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(from = maxtime, to = maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else {
      stop("unknown model type")
    }

  } # end loop across modules

  out$forecast_type <- forecast_type
  rownames(out) <- NULL
  return(out)

}
