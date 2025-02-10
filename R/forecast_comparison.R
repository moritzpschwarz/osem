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
  modules <- model$module_collection %>% arrange(order)
  transformations <- model$opts_df %>% select(dependent, log_opts)
  fulldata <- model$full_data
  maxtime <- fulldata %>% filter(!grepl("\\.hat$", na_item)) %>% drop_na() %>% pull(time) %>% max()
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
        fc <- predict(ar_model, n.ahead = n.ahead_model)
        transformations_model <- transformations %>% filter(dependent == depvar) %>% pull(log_opts) %>% pluck(1) %>% pull(depvar)
        stopifnot(transformations_model %in% c("log", "asinh", "level"))
        if (transformations_model == "log") {
          fc_level <- exp(fc)
        } else if (transformations_model == "asinh") {
          fc_level <- sinh(fc)
        } else {
          fc_level <- fc
        }
      } else if (type == "d") { # do model object to extract data from
        mintime_model <- fulldata %>% filter(na_item == paste0(depvar, ".hat")) %>% drop_na() %>% pull(time) %>% min() # first model value
        data <- fulldata %>% filter(na_item == depvar) %>% filter(time >= mintime_model)
        maxtime_model <- data %>% drop_na() %>%  pull(time) %>% max()
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        if (any(data$values <= 0, na.rm = TRUE)) {
          data <- data %>% mutate(values_trans = asinh(values))
          ar_model <- gets::arx(y = data$values_trans, mc = mc, ar = 1:lags)
          ar_model$call$mc <- mc
          ar_model$call$ar <- 1:lags
          fc <- predict(ar_model, n.ahead = n.ahead_model)
          fc_level <- sinh(fc)
        } else {
          data <- data %>% mutate(values_trans = log(values))
          ar_model <- gets::arx(y = data$values_trans, mc = mc, ar = 1:lags)
          ar_model$call$mc <- mc
          ar_model$call$ar <- 1:lags
          fc <- predict(ar_model, n.ahead = n.ahead_model)
          fc_level <- exp(fc)
        }
      } else {
        stop("type not recognized")
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- bind_rows(out, out_model)

    } else if (forecast_type == "RW") {

      data <- fulldata %>% filter(na_item == depvar) %>% drop_na() %>% arrange(time) %>% slice_tail(n = 1)
      maxtime_model <- data %>% pull(time)
      n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = data$values)
      out <- bind_rows(out, out_model)

    } else if (forecast_type == "auto"){

      if (type == "n") { # can extract data from module
        estimated_model <- modules[i, "model"][[1]][[1]]
        data <- estimated_model$aux$y
        maxtime_model <- max(estimated_model$aux$y.index)
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1

        arima_model <- forecast::auto.arima(data)
        fc <- forecast::forecast(arima_model, h = n.ahead_model)

        transformations_model <- transformations %>% filter(dependent == depvar) %>% pull(log_opts) %>% pluck(1) %>% pull(depvar)
        stopifnot(transformations_model %in% c("log", "asinh", "level"))
        if (transformations_model == "log") {
          fc_level <- exp(as.numeric(fc$mean))
        } else if (transformations_model == "asinh") {
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          fc_level <- fc
        }
      } else if (type == "d") { # do model object to extract data from
        mintime_model <- fulldata %>% filter(na_item == paste0(depvar, ".hat")) %>% drop_na() %>% pull(time) %>% min() # first model value
        data <- fulldata %>% filter(na_item == depvar) %>% filter(time >= mintime_model)
        maxtime_model <- data %>% drop_na() %>%  pull(time) %>% max()
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        if (any(data$values <= 0, na.rm = TRUE)) {
          data <- data %>% mutate(values_trans = asinh(values))
          arima_model <- forecast::auto.arima(data$values_trans)
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          data <- data %>% mutate(values_trans = log(values))
          arima_model <- forecast::auto.arima(data$values_trans)
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- exp(as.numeric(fc$mean))
        }
      } else {
        stop("type not recognized")
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- bind_rows(out, out_model)

    } else if (forecast_type == "ets"){

      if (type == "n") { # can extract data from module
        estimated_model <- modules[i, "model"][[1]][[1]]
        data <- estimated_model$aux$y
        maxtime_model <- max(estimated_model$aux$y.index)
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1

        arima_model <- forecast::ets(data, model = "ZZZ")
        fc <- forecast::forecast(arima_model, h = n.ahead_model)

        transformations_model <- transformations %>% filter(dependent == depvar) %>% pull(log_opts) %>% pluck(1) %>% pull(depvar)
        stopifnot(transformations_model %in% c("log", "asinh", "level"))
        if (transformations_model == "log") {
          fc_level <- exp(as.numeric(fc$mean))
        } else if (transformations_model == "asinh") {
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          fc_level <- fc
        }
      } else if (type == "d") { # do model object to extract data from
        mintime_model <- fulldata %>% filter(na_item == paste0(depvar, ".hat")) %>% drop_na() %>% pull(time) %>% min() # first model value
        data <- fulldata %>% filter(na_item == depvar) %>% filter(time >= mintime_model)
        maxtime_model <- data %>% drop_na() %>%  pull(time) %>% max()
        n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
        if (any(data$values <= 0, na.rm = TRUE)) {
          data <- data %>% mutate(values_trans = asinh(values))
          arima_model <- forecast::ets(data$values_trans, model = "ZZZ")
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- sinh(as.numeric(fc$mean))
        } else {
          data <- data %>% mutate(values_trans = log(values))
          arima_model <- forecast::ets(data$values_trans, model = "ZZZ")
          fc <- forecast::forecast(arima_model, h = n.ahead_model)
          fc_level <- exp(as.numeric(fc$mean))
        }
      } else {
        stop("type not recognized")
      }
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1], values = fc_level)
      out <- bind_rows(out, out_model)

    } else {
      stop("unknown model type")
    }

  } # end loop across modules

  out$forecast_type <- forecast_type
  rownames(out) <- NULL
  return(out)

}
