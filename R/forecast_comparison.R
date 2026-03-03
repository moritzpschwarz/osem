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

    if (forecast_type == "ar") {

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
        mintime_model <- fulldata %>%
          dplyr::filter(.data$na_item == paste0(depvar, ".hat")) %>%
          tidyr::drop_na() %>%
          dplyr::pull("time") %>%
          min() # first model value

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
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1],
                              values = fc_level)
      out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type == "RW") {

      data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% tidyr::drop_na() %>% dplyr::arrange(.data$time) %>% dplyr::slice_tail(n = 1)
      maxtime_model <- data %>% dplyr::pull("time")
      n.ahead_model <- length(seq.Date(maxtime_model, maxhorizon, by = "quarter")) - 1
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1],
                              values = data$values)
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
        mintime_model <- fulldata %>%
          dplyr::filter(.data$na_item == paste0(depvar, ".hat")) %>%
          tidyr::drop_na() %>%
          dplyr::pull("time") %>%
          min() # first model value

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
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1],
                              values = fc_level)
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
        mintime_model <- fulldata %>%
          dplyr::filter(.data$na_item == paste0(depvar, ".hat")) %>%
          tidyr::drop_na() %>%
          dplyr::pull("time") %>%
          min() # first model value

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
      out_model <- data.frame(na_item = depvar, time = seq.Date(maxtime_model, maxhorizon, by = "quarter")[-1],
                              values = fc_level)
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
#' @param grepl_variables Optional character vector of variable names to filter the forecast comparison to. If NULL (default), forecasts are created for all variables in the model.
#'
#' @return Returns a data frame with the point forecasts.
#'
#' @details
#' The function first determines the maximum forecast horizon by adding the n.ahead argument to the most recent data
#' observation across all modules. For variables whose forecast origin is before that, it creates additional forecasts
#' up to the forecast origin. Hence, the actual number of forecasted values may differ across variables.
#'
#' When the forecast type is "ar", the function first transforms the variable into logs (if only positive values
#' observed), otherwise using the asinh transformation. Reported forecast values are after conversion back to the level
#' of the variable.
#'
#' In contrast to forecast_comparison(), we use the maximum available data for the univariate forecasts rather than ensuring that the same subsample is used on OSEM and the univariate models.


forecast_comparison2 <- function(model, n.ahead, forecast_type = c("ar", "RW"),
                                 lags = NULL, mc = TRUE, grepl_variables = NULL) {

  # extract model info
  lags <- if (is.null(lags)) {model$args$max.ar} else {lags}
  modules <- model$module_collection %>% dplyr::arrange(.data$order)
  transformations <- model$opts_df %>% dplyr::select("dependent", "log_opts")
  fulldata <- model$full_data
  maxtime <- fulldata %>% dplyr::filter(!grepl("\\.hat$", .data$na_item)) %>% tidyr::drop_na() %>% dplyr::pull("time") %>% max()
  maxhorizon <- seq.Date(maxtime, by = check_frequencies(model$processed_input_data)$freq, length.out = (n.ahead + 1))[n.ahead + 1]

  # check whether forecast origin same for all
  fulldata %>%
    dplyr::filter(!grepl("\\.hat$", .data$na_item)) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::summarise(maxtime = max(.data$time)) %>%
    dplyr::pull("maxtime") %>%
    unique() -> maxtimes

  if(length(maxtimes) != 1L){
    return(NULL)
  }
  #stopifnot(length(maxtimes) == 1L)
  #if(length(maxtimes) == 1L){return(NULL)}

  # set up forecast output
  out <- dplyr::tibble()
  #all_forecast_paths <- list()

  for (i in 1:NROW(modules)) {

    type <- modules[i, "type"][[1]]
    depvar <- modules[i, "dependent"][[1]]

    if(!is.null(grepl_variables)){
      if(!grepl(grepl_variables, depvar)){
        next
      }
    }

    # now use all available data
    data <- fulldata %>% dplyr::filter(.data$na_item == depvar) %>% tidyr::drop_na() %>% dplyr::arrange(.data$time)
    # model identities in levels
    transformations_model <- if (type == "n") {
      transformations %>% dplyr::filter(.data$dependent == depvar) %>% dplyr::pull("log_opts") %>% purrr::pluck(1) %>% dplyr::pull(depvar)
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

    if (forecast_type == "ar") {

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
      origin_date <- zoo::index(y[length(y)])

      temp_path <- data.frame(
        Origin_Date = rep(origin_date, n.ahead),
        Horizon = 1:n.ahead,
        na_item = rep(depvar, n.ahead),
        #time = future_time,
        values = fc_level,
        stringsAsFactors = FALSE
      ) %>%
        tidyr::pivot_wider(id_cols = c("Origin_Date", "Horizon"),
                           names_from = "na_item",
                           values_from = "values")

      out <- out %>% dplyr::bind_rows(temp_path)

    } else if (forecast_type == "RW") {

      last_row <- data %>%
        tidyr::drop_na("values") %>%
        dplyr::arrange(.data$time) %>%
        dplyr::slice_tail(n = 1)

      fc_level <- last_row %>% dplyr::pull(.data$values)
      origin_date <- last_row %>% dplyr::pull(.data$time)

      # Future dates: exactly n.ahead quarterly steps after origin_date
      future_time <- seq.Date(from = origin_date, by = "quarter", length.out = n.ahead + 1L)[-1L]

      temp_path <- data.frame(
        Origin_Date = rep(origin_date, n.ahead),
        Horizon = 1:n.ahead,
        na_item = rep(depvar, n.ahead),
        #time = future_time,
        values = rep(fc_level, n.ahead),
        stringsAsFactors = FALSE
      ) %>%
        tidyr::pivot_wider(id_cols = c("Origin_Date", "Horizon"),
                           names_from = "na_item",
                           values_from = "values")

      out <- out %>% dplyr::bind_rows(temp_path)

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
      #out_model <- data.frame(na_item = depvar, time = seq.Date(from = maxtime, to = maxhorizon, by = "quarter")[-1], values = fc_level)
      #out <- dplyr::bind_rows(out, out_model)

      origin_date <- zoo::index(y[length(y)])

      temp_path <- data.frame(
        Origin_Date = rep(origin_date, n.ahead),
        Horizon = 1:n.ahead,
        na_item = rep(depvar, n.ahead),
        #time = future_time,
        values = fc_level,
        stringsAsFactors = FALSE
      ) %>%
        tidyr::pivot_wider(id_cols = c("Origin_Date", "Horizon"),
                           names_from = "na_item",
                           values_from = "values")

      out <- out %>% dplyr::bind_rows(temp_path)


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
      origin_date <- zoo::index(y[length(y)])

      temp_path <- data.frame(
        Origin_Date = rep(origin_date, n.ahead),
        Horizon = 1:n.ahead,
        na_item = rep(depvar, n.ahead),
        #time = future_time,
        values = fc_level,
        stringsAsFactors = FALSE
      ) %>%
        tidyr::pivot_wider(id_cols = c("Origin_Date", "Horizon"),
                           names_from = "na_item",
                           values_from = "values")

      out <- out %>% dplyr::bind_rows(temp_path)

      # out_model <- data.frame(na_item = depvar, time = seq.Date(from = maxtime, to = maxhorizon, by = "quarter")[-1], values = fc_level)
      # out <- dplyr::bind_rows(out, out_model)

    } else if (forecast_type %in% c("VAR", "BVAR")){

      type <- modules[i, "type"][[1]]
      depvar <- modules[i, "dependent"][[1]]
      indep <- modules[i, "indep"][[1]][[1]]

      if(identical(indep, character(0))){next}

      # now use all available data
      data_init <- fulldata %>% dplyr::filter(.data$na_item %in% c(depvar,indep)) %>% tidyr::drop_na() %>% dplyr::arrange(.data$time)
      # model identities in levels
      transformations_model <- if (type == "n") {
        transformations %>%
          dplyr::filter(.data$dependent == depvar) %>%
          dplyr::pull("log_opts") %>%
          purrr::pluck(1) %>%
          dplyr::as_tibble() %>%
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "na_item",
                              values_to = "transformation")
      } else {
        dplyr::tibble(na_item = depvar, transformation = "level")
      }

      stopifnot(all(transformations_model$transformation %in% c("log", "asinh", "level")))

      data <- data_init %>%
        dplyr::full_join(transformations_model, by = "na_item") %>%
        dplyr::mutate(values_trans = .data$values)

      i_log   <- data$transformation == "log"
      i_asinh <- data$transformation == "asinh"
      i_level <- data$transformation == "level" | is.na(data$transformation)

      data$values_trans[i_log]   <- log(data$values[i_log])
      data$values_trans[i_asinh] <- asinh(data$values[i_asinh])

      # does variable exist for maxtime in the data?
      stopifnot(max(data$time) == maxtime)
      # extract y variable
      # y <- zoo::zoo(x = data$values_trans, order.by = data$time)

      check_frequencies(data) -> freqs

      if(length(unique(freqs$frequency)) > 1L){
        stop("variables have different frequencies, cannot estimate VAR")
      }

      if(freqs$frequency == "3 months"){

        df <- data %>%
          dplyr::arrange(.data$time) %>%
          dplyr::select(-c("transformation")) %>%
          tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values_trans") %>%
          #dplyr::select(-"time") %>%
          tidyr::drop_na()

        time <- df %>%
          dplyr::pull("time")

        df <- df %>% dplyr::select(-"time")

        start_year <- as.integer(format(min(time), "%Y"))
        start_qtr  <- ((as.integer(format(min(time), "%m")) - 1) %/% 3) + 1

        if(ncol(df) < 2L){
          next
        }

        x_ts <- stats::ts(df, start = c(start_year, start_qtr), frequency = 4)
      } else if (freqs$frequency == "year"){
        df <- data %>%
          dplyr::arrange(.data$time) %>%
          dplyr::select(-c("transformation")) %>%
          tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values_trans") %>%
          #dplyr::select(-"time") %>%
          tidyr::drop_na()

        if(ncol(df) < 2L){
          next
        }

        time <- df %>%
          dplyr::pull("time")

        df <- df %>% dplyr::select(-"time")

        start_year <- as.integer(format(min(time), "%Y"))

        x_ts <- stats::ts(df, start = start_year, frequency = 1)
      } else {
        stop("frequency not recognized")
      }

      if(forecast_type == "VAR"){

        max_feasible_lag_vars <- function(x, type = "const") {
          T <- nrow(x); K <- ncol(x)
          det_terms <- switch(type,
                              "const" = 1,
                              "trend" = 2,     # const + trend
                              "both"  = 2,     # const + trend (vars treats "both" similarly)
                              "none"  = 0,
                              stop("Unsupported type")
          )
          # strict: need df > 0: (T - p) - (Kp + det_terms) > 0
          floor((T - det_terms - 1) / (K + 1))
        }
        var_lag <- suppressWarnings(vars::VARselect(x_ts, lag.max = max_feasible_lag_vars(x_ts), type = "const")$selection["AIC(n)"])

        var_model <- vars::VAR(x_ts, p = var_lag, type = "const") # assuming fixed lags
        #var_model <- VAR(train_data, p = VARselect(train_data, lag.max = 8, type = "const")$selection["AIC(n)"], type = "const") # dynamically selecting lags up to 8

        var_forecast <- stats::predict(var_model, n.ahead = n.ahead)

        # Get the date this forecast is being made FROM (the last observation in the training set)
        #origin_date <- var_data$time[t]
        origin_date <- maxtime

        # Initialize a temporary dataframe for this specific window's path
        temp_path <- data.frame(
          Origin_Date = rep(origin_date, n.ahead),
          Horizon = 1:n.ahead,
          dep_var = rep(depvar, n.ahead) # Assuming we want to store the dependent variable's forecasts
        )
        # Extract the full 1 to h point forecasts for all variables
        for (var_name in colnames(x_ts)) {
          temp_path[[var_name]] <- var_forecast$fcst[[var_name]][, "fcst"]
        }
        # Add this window's path to our list
        #all_forecast_paths[[length(all_forecast_paths) + 1]] <- temp_path

        # use transformations_model to convert back to levels
        temp_path_unconv <- temp_path %>%
          tidyr::pivot_longer(cols = -c("Origin_Date", "Horizon", "dep_var"), names_to = "na_item", values_to = "values") %>%
          dplyr::left_join(transformations_model, by = "na_item")

        i_log   <- temp_path_unconv$transformation == "log"
        i_asinh <- temp_path_unconv$transformation == "asinh"
        i_level <- temp_path_unconv$transformation == "level" | is.na(temp_path_unconv$transformation)

        temp_path_unconv$values[i_log]   <- exp(temp_path_unconv$values[i_log])
        temp_path_unconv$values[i_asinh] <- sinh(temp_path_unconv$values[i_asinh])

        temp_path <- temp_path_unconv %>%
          tidyr::pivot_wider(id_cols = c("Origin_Date", "Horizon","dep_var"),
                             names_from = "na_item",
                             values_from = "values") %>%
          dplyr::select("Origin_Date", "Horizon","dep_var",dplyr::all_of(depvar))

        out <- out %>% dplyr::bind_rows(temp_path)


      } else if (forecast_type == "BVAR"){

        # Apply BVAR
        # Expanding Window Loop for BVAR
        # Note: MCMC sampling takes more computational time,
        # so this will run slower than a standard VAR
        #print(paste0("Estimating BVAR for variable ", depvar, " with ", length(colnames(x_ts)), " variables and ", lags, " lags."))
        # 1. Calculate the standard deviation of the differences for each column (Getting around default priors issue by mannually calculating psi)
        manual_psi <- apply(x_ts, 2, function(x) stats::sd(diff(x), na.rm = TRUE))

        # 2. Failsafe: Ensure no variance is exactly zero (which would also crash the prior)
        manual_psi[manual_psi < 1e-6] <- 1e-6

        # 3. Build a custom prior using our manual psi
        my_priors <- BVAR::bv_priors(
          mn = BVAR::bv_minnesota(psi = BVAR::bv_psi(mode = manual_psi))
        )

        # a. Estimate the BVAR model
        # We use the default Minnesota Prior here. 'verbose = FALSE' keeps the console clean.
        bvar_model <- BVAR::bvar(x_ts, lags = lags, priors=my_priors, verbose = FALSE)
        #bvar_model <- bvar(train_data, lags = var_lag, verbose = FALSE)

        # b. Forecast h steps ahead
        # This generates draws from the posterior predictive distribution
        bvar_forecast <- stats::predict(bvar_model, horizon = n.ahead)

        # Get the date this forecast is being made FROM
        origin_date <- maxtime

        # Initialize a temporary dataframe for this specific window's path
        temp_path <- data.frame(
          Origin_Date = rep(origin_date, n.ahead),
          Horizon = 1:n.ahead,
          dep_var = rep(depvar, n.ahead) # Assuming we want to store the dependent variable's forecasts
        )

        # Extract the full 1 to h point forecasts for all variables
        for (i in 1:ncol(x_ts)) {
          var_name <- colnames(x_ts)[i]
          draws_matrix <- matrix(bvar_forecast$fcast[, , i], ncol = n.ahead)
          temp_path[[var_name]] <- apply(draws_matrix, MARGIN = 2, FUN = stats::median)
        }

        # use transformations_model to convert back to levels
        temp_path_unconv <- temp_path %>%
          tidyr::pivot_longer(cols = -c("Origin_Date", "Horizon", "dep_var"), names_to = "na_item", values_to = "values") %>%
          dplyr::left_join(transformations_model, by = "na_item")

        i_log   <- temp_path_unconv$transformation == "log"
        i_asinh <- temp_path_unconv$transformation == "asinh"
        i_level <- temp_path_unconv$transformation == "level" | is.na(temp_path_unconv$transformation)

        temp_path_unconv$values[i_log]   <- exp(temp_path_unconv$values[i_log])
        temp_path_unconv$values[i_asinh] <- sinh(temp_path_unconv$values[i_asinh])

        temp_path <- temp_path_unconv %>%
          tidyr::pivot_wider(id_cols = c("Origin_Date", "Horizon","dep_var"),
                             names_from = "na_item",
                             values_from = "values") %>%
          dplyr::select("Origin_Date", "Horizon","dep_var",dplyr::all_of(depvar))


        out <- out %>% dplyr::bind_rows(temp_path %>% dplyr::mutate(forecast_type = "BVAR"))
      }

    } else {
      stop("unknown model type")
    }



  } # end loop across modules

  out$forecast_type <- forecast_type
  rownames(out) <- NULL
  return(out)

}
