# End-to-end forecast and nowcast tests --------------------------------------


# Small data helpers ---------------------------------------------------------

make_forecast_dictionary <- function(variables) {
  dplyr::tibble(
    model_varname = variables,
    full_name = variables,
    database = "local",
    geo = "DE",
    dataset_id = NA_character_,
    freq = ""
  )
}


make_forecast_input <- function(data) {
  data %>%
    tidyr::pivot_longer(
      -"time",
      names_to = "na_item",
      values_to = "values"
    )
}


make_forecast_exog <- function(data,
                               regressors,
                               n.ahead = 3,
                               nowcast_periods = 0) {
  if (nowcast_periods > 0) {
    nowcast_exog <- data %>%
      utils::tail(nowcast_periods) %>%
      dplyr::select("time", dplyr::all_of(regressors))
  } else {
    nowcast_exog <- data[0, c("time", regressors), drop = FALSE]
  }

  forecast_times <- seq.Date(
    from = max(data$time),
    by = "quarter",
    length.out = n.ahead + 1L
  )[-1]

  forecast_exog <- dplyr::tibble(time = forecast_times)
  for (variable in regressors) {
    forecast_exog[[variable]] <- rep(
      utils::tail(data[[variable]], 1),
      n.ahead
    )
  }

  dplyr::bind_rows(nowcast_exog, forecast_exog)
}


# Simulated data -------------------------------------------------------------

simulate_ardl_forecast_data <- function(nobs = 100, seed = 123) {
  set.seed(seed)

  lx <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    lx[t] <- 0.65 * lx[t - 1] + stats::rnorm(1, sd = 0.08)
    ly[t] <- 0.55 * ly[t - 1] + 0.25 * lx[t] +
      stats::rnorm(1, sd = 0.06)
  }

  dplyr::tibble(
    time = seq.Date(
      from = as.Date("1990-01-01"),
      by = "quarter",
      length.out = nobs
    ),
    X = exp(4 + lx),
    Y = exp(4 + ly)
  )
}


# X1 and X2 are independent I(1) regressors and z is stationary. Therefore,
# Y, X1, and X2 are linked by a stable cointegrating relation.
simulate_ecm_forecast_data <- function(nobs = 130, seed = 456) {
  set.seed(seed)

  lx1 <- numeric(nobs)
  lx2 <- numeric(nobs)
  z <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    lx1[t] <- lx1[t - 1] + stats::rnorm(1, sd = 0.07)
    lx2[t] <- lx2[t - 1] + stats::rnorm(1, sd = 0.06)
    z[t] <- 0.45 * z[t - 1] + stats::rnorm(1, sd = 0.05)
    ly[t] <- 0.8 * lx1[t] + 0.35 * lx2[t] + z[t]
  }

  dplyr::tibble(
    time = seq.Date(
      from = as.Date("1990-01-01"),
      by = "quarter",
      length.out = nobs
    ),
    X1 = exp(4 + lx1),
    X2 = exp(4 + lx2),
    Y = exp(4 + ly)
  )
}


# X and Y are independent I(1) processes. The automatic ECM decision should
# therefore select the fully differenced equation.
simulate_diff_forecast_data <- function(nobs = 180, seed = 872) {
  set.seed(seed)

  lx <- numeric(nobs)
  ly <- numeric(nobs)

  for (t in 2:nobs) {
    lx[t] <- lx[t - 1] + stats::rnorm(1, sd = 0.08)
    ly[t] <- ly[t - 1] + stats::rnorm(1, sd = 0.08)
  }

  dplyr::tibble(
    time = seq.Date(
      from = as.Date("1990-01-01"),
      by = "quarter",
      length.out = nobs
    ),
    X = exp(4 + lx),
    Y = exp(4 + ly)
  )
}


# Forecasting without nowcasting --------------------------------------------

test_that("ARDL forecasting returns transformed levels", {
  simulated_data <- simulate_ardl_forecast_data()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  model <- run_model(
    specification = specification,
    dictionary = make_forecast_dictionary(c("Y", "X")),
    input = make_forecast_input(simulated_data),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ardl",
    ecm_pretest = "none",
    trend = FALSE,
    max.ar = 1,
    max.dl = 1,
    saturation = character(0),
    gets_selection = FALSE,
    indicator_compression = FALSE,
    constrain.to.minimum.sample = FALSE,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  forecast <- forecast_model(
    model = model,
    exog_predictions = make_forecast_exog(
      data = simulated_data,
      regressors = "X"
    ),
    n.ahead = 3,
    ci.levels = 0.8,
    exog_fill_method = NULL,
    plot = FALSE,
    uncertainty_sample = 4,
    quiet = TRUE
  )

  # forecast looks weird because uncertainty sample is only 4

  recipe <- model$module_collection$model.args[[1]]$forecast_recipe
  forecast_row <- forecast$forecast %>%
    dplyr::filter(.data$dep_var == "Y")
  central <- forecast_row$central.estimate[[1]]
  draws <- forecast_row$all.estimates[[1]]
  equation_prediction <- forecast_row$predict.isat_object[[1]]

  expect_identical(recipe$model_form, "ardl")
  expect_null(forecast$nowcast_data)
  expect_equal(NROW(central), 3)
  expect_equal(NROW(draws), 3)
  expect_equal(NCOL(draws), 5)
  expect_true(all(is.finite(central[[recipe$transformed_level_name]])))
  expect_true(all(is.finite(as.matrix(draws[, -1]))))
  expect_equal(
    central[[recipe$transformed_level_name]],
    equation_prediction$yhat
  )
})


test_that("differenced forecasts are reconstructed into levels", {
  simulated_data <- simulate_diff_forecast_data()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  model <- run_model(
    specification = specification,
    dictionary = make_forecast_dictionary(c("Y", "X")),
    input = make_forecast_input(simulated_data),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ecm",
    ecm_pretest = "auto",
    trend = FALSE,
    max.ar = 1,
    max.dl = 1,
    saturation = character(0),
    gets_selection = FALSE,
    indicator_compression = FALSE,
    constrain.to.minimum.sample = FALSE,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  forecast <- forecast_model(
    model = model,
    exog_predictions = make_forecast_exog(
      data = simulated_data,
      regressors = "X"
    ),
    n.ahead = 3,
    ci.levels = 0.8,
    exog_fill_method = NULL,
    plot = FALSE,
    uncertainty_sample = 4,
    quiet = TRUE
  )

  recipe <- model$module_collection$model.args[[1]]$forecast_recipe
  forecast_row <- forecast$forecast %>%
    dplyr::filter(.data$dep_var == "Y")
  central <- forecast_row$central.estimate[[1]]
  draws <- forecast_row$all.estimates[[1]]
  equation_prediction <- forecast_row$predict.isat_object[[1]]

  initial_level <- osem:::transform_osem_values(
    utils::tail(simulated_data$Y, 1),
    recipe$dependent_transformation
  )
  expected_levels <- initial_level + cumsum(equation_prediction$yhat)

  expect_identical(recipe$model_form, "diff")
  expect_null(forecast$nowcast_data)
  expect_equal(
    central[[recipe$transformed_level_name]],
    expected_levels
  )
  expect_true(all(is.finite(as.matrix(draws[, -1]))))
})


test_that("cointegrated ECM forecasts update levels recursively", {
  simulated_data <- simulate_ecm_forecast_data()

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X1 + X2",
    lag = "",
    cvar = ""
  )

  model <- run_model(
    specification = specification,
    dictionary = make_forecast_dictionary(c("Y", "X1", "X2")),
    input = make_forecast_input(simulated_data),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ecm",
    ecm_pretest = "none",
    trend = FALSE,
    max.ar = 1,
    max.dl = 1,
    saturation = character(0),
    gets_selection = FALSE,
    indicator_compression = FALSE,
    constrain.to.minimum.sample = FALSE,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  forecast <- forecast_model(
    model = model,
    exog_predictions = make_forecast_exog(
      data = simulated_data,
      regressors = c("X1", "X2")
    ),
    n.ahead = 3,
    ci.levels = 0.8,
    exog_fill_method = NULL,
    plot = FALSE,
    uncertainty_sample = 4,
    quiet = TRUE
  )

  recipe <- model$module_collection$model.args[[1]]$forecast_recipe
  forecast_row <- forecast$forecast %>%
    dplyr::filter(.data$dep_var == "Y")
  central <- forecast_row$central.estimate[[1]]
  draws <- forecast_row$all.estimates[[1]]
  equation_prediction <- forecast_row$predict.isat_object[[1]]

  initial_level <- osem:::transform_osem_values(
    utils::tail(simulated_data$Y, 1),
    recipe$dependent_transformation
  )
  forecast_levels <- central[[recipe$transformed_level_name]]

  expect_identical(recipe$model_form, "ecm")
  expect_setequal(recipe$regressors, c("X1", "X2"))
  expect_null(forecast$nowcast_data)
  expect_equal(
    diff(c(initial_level, forecast_levels)),
    equation_prediction$yhat
  )
  expect_true(all(is.finite(forecast_levels)))
  expect_true(all(is.finite(as.matrix(draws[, -1]))))
})


# Forecasting with nowcasting ------------------------------------------------

test_that("ARDL forecasting works after ragged-edge nowcasting", {
  simulated_data <- simulate_ardl_forecast_data()
  estimation_data <- simulated_data
  estimation_data$Y[(NROW(estimation_data) - 1):NROW(estimation_data)] <- NA_real_

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  model <- run_model(
    specification = specification,
    dictionary = make_forecast_dictionary(c("Y", "X")),
    input = make_forecast_input(estimation_data),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ardl",
    ecm_pretest = "none",
    trend = FALSE,
    max.ar = 1,
    max.dl = 1,
    saturation = character(0),
    gets_selection = FALSE,
    indicator_compression = FALSE,
    constrain.to.minimum.sample = FALSE,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  forecast <- forecast_model(
    model = model,
    exog_predictions = make_forecast_exog(
      data = simulated_data,
      regressors = "X",
      nowcast_periods = 2
    ),
    n.ahead = 3,
    ci.levels = 0.8,
    exog_fill_method = NULL,
    plot = FALSE,
    uncertainty_sample = 4,
    quiet = TRUE
  )

  recipe <- model$module_collection$model.args[[1]]$forecast_recipe
  nowcast <- forecast$nowcast_data %>%
    dplyr::filter(.data$na_item == "Y") %>%
    dplyr::arrange(.data$time)
  central <- forecast$forecast$central.estimate[[1]]

  expect_identical(recipe$model_form, "ardl")
  expect_equal(nowcast$time, utils::tail(simulated_data$time, 2))
  expect_equal(NROW(nowcast), 2)
  expect_true(all(is.finite(nowcast$values)))
  expect_true(all(nowcast$values > 0))
  expect_equal(NROW(central), 3)
})


test_that("differenced forecasting works after ragged-edge nowcasting", {
  simulated_data <- simulate_diff_forecast_data()
  estimation_data <- simulated_data
  estimation_data$Y[(NROW(estimation_data) - 1):NROW(estimation_data)] <- NA_real_

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X",
    lag = "",
    cvar = ""
  )

  model <- run_model(
    specification = specification,
    dictionary = make_forecast_dictionary(c("Y", "X")),
    input = make_forecast_input(estimation_data),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ecm",
    ecm_pretest = "auto",
    trend = FALSE,
    max.ar = 1,
    max.dl = 1,
    saturation = character(0),
    gets_selection = FALSE,
    indicator_compression = FALSE,
    constrain.to.minimum.sample = FALSE,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  forecast <- forecast_model(
    model = model,
    exog_predictions = make_forecast_exog(
      data = simulated_data,
      regressors = "X",
      nowcast_periods = 2
    ),
    n.ahead = 3,
    ci.levels = 0.8,
    exog_fill_method = NULL,
    plot = FALSE,
    uncertainty_sample = 4,
    quiet = TRUE
  )

  recipe <- model$module_collection$model.args[[1]]$forecast_recipe
  nowcast <- forecast$nowcast_data %>%
    dplyr::filter(.data$na_item == "Y") %>%
    dplyr::arrange(.data$time)
  central <- forecast$forecast$central.estimate[[1]]
  last_observed <- simulated_data$Y[NROW(simulated_data) - 2]

  expect_identical(recipe$model_form, "diff")
  expect_equal(nowcast$time, utils::tail(simulated_data$time, 2))
  expect_equal(NROW(nowcast), 2)
  expect_true(all(is.finite(nowcast$values)))
  expect_true(all(nowcast$values > 0.2 * last_observed))
  expect_true(all(nowcast$values < 5 * last_observed))
  expect_equal(NROW(central), 3)
})


test_that("cointegrated ECM forecasting works after recursive nowcasting", {
  simulated_data <- simulate_ecm_forecast_data()
  estimation_data <- simulated_data
  estimation_data$Y[(NROW(estimation_data) - 1):NROW(estimation_data)] <- NA_real_

  specification <- dplyr::tibble(
    type = "n",
    dependent = "Y",
    independent = "X1 + X2",
    lag = "",
    cvar = ""
  )

  model <- run_model(
    specification = specification,
    dictionary = make_forecast_dictionary(c("Y", "X1", "X2")),
    input = make_forecast_input(estimation_data),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ecm",
    ecm_pretest = "none",
    trend = FALSE,
    max.ar = 1,
    max.dl = 1,
    saturation = character(0),
    gets_selection = FALSE,
    indicator_compression = FALSE,
    constrain.to.minimum.sample = FALSE,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  forecast <- forecast_model(
    model = model,
    exog_predictions = make_forecast_exog(
      data = simulated_data,
      regressors = c("X1", "X2"),
      nowcast_periods = 2
    ),
    n.ahead = 3,
    ci.levels = 0.8,
    exog_fill_method = NULL,
    plot = FALSE,
    uncertainty_sample = 4,
    quiet = TRUE
  )

  recipe <- model$module_collection$model.args[[1]]$forecast_recipe
  nowcast <- forecast$nowcast_data %>%
    dplyr::filter(.data$na_item == "Y") %>%
    dplyr::arrange(.data$time)
  central <- forecast$forecast$central.estimate[[1]]
  last_observed <- simulated_data$Y[NROW(simulated_data) - 2]

  expect_identical(recipe$model_form, "ecm")
  expect_equal(nowcast$time, utils::tail(simulated_data$time, 2))
  expect_equal(NROW(nowcast), 2)
  expect_true(all(is.finite(nowcast$values)))
  expect_true(all(nowcast$values > 0.2 * last_observed))
  expect_true(all(nowcast$values < 5 * last_observed))
  expect_equal(NROW(central), 3)
})
