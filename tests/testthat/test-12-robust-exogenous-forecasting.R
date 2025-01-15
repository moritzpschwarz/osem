test_that("robust forecasting method integration works", {
  specification <- dplyr::tibble(
    type = c(
      "n"
    ),
    dependent = c(
      "FinConsExpHH"
    ),
    independent = c(
      "FinConsExpGov + HICP_Gas"
    )
  )

  set.seed(1)
  testdata <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                            FinConsExpGov = rnorm(mean = 100, n = length(time)),
                            #HICP_Gas = rnorm(mean = 200, n = length(time)),
                            # simulate an AR1 process with rho = 0.3 and call it HICP_Gas
                            HICP_Gas = as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 30, mean = 200)),
                            L1.HICP_Gas = lag(HICP_Gas),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas -0.2 * L1.HICP_Gas +
                              as.numeric(arima.sim(n = length(time), list(ar = 0.8), sd = 0.2, mean = 0)))
  #rnorm(length(time), mean = 0, sd = 0.2))
  testdata_2 <- testdata %>%
    dplyr::mutate(L2.HICP_Gas = lag(L1.HICP_Gas))

  testdata <- tidyr::pivot_longer(testdata, -time, names_to = "na_item", values_to = "values")

  lag <- 12     # number of lags
  H <- 12      # number of forecast horizons
  W <- 1       #  Local window smoothing - usually set equal to frequency of series
  trend = FALSE  # linear trend?

  model <- run_model(specification = specification,
                     dictionary = dict,
                     inputdata_directory = testdata,
                     primary_source = "local")

  #test that program stops if you try to model a method that is not implmented
  expect_error( forecast_fail <- forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "test",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = W,
    lag = lag,
    trend = trend
  ))

  #test that program stops if window is <1
  expect_error( forecast_fail <- forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "clements_hendry",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = 0.9,
    lag = lag,
    trend = trend
  ))

  #test that program stops if lag < 1
  expect_error( forecast_fail <- forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "clements_hendry",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = 1,
    lag = 0.5,
    trend = trend
  ))

  #test that program stops if lag is a non integer
  expect_error( forecast_fail <- forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "clements_hendry",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = 1,
    lag = 1.5,
    trend = trend
  ))


  #test that program stops if window is a non integer
  expect_error( forecast_fail <- forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "clements_hendry",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = 200.14,
    lag = 1,
    trend = trend
  ))

  #test that trend has to be boolean input
  expect_error( forecast_fail <- forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "clements_hendry",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = 1,
    lag = 12,
    trend = 10
  ))


})

test_that("robust forecasting methods can execute",{

  specification <- dplyr::tibble(
    type = c(
      "n"
    ),
    dependent = c(
      "FinConsExpHH"
    ),
    independent = c(
      "FinConsExpGov + HICP_Gas"
    )
  )
  #set.seed(1)
  testdata <- read.csv(test_path("testdata", "robust_exogenous_forecasting", "exogenous_test_data.csv"))

  #testdata$values <- testdata$values/100

  lag <- 12     # number of lags
  H <- 12      # number of forecast horizons
  W <- 1       #  Local window smoothing - usually set equal to frequency of series
  trend = FALSE  # linear trend?

  model <- run_model(specification = specification,
                     dictionary = dict,
                     inputdata_directory = testdata,
                     primary_source = "local")

  expect_message(model_forecast <-forecast_model(
    model,
    exog_predictions = NULL,
    n.ahead = 10,
    ci.levels = c(0.5, 0.66, 0.95),
    exog_fill_method = "clements_hendry",
    ar.fill.max = 4,
    plot = FALSE,
    uncertainty_sample = 100,
    quiet = FALSE,
    window = 4,
    lag = 1,
    trend = trend
  ))

})
