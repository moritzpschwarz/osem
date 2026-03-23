# Diebold-Mariano test for osem forecast comparison

This function performs the Diebold-Mariano test to compare the forecast
accuracy of the OSEM model against specified comparison methods (e.g.,
VAR, BVAR) for insample forecasts. The function allows for flexible
specification of insample forecasting methods and handles the
preparation of forecast errors for the DM test, including adjustments
for overlapping forecasts. The results include DM statistics and
p-values for each variable and forecast horizon.

## Usage

``` r
dm_test(
  model,
  insample_model = NULL,
  insample_methods = c("ets", "auto"),
  insample_main_comparison = "ets",
  insample_sample_share = 0.5,
  comparison_methods = c("VAR", "BVAR"),
  parallel.cores = NULL,
  dm.horizons = 8,
  dm.variance = "bartlett",
  dm.alternative = "two.sided",
  dm.power = 2,
  lags = 4,
  grepl_variables = NULL,
  comparison_data = NULL,
  quiet = FALSE
)
```

## Arguments

- model:

  A model of class 'osem' (to be returned by
  [`run_model`](http://www.moritzschwarz.org/osem/reference/run_model.md)).

- insample_model:

  A model of class 'osem.forecast.insample' (to be returned by
  [`forecast_insample`](http://www.moritzschwarz.org/osem/reference/forecast_insample.md))
  to be used for the insample forecast comparisons. If provided, other
  options for insample modelling (sample share, methods, etc.) are
  ignored.

- insample_methods:

  A character vector of methods to use for the insample forecast
  comparisons. Default is c("ets", "auto"). Note that the method
  specified in insample_main_comparison must be included in this vector.

- insample_main_comparison:

  A character string specifying the method to be used as the main
  comparison for the insample forecasts. Default is "ets". Must be one
  of the methods specified in insample_methods.

- insample_sample_share:

  A numeric value between 0 and 1 specifying the share of the sample to
  be used for insample forecasting. Default is 0.5.

- comparison_methods:

  A character vector specifying the methods to compare against the
  insample main comparison method. Default is c("VAR", "BVAR"). Must be
  a subset of c("VAR", "BVAR", "RW", "ar", "ets", "auto") which
  correspond to Vector Autoregressions (VAR), Bayesian VAR (BVAR),
  Random Walks (RW) and Autoregressive models.

- parallel.cores:

  An integer specifying the number of cores to use for parallel
  processing when running insample forecasts. If NULL, the function will
  not use parallel processing. Default is NULL.

- dm.horizons:

  An integer specifying the forecast horizons to be used for the
  Diebold-Mariano test. Must be a single positive integer. Default is 8.

- dm.variance:

  A character string specifying the variance estimator to be used in the
  Diebold-Mariano test. Default is "bartlett". Must be one of the
  variance estimators supported by forecast::dm.test().

- dm.alternative:

  A character string specifying the alternative hypothesis for the
  Diebold-Mariano test. Default is "two.sided". Must be one of
  "two.sided", "less", or "greater".

- dm.power:

  A numeric value specifying the power to which forecast errors are
  raised in the Diebold-Mariano test. Default is 2 (squared errors).
  Must be a single positive number.

- lags:

  An integer specifying the number of lags to be used in the comparison
  methods. Default is 4.

- grepl_variables:

  A character vector of variable names to be included in the forecast
  comparisons. If NULL, all variables will be included. Default is NULL.

- comparison_data:

  A data frame containing pre-prepared forecast errors and related
  information for the DM test. If provided, the function will use this
  data instead of running the insample forecasts and preparing the data
  internally. Default is NULL.

- quiet:

  A logical value indicating whether to suppress messages during the
  execution of the function. Default is FALSE.

## Value

A list containing the results of the Diebold-Mariano test for forecast
comparisons, including a tibble with DM statistics and p-values for each
variable and forecast horizon, as well as the prepared data used for the
comparisons. If the insample_model was not provided, the list will also
include the insample forecast model used for the comparisons.
