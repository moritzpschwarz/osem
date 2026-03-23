# Evaluate the sensitivity of forecasts to varying exogenous values

This function evaluates the sensitivity of forecasts to varying
exogenous values by modifying the exogenous values in the forecast
horizon and comparing the resulting forecasts to the original forecasts.
The function allows for both percentage-based and unit-based
modifications of the exogenous values, and can also calculate impulse
response functions if desired.

## Usage

``` r
forecast_sensitivity(
  model,
  size = 0.5,
  size_type = "pct",
  grepl_variables = NULL,
  quiet = FALSE,
  impulse_response = TRUE,
  exog_fill_method = "AR",
  exclude_zero_change = TRUE,
  include_uncertainty = TRUE
)
```

## Arguments

- model:

  A model object of class 'osem'.

- size:

  Numeric. If 'size_type' = 'pct' then 'size' must be larger than -1 as
  it represents the percentage value that the exogenous values will be
  multiplied with. Default is 0.5 (i.e. 50%). If 'size_type' = 'unit',
  then 'size' represents the unit value that will be added to the
  exogenous values (e.g. to run a one-unit IRF, use 'size = 1' and
  size_type = 'unit').

- size_type:

  Character. Either "pct" or "unit". If "pct", the exogenous values will
  be multiplied with (1 + size). If "unit", the exogenous values will be
  increased by size. Default is "pct".

- grepl_variables:

  Regular Expression Character. Can be used to select variables to be
  included in the sensitivity procedure.

- quiet:

  Logical. Should messages about the forecast procedure be suppressed?

- impulse_response:

  Logical. Should an impulse response function be calculated. Main
  reason to deactivate this might be improving the speed of this
  function.

- exog_fill_method:

  Character, either 'AR', 'auto', or 'last'. When no exogenous values
  have been provided, these must be inferred. When option
  'exog_fill_method = "AR"' then an autoregressive model is used to
  further forecast the exogenous values. With 'last', simply the last
  available value is used. 'auto' is an
  [`auto.arima`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
  model.

- exclude_zero_change:

  Logical. Should cases where the change in forecasts is exactly zero be
  excluded from the output (i.e. set to NA). This can be useful if there
  are many variables and many of them have no effect on the forecasts,
  which can make the plot easier to read. This can happen if e.g. the
  modified variable is not part of the specification of the module or
  has been removed due to model selection.

- include_uncertainty:

  Logical. Should the uncertainty of the forecasts be included in the
  plot. If TRUE, the function will calculate the difference in all
  forecast runs (not just the central estimate) between the modified and
  original forecasts and show the 5-95% and 25-75% quantiles of these
  differences in the plot as shaded areas.

## Value

A list that contains a tibble with the original forecasts and the
difference to the modified forecasts as well as plot(s).
