# Creates baseline forecasts for comparison with OSEM

Creates baseline forecasts for comparison with OSEM

## Usage

``` r
forecast_comparison2(
  model,
  n.ahead,
  forecast_type = c("ar", "RW"),
  lags = NULL,
  mc = TRUE,
  grepl_variables = NULL
)
```

## Arguments

- model:

  A model object of class 'osem'.

- n.ahead:

  Periods to forecast ahead

- forecast_type:

  Type of forecast. Either "AR" for autoregressive or "RW" for random
  walk.

- lags:

  Number of lags to include in the AR model. Ignore for RW model.
  Default chooses the same as the max.ar setting of the model object.

- mc:

  Logical value whether to include in intercept in the AR model or not.

- grepl_variables:

  Optional character vector of variable names to filter the forecast
  comparison to. If NULL (default), forecasts are created for all
  variables in the model.

## Value

Returns a data frame with the point forecasts.

## Details

The function first determines the maximum forecast horizon by adding the
n.ahead argument to the most recent data observation across all modules.
For variables whose forecast origin is before that, it creates
additional forecasts up to the forecast origin. Hence, the actual number
of forecasted values may differ across variables.

When the forecast type is "ar", the function first transforms the
variable into logs (if only positive values observed), otherwise using
the asinh transformation. Reported forecast values are after conversion
back to the level of the variable.

In contrast to forecast_comparison(), we use the maximum available data
for the univariate forecasts rather than ensuring that the same
subsample is used on OSEM and the univariate models.
