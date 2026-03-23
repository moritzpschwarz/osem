# Internal function to forecast exogenous variables

Internal function to forecast exogenous variables

## Usage

``` r
forecast_exogenous_values(
  model,
  exog_vars,
  exog_predictions,
  exog_fill_method,
  ar.fill.max,
  n.ahead,
  quiet
)
```

## Arguments

- model:

  A model object of class 'osem'.

- exog_vars:

  The set of exogenous variables to be forecasted.

- exog_predictions:

  A data.frame or tibble with values for the exogenous values. The
  number of rows of this data must be equal to n.ahead.

- exog_fill_method:

  Character, either 'AR', 'auto', or 'last'. When no exogenous values
  have been provided, these must be inferred. When option
  'exog_fill_method = "AR"' then an autoregressive model is used to
  further forecast the exogenous values. With 'last', simply the last
  available value is used. 'auto' is an
  [`auto.arima`](https://pkg.robjhyndman.com/forecast/reference/auto.arima.html)
  model.

- ar.fill.max:

  Integer. When no exogenous values have been provided, these must be
  inferred. If option 'exog_fill_method = "AR"' then an autoregressive
  model is used to further forecast the exogenous values. This options
  determines the number of AR terms that should be used. Default is 4.

- n.ahead:

  Periods to forecast ahead

- quiet:

  Logical. Should messages about the forecast procedure be suppressed?

## Value

A dataset containing the set of forecasted exogenous values.
