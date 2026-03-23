# Forecast ISAT Module (internal)

This internal function generates forecasts from an ISAT (Indicator
Saturation) model within the OSEM framework (see
[`isat`](https://rdrr.io/pkg/gets/man/isat.html)).

## Usage

``` r
forecast_isat(
  model,
  i,
  exog_df_ready,
  exog_df_ready_full,
  n.ahead,
  current_spec,
  prediction_list,
  uncertainty_sample,
  ci.levels,
  nowcasted
)
```

## Arguments

- model:

  The overall 'osem' model as returned by
  [`run_model`](http://www.moritzschwarz.org/osem/reference/run_model.md)

- i:

  The index of the current module within the model's module collection

- exog_df_ready:

  The exogenous data frame prepared for forecasting

- exog_df_ready_full:

  The full exogenous data frame prepared for forecasting

- n.ahead:

  Number of steps ahead to forecast

- current_spec:

  The current specification for the module being forecasted

- prediction_list:

  The collection of all predictions

- uncertainty_sample:

  The number of uncertainty samples to draw for the prediction

- ci.levels:

  The confidence interval levels for the prediction

- nowcasted:

  The nowcasted data for the model

## Value

A tibble containing the updated prediction_list object with forecasts
for the current module
