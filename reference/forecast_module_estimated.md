# Forecast module for estimated relationships

This internal function handles forecasts from estimated models (ISAT and
CVAR).

## Usage

``` r
forecast_module_estimated(
  model,
  i,
  exog_df_ready,
  exog_df_ready_full,
  n.ahead,
  current_spec,
  prediction_list,
  uncertainty_sample,
  nowcasted,
  ci.levels
)
```

## Arguments

- model:

  The full OSEM model object

- i:

  The order of the current module being forecasted

- exog_df_ready:

  The exogenous data frame prepared for forecasting

- exog_df_ready_full:

  The full exogenous data frame prepared for forecasting

- n.ahead:

  Number of steps ahead to forecast

- current_spec:

  The current module specification

- prediction_list:

  The collection of all predictions

- uncertainty_sample:

  The uncertainty sample for forecasts

- nowcasted:

  Nowcasted data for the forecast period

- ci.levels:

  Confidence interval levels for the forecasts

## Value

A tibble containing the updated prediction_list object with forecasts
for the current module
