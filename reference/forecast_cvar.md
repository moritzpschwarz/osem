# Forecast CVAR Model (internal)

This internal function generates forecasts from a CVAR (Cointegrated
Vector Autoregression) model within the OSEM framework.

## Usage

``` r
forecast_cvar(
  model,
  i,
  exog_df_ready,
  full_exog_predicted_data,
  n.ahead,
  current_spec,
  prediction_list,
  uncertainty_sample,
  nowcasted_data
)
```

## Arguments

- model:

  The overall 'osem' model as returned by
  [`run_model`](http://www.moritzschwarz.org/osem/reference/run_model.md)

- i:

  Index of the CVAR model within the module collection

- exog_df_ready:

  Data frame of exogenous variables prepared for forecasting

- full_exog_predicted_data:

  Full data frame of predicted exogenous variables

- n.ahead:

  Number of steps ahead to forecast

- current_spec:

  Current module specification

- prediction_list:

  List of predictions from previous modules

- uncertainty_sample:

  Number of uncertainty samples to generate

- nowcasted_data:

  Nowcasted data for the current module

## Value

A list with class 'osem.forecast.module' containing the central and all
estimates forecasts.
