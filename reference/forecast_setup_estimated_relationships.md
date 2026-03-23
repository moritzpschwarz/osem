# Internal function to set-up the forecasting of estimated relationships

Internal function to set-up the forecasting of estimated relationships

## Usage

``` r
forecast_setup_estimated_relationships(
  model,
  i,
  exog_df_ready,
  n.ahead,
  current_spec,
  prediction_list,
  uncertainty_sample,
  nowcasted_data = NULL,
  full_exog_predicted_data = NULL
)
```

## Arguments

- model:

  A model object of class 'osem'.

- i:

  Current module that is being cycled through

- exog_df_ready:

  Outcome of forecast_exogenous_values() which is the set of forecasted
  exogenous values

- n.ahead:

  Periods to forecast ahead

- current_spec:

  The current specification for the module being forecasted

- prediction_list:

  The full list of all predictions. The results of the function will be
  saved in this list.

- uncertainty_sample:

  Integer. Number of draws to be made for the error bars. Default is
  100.

- nowcasted_data:

  The full_data element of the model object resulting from the
  nowcasting() function. Used to substitute missing historical data.

- full_exog_predicted_data:

  An argument to pass a larger data.frame to the function that can
  contain the entire exogenously predicted data. This is an argument
  that is needed in the nowcasting() function.

## Value

A list containing, among other elements, the data required to carry out
the forecast for this estimated module.
