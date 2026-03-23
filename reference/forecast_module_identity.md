# An internal function to forecast identities within forecast_model

An internal function to forecast identities within forecast_model

## Usage

``` r
forecast_module_identity(
  model,
  exog_df_ready,
  current_spec,
  prediction_list,
  uncertainty_sample
)
```

## Arguments

- model:

  A model object of class 'osem'.

- exog_df_ready:

  The outcome of
  [forecast_exogenous_values](http://www.moritzschwarz.org/osem/reference/forecast_exogenous_values.md)
  function (potentially modified by the
  [nowcasting](http://www.moritzschwarz.org/osem/reference/nowcasting.md)
  function)

- current_spec:

  Current specification. Is defined within
  [forecast_model](http://www.moritzschwarz.org/osem/reference/forecast_model.md).

- prediction_list:

  Current List containing all predictions. Is defined within
  [forecast_model](http://www.moritzschwarz.org/osem/reference/forecast_model.md).

- uncertainty_sample:

  Integer. Number of draws to be made for the error bars. Default is
  100.

## Value

A list that is then used by forecast_model to set-up the final
prediction object. The list contains the identity estimate.
