# Nowcast missing data for forecasting

The `nowcasting()` function checks across all variables in the model
whether there are any missing values that need to be nowcasted. If there
are, it will nowcast these values and add them to the model. This is
done by using the forecasted values of the exogenous variables. The
function will return a list with two full model objects. One contains
the original model and one contains the nowcasted model.

## Usage

``` r
nowcasting(model, exog_df_ready, frequency)
```

## Arguments

- model:

  A model object of class 'osem'.

- exog_df_ready:

  The outcome of the
  [forecast_exogenous_values](http://www.moritzschwarz.org/osem/reference/forecast_exogenous_values.md)
  function, as prepared by the
  [forecast_model](http://www.moritzschwarz.org/osem/reference/forecast_model.md)
  function..

- frequency:

  Character string that indicates the frequency of the model. Must be
  compatible with the 'by' argument in
  [`seq.Date()`](https://rdrr.io/r/base/seq.Date.html).

## Value

Returns a list with two full model objects. One contains the original
model and one contains the nowcasted model.

## Details

Concretely, the function checks in model\$processed_input_data for the
maximum time point. It then identifies which variables do not fully
extend to this time point. For these variables, it will nowcast the
values using the forecasted values of the exogenous variables. So
fundamentally these nowcasts depend on the exogenous forecasts.
