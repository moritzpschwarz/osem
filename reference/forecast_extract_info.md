# Extract relevant information from the osem model object for forecasting and nowcasting

Extract relevant information from the osem model object for forecasting
and nowcasting

## Usage

``` r
forecast_extract_info(model, i, n.ahead, exog_df_ready)
```

## Arguments

- model:

  A model object of class 'osem'.

- i:

  Current module that is being cycled through

- n.ahead:

  Periods to forecast ahead

- exog_df_ready:

  Outcome of forecast_exogenous_values() which is the set of forecasted
  exogenous values

## Value

The relevant information in a list format that enables forecasting and
nowcasting
