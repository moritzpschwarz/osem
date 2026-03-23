# Plot OSEM Insample Forecast

Plot an Insample-Forecast of the OSEM Model

## Usage

``` r
# S3 method for class 'osem.forecast.insample'
plot(
  x,
  title = "OSEM Insample Hindcasts",
  first_date = NULL,
  first_date_insample_model = NULL,
  grepl_variables = NULL,
  grepl_method = NULL,
  ...
)
```

## Arguments

- x:

  An object of class osem.forecast.insample, which is the output from
  the
  [forecast_insample](http://www.moritzschwarz.org/osem/reference/forecast_insample.md)
  function.

- title:

  Character. Title of the plot. Default is "OSEM Model Forecast".

- first_date:

  Character. First date value to be shown. Must be a character value
  that can be turned into a date using as.Date() or NULL.

- first_date_insample_model:

  A character string that will be converted to a Date that indicates the
  first insample forecast to show in the plot. By default (`NULL`) all
  insample forecasts will be shown.

- grepl_variables:

  Regular Expression Character. Can be used to select variables to be
  plotted. Experimental feature so use with care.

- grepl_method:

  A character string that will be used to filter the forecasting method.

- ...:

  Additional arguments passed to the plotting function.
