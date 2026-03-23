# Prepare the clean dataframe for modelling

Prepare the clean dataframe for modelling

## Usage

``` r
clean_data(
  raw_data,
  max.ar = 4,
  max.dl = 2,
  trend = TRUE,
  opts_df,
  module,
  use_logs
)
```

## Arguments

- raw_data:

  A tibble or data.frame with the y variable and the x variables. Needs
  to have a column called 'time', which is of class
  [`Date`](https://rdrr.io/r/base/Dates.html). Variable names need to be
  in column 'na_item', and values in column 'values'.

- max.ar:

  Integer. The maximum number of lags to use for the AR terms. as well
  as for the independent variables.

- max.dl:

  Integer. The maximum number of lags to use for the independent
  variables (the distributed lags).

- trend:

  Logical. Should a trend be added? Default is TRUE.

- opts_df:

  Internal object containing detailed options and information on
  individual modules.

- module:

  A row of the specification table.

- use_logs:

  To decide whether to log any variables. Must be one of 'both', 'y',
  'x', or 'none'. Default is 'both'.

## Value

A tibble with the cleaned data.
