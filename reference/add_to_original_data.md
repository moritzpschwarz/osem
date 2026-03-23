# Add the estimated fitted values back to the original

Add the estimated fitted values back to the original

## Usage

``` r
add_to_original_data(
  clean_data,
  model_object,
  dep_var_basename = "imports_of_goods_and_services",
  model_type = "ardl",
  opts_df,
  module
)
```

## Arguments

- clean_data:

  An input data.frame or tibble. Must be the output of clean_data() to
  fit all requirements.

- model_object:

  Most likely should be returned by the 'estimate_module()' function.

- dep_var_basename:

  A character string of the name of the dependent variable as contained
  in clean_data() in a level form (i.e. no ln or D in front of the
  name).

- model_type:

  Either 'ardl', 'ecm', or 'cvar' to determine whether to estimate the
  model as an Autoregressive Distributed Lag Function (ardl), as an
  Equilibrium Correction Model (ecm), or as a cointegrated vector
  autoregression.

- opts_df:

  Internal object containing detailed options and information on
  individual modules.

- module:

  A row of the specification table.

## Value

A tibble with the fitted values as one column.
