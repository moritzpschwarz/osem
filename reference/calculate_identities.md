# Calculate values for identity/definition variables not available in Eurostat

Calculates true values for variables that are given by identity or
definition but are not directly available from Eurostat.

## Usage

``` r
calculate_identities(specification, data, dictionary = NULL)
```

## Arguments

- specification:

  A tibble or data.frame as returned by
  [`translate_variables`](http://www.moritzschwarz.org/osem/reference/translate_variables.md).

- data:

  A tibble or data.frame containing the downloaded or locally loaded
  data as returned by
  [`load_or_download_variables`](http://www.moritzschwarz.org/osem/reference/load_or_download_variables.md).

- dictionary:

  A tibble or data.frame storing the model variable names in a column
  named 'model_varname' and the corresponding Eurostat variable code in
  'eurostat_code'. When `NULL`, the [default
  dictionary](http://www.moritzschwarz.org/osem/reference/dict.md) is
  used.

## Value

Returns the original tibble or data.frame `data` with added values for
the identity/definition variables that were not available in Eurostat.
