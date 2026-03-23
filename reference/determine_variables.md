# Determine which model variables need to be found

Takes the specification and returns a dictionary-like data frame with
the required model variables, their filters, and where they can be
found.

## Usage

``` r
determine_variables(specification, dictionary)
```

## Arguments

- specification:

  A tibble or data.frame as returned by
  [`check_config_table`](http://www.moritzschwarz.org/osem/reference/check_config_table.md).

- dictionary:

  A tibble or data.frame storing the Eurostat variable code in column
  'eurostat_code'. If `download == TRUE` then the dictionary also
  requires column named 'dataset_id' that stores the Eurostat dataset
  id. When `NULL`, the [default
  dictionary](http://www.moritzschwarz.org/osem/reference/dict.md) is
  used.

## Value

Returns a data.frame that is a subset of the dictionary (required
variables only) and an additional column `found`, which is set to
`FALSE` initially and keeps track of the data that has been found
through the (down)load functions.
