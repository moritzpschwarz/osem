# An internal function that checks the frequencies of the model values

This function checks and deals with different types of mixed frequency
models. When there is a mixed frequency within a variable, it tries to
remove

## Usage

``` r
check_frequencies(full_data, quiet)
```

## Arguments

- full_data:

  The full dataset that is used in the model

- quiet:

  Logical with default = FALSE. Should messages be displayed? These
  messages are intended to give more information about the estimation
  and data retrieval process.

## Value

A list with the updated full_data and the frequency of the data
