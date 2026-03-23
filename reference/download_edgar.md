# Download Edgar

Downloads Edgar data given a data.frame of required variables

## Usage

``` r
download_edgar(to_obtain, quiet)
```

## Arguments

- to_obtain:

  A data.frame as returned by
  [`determine_variables()`](http://www.moritzschwarz.org/osem/reference/determine_variables.md).

- quiet:

  Logical with default = `FALSE`. Should messages be displayed? These
  messages are intended to give more information about the estimation
  and data retrieval process.

## Value

Returns a list with two named elements: `$df` stores the downloaded data
and `$to_obtain` the updated data.frame tracking which variables still
need to be obtained.
