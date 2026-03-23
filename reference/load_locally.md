# Load Locally

Loads data from a local directory

## Usage

``` r
load_locally(to_obtain, input, quiet)
```

## Arguments

- to_obtain:

  A data.frame as returned by
  [`determine_variables()`](http://www.moritzschwarz.org/osem/reference/determine_variables.md).

- input:

  A character vector of file paths, a data.frame, or a list of
  data.frames or file paths. Required if `download == FALSE` and at
  least one variable is specified as coming from a local source.

- quiet:

  Logical with default = `FALSE`. Should messages be displayed? These
  messages are intended to give more information about the estimation
  and data retrieval process.

## Value

Returns a list with two named elements: `$df` stores the downloaded data
and `$to_obtain` the updated data.frame tracking which variables still
need to be obtained.
