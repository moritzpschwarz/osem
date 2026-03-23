# Print output of an OSEM Insample Forecast

Print output of an OSEM Insample Forecast

## Usage

``` r
# S3 method for class 'osem.forecast.insample'
print(x, plot = TRUE, full_names = FALSE, ...)
```

## Arguments

- x:

  An object of class osem.forecast.insample, which is the output from
  the
  [forecast_insample](http://www.moritzschwarz.org/osem/reference/forecast_insample.md)
  function.

- plot:

  Logical. Default = `TRUE`. Should the osem insample forecast output be
  plotted?

- full_names:

  Logical. Default = `FALSE`. Should the full names of the variables
  from the dictionary be printed?

- ...:

  Further arguments.

## Value

A printed summary of the insample forecast output, including RMSFE and
forecast within uncertainty statistics.
