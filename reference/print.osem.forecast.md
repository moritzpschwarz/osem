# Print output of an OSEM Forecast

Print output of an OSEM Forecast

## Usage

``` r
# S3 method for class 'osem.forecast'
print(x, plot = TRUE, full_names = FALSE, ...)
```

## Arguments

- x:

  An object of type 'osem.forecast'

- plot:

  Logical. Default = `TRUE`. Should the osem forecast output be plotted?

- full_names:

  Logical. Default = `FALSE`. Should the full names of the variables
  from the dictionary be printed?

- ...:

  Further arguments.

## Value

A printed summary of the OSEM forecast output, including the forecast
horizon, method, and central estimates.
