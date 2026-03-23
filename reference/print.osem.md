# Print output of an OSEM Model

Print output of an OSEM Model

## Usage

``` r
# S3 method for class 'osem'
print(x, plot = TRUE, full_names = FALSE, ...)
```

## Arguments

- x:

  An object of type 'osem'

- plot:

  Logical. Default = `TRUE`. Should the osem model output be plotted?

- full_names:

  Logical. Default = `FALSE`. Should the full names of the variables
  from the dictionary be printed?

- ...:

  Further arguments.

## Value

A printed summary of the OSEM model output, including estimation
options, relationships considered, and diagnostics.
