# OSEM Diagnostics

Creates a Summary of the Diagnostics of the OSEM Model

## Usage

``` r
diagnostics_model(model)
```

## Arguments

- model:

  A model of class 'osem' (to be returned by
  [`run_model`](http://www.moritzschwarz.org/osem/reference/run_model.md)).

## Value

Returns a data.frame with the p-values of the AR and ARCH
misspecification tests and the number of impulse and step indicators
retained for each module.
