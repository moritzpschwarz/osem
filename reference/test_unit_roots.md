# Run three types of ADF unit root tests.

Run three types of ADF unit root tests.

## Usage

``` r
test_unit_roots(x, max.ar, selectlags = c("Fixed", "AIC", "BIC"))
```

## Arguments

- x:

  A numeric vector.

- max.ar:

  Maximum lag for augmented Dickey-Fuller test.

- selectlags:

  Character specifying "Fixed" lags or automatic lag selection with
  information criteria "AIC" or "BIC".

## Value

A named list with four elements, storing the function arguments and the
resulting three ADF unit root tests: (i) no deterministic terms, (ii) an
intercept, (iii) intercept and trend.
