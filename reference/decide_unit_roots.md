# Automated decision based on unit root tests.

Automated decision based on unit root tests.

## Usage

``` r
decide_unit_roots(urtest, alpha = c("1pct", "5pct", "10pct"))
```

## Arguments

- urtest:

  A named list as returned from
  [`test_unit_roots`](http://www.moritzschwarz.org/osem/reference/test_unit_roots.md).

- alpha:

  Significance level for unit root tests.

## Value

A named list as the input but with additional element `$decision`.
