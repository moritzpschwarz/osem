# Provide unit root diagnostics for the data.

Provide unit root diagnostics for the data.

## Usage

``` r
diagnostics_unit_root(model)
```

## Arguments

- model:

  An [`osem`](http://www.moritzschwarz.org/osem/reference/new_osem.md)
  object.

## Value

A tibble storing the unit root test results for each variable that
appears in a modelled equation ("d"). Since log transoformations may
differ by whether the variable is a dependent or independent variable,
separate tests are conducted. THe tibble also records in which modules
the variables feature.
