# Performs computations for the identity/definition modules

Calculates the fitted values for identity/definition modules using
previously fitted values of endogenous variables.

## Usage

``` r
identity_module(module, data, classification)
```

## Arguments

- module:

  A row of the specification table.

- data:

  A dataset containing at least the required dependent variable and
  independent variables for the module.

- classification:

  A data.frame with two columns: 'var' stores the Eurostat variable code
  and 'class' the classification code (one of `"n"`, `"d"`, or `"x"`)
  reflecting whether the variable is endogenous by modelling, endogenous
  by identity/definition, or exogenous. As returned by
  [classify_variables](http://www.moritzschwarz.org/osem/reference/classify_variables.md).

## Value

Returns the original tibble or data.frame `data` in wide format and
appended with the fitted values of the module.
