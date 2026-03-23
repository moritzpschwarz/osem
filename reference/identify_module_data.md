# Identifies which variables are required for running the module

Identifies the required variables for running the module and extracts
that subset.

## Usage

``` r
identify_module_data(module, classification, data)
```

## Arguments

- module:

  A row of the specification table.

- classification:

  A data.frame with two columns: 'var' stores the Eurostat variable code
  and 'class' the classification code (one of `"n"`, `"d"`, or `"x"`)
  reflecting whether the variable is endogenous by modelling, endogenous
  by identity/definition, or exogenous. As returned by
  [classify_variables](http://www.moritzschwarz.org/osem/reference/classify_variables.md).

- data:

  A dataset containing at least the required dependent variable and
  independent variables for the module.

## Value

Returns a subset of the original `data` that only contains the relevant
variables for the module.
