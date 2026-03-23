# Translate model variable names to Eurostat codes

Takes a specification and translates the model variable names into
Eurostat codes. Also adds the Eurostat dataset identifier from which the
variable is extracted.

## Usage

``` r
translate_variables(specification, dictionary = NULL)
```

## Arguments

- specification:

  A tibble or data.frame with one column named 'dependent' that contains
  the LHS (Y) variables and one column named 'independent' that contains
  the RHS (x) variables, separated by + and - . Column 'type' should
  specify whether the dependent variable is endogenous by modelling
  (`"n"`) or endogenous by accounting definition (`"d"`).

- dictionary:

  A tibble or data.frame storing the model variable names in a column
  named 'model_varname' and the corresponding Eurostat variable code in
  'eurostat_code'. When `NULL`, the [default
  dictionary](http://www.moritzschwarz.org/osem/reference/dict.md) is
  used.

## Value

Returns a tibble or data.frame as the input `specification` with two
added columns for the model variable name (`model_varname`) and the
dataset identifier (`dataset_id`).
