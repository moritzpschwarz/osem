# Simple constructor for class 'osem'

Simple constructor for class 'osem'

## Usage

``` r
new_osem(x = list())
```

## Arguments

- x:

  A list storing the osem output.

## Value

An object of class `"osem"`, which is a named list with seven elements:

- args:

  A named list storing the user arguments for the OSEM model.

- module_order:

  The original but ordered specification.

- module_collection:

  The above specification with two added columns that store the model
  object for each module and the dataset used for estimation, including
  fitted values for the dependent variable.

- processed_input_data:

  A tibble or data.frame containing the processed input data, including
  variables that are constructed by definition.

- full_data:

  A tibble or data.frame containing the complete original data for the
  OSEM model and the fitted values of each module.

- dictionary:

  A tibble storing information about each variable, including from where
  it is obtained.

- opts_df:

  A tibble storing for each module whether the involved variables have
  been transformed using the log or asinh function.
