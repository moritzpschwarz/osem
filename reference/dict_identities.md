# Dictionary for economic model identities

A dataset containing definitions of economic identities used in the
model. These represent mathematical relationships between variables that
must hold by definition. For details see
[create_dictionaries](http://www.moritzschwarz.org/osem/reference/create_dictionaries.md).

## Usage

``` r
dict_identities
```

## Format

A tibble with 7 rows and 3 variables:

- model_varname:

  Variable name of the identity in the model equations, must be unique

- full_name:

  Full name/description of the identity

- variable_code:

  Additional identifier for the identity where applicable

## Source

Own compilation based on standard economic accounting relationships.

## Note

These identities represent accounting relationships and mathematical
constraints that must be satisfied within the economic model.
