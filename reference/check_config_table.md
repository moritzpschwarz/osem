# Check the configuration of the model contained in the config table

Check the configuration of the model contained in the config table

## Usage

``` r
check_config_table(config_table, quiet = TRUE)
```

## Arguments

- config_table:

  A tibble or data.frame specifying the model. It must include the
  following columns:

  type

  :   Character specifying whether the LHS variable is given as a
      definition/identity ("d") or modelled endogenously ("n")

  dependent

  :   LHS variable.

  independent

  :   RHS variables separated by +, -, /, or \*.

  lag

  :   RHS variables that should only enter as lags and not
      contemporaneously, separated by a comma.

  cvar

  :   Unique character identifiers to group LHS variables into
      estimation as a cointegrated vector autoregression (CVAR).

- quiet:

  Logical whether should plot the DAG.

## Value

A tibble that appends the order of the modules to be run to the input
tibble/data.frame. Variable rows from the same cvar system are collapsed
to a single row.

## Details

For backwards compatibility, we still allow for specifications table
that only specify "type", "dependent", and "independent" columns. In
these cases, we add empty columns "lag" and "cvar".

## Examples

``` r
config_table_small <- dplyr::tibble(
  type = c("d", "d", "n"),
  dependent = c("JL", "TOTS", "B"),
  independent = c("TOTS - CP - CO - J - A", "YF + B", "CP + J"),
  lag = c("", "", ""),
  cvar = c("", "", "")
)
osem:::check_config_table(config_table_small)
#> # A tibble: 3 × 9
#>   order type  dependent independent      lag   cvar  block_order sub_order index
#>   <int> <chr> <chr>     <chr>            <chr> <chr>       <int>     <dbl> <int>
#> 1     1 n     B         CP  +  J         ""    ""              1         1     1
#> 2     2 d     TOTS      YF  +  B         ""    ""              2         1     2
#> 3     3 d     JL        TOTS  -  CP  - … ""    ""              3         1     3

mwe <- dplyr::tibble(
  type = c("n", "n", "n", "n", "n", "n", "d", "n", "n"),
  dependent = c("X", "Y", "U", "V", "W", "M", "T", "Q", "S"),
  independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R"),
  lag = c("", "", "", "W", "", "U, Y", "", "", ""),
  cvar = c("system1", "system1", "", "", "", "", "", "", "")
)
osem:::check_config_table(mwe)
#> # A tibble: 8 × 9
#>   order type  dependent independent     lag    cvar  block_order sub_order index
#>   <int> <chr> <chr>     <chr>           <chr>  <chr>       <int>     <dbl> <int>
#> 1     1 n     U         ""              ""     ""              1         1     1
#> 2     2 n     Q         ""              ""     ""              2         1     2
#> 3     3 n     X,Y       "U"             ""     "sys…           3         1     3
#> 4     4 n     V         "U  +  W"       "W"    ""              4         1     4
#> 5     5 n     W         "U  +  V"       ""     ""              4         2     5
#> 6     6 n     S         "R"             ""     ""              5         1     6
#> 7     7 n     M         "Y  +  U"       "U, Y" ""              6         1     7
#> 8     8 d     T         "U  +  V  +  W" ""     ""              7         1     8
```
