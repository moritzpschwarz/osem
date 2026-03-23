# Estimate the specific CVAR sub-system using the Johansen procedure

Estimate the specific CVAR sub-system using the Johansen procedure

## Usage

``` r
estimate_cvar(
  clean_data,
  system_name,
  dep_vars_basename,
  x_vars_basename,
  use_logs,
  cvar.ar,
  freq,
  coint_deterministic = c("none", "const", "trend"),
  coint_significance = c("1pct", "5pct", "10pct"),
  quiet = FALSE
)
```

## Arguments

- clean_data:

  An input data.frame or tibble. Must be the output of clean_data() to
  fit all requirements.

- system_name:

  A character string specifying the name of the CVAR sub-system.

- dep_vars_basename:

  A character string of the names of the dependent variable separated by
  a comma, and contained in clean_data() in a level form (i.e. no ln or
  D in front of the name).

- x_vars_basename:

  A character vector of the name(s) of the independent variable(s), and
  contained in clean_data() in a level form (i.e. no ln or D in front of
  the name). Can be an empty string if no exogenous regressors are
  included.

- use_logs:

  To decide whether to log any variables. Must be one of 'both', 'y',
  'x', or 'none'. Default is 'both'.

- cvar.ar:

  Number of lags of the VAR system in levels. Must be \> 2.

- freq:

  Data frequency if want to include seasonal dummies, NULL otherwise.

- coint_deterministic:

  Character specifying whether and if yes which deterministic component
  to include in the cointegrating relationship.

- coint_significance:

  Significance level for the rank test for cointegration. Can only be
  one of '1pct', '5pct', '10pct'.

- quiet:

  Logical with default = FALSE. Should messages be displayed? These
  messages are intended to give more information about the estimation
  and data retrieval process.

## Value

A list with class 'osem.cvar' containing unit root tests for the
variables involved, the resulting CVAR model, and the argument setting:

- cointtest:

  An object of
  [ca.jo-class](https://rdrr.io/pkg/urca/man/ca.jo-class.html) with the
  cointegration test result.

- vecm:

  A list storing the restricted vector error correction model as
  returned by
  [`urca::cajorls()`](https://rdrr.io/pkg/urca/man/cajorls.html).

- varm:

  An object storing the restricted vector autoregressive model in levels
  as returned by
  [`vars::vec2var()`](https://rdrr.io/pkg/vars/man/vec2var.html).

- rank:

  The CVAR rank as determined by Johansen's trace test.

- urtest:

  A list of two lists storing the augmented Dickey-Fuller unit root
  tests for all x and y variables as class
  [ur.df-class](https://rdrr.io/pkg/urca/man/ur.df-class.html).

- args:

  A list storing the settings of the function call.
