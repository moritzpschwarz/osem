# Runs a module

Runs a module in two different ways depending on whether the module is
endogenously modelled or endogenous by identity/definition.

## Usage

``` r
run_module(
  module,
  data,
  classification,
  use_logs = c("both", "y", "x"),
  trend = TRUE,
  ardl_or_ecm = "ardl",
  max.ar = 4,
  max.dl = 2,
  saturation = c("IIS", "SIS"),
  saturation.tpval = 0.01,
  max.block.size = 20,
  gets_selection = TRUE,
  selection.tpval = 0.01,
  opts_df,
  keep,
  pretest_steps,
  quiet,
  cvar.ar = 4,
  freq = NULL,
  coint_deterministic = "const",
  coint_significance = "5pct"
)
```

## Arguments

- module:

  A row of the specification table.

- data:

  A tibble or data.frame containing the full data for the OSEM model.

- classification:

  A data.frame with two columns: 'var' stores the Eurostat variable code
  and 'class' the classification code (one of `"n"`, `"d"`, or `"x"`)
  reflecting whether the variable is endogenous by modelling, endogenous
  by identity/definition, or exogenous. As returned by
  [classify_variables](http://www.moritzschwarz.org/osem/reference/classify_variables.md).

- use_logs:

  To decide whether to log any variables. Must be one of 'both', 'y',
  'x', or 'none'. Default is 'both'.

- trend:

  Logical. Should a trend be added? Default is TRUE.

- ardl_or_ecm:

  Either 'ardl' or 'ecm' to determine whether to estimate the model as
  an Autoregressive Distributed Lag Function (ardl) or as an Equilibrium
  Correction Model (ecm).

- max.ar:

  Integer. The maximum number of lags to use for the AR terms. as well
  as for the independent variables.

- max.dl:

  Integer. The maximum number of lags to use for the independent
  variables (the distributed lags).

- saturation:

  Carry out Indicator Saturation using the 'isat' function in the 'gets'
  package. Needs a character vector or string. Default is
  'c("IIS","SIS")' to carry out Impulse Indicator Saturation and Step
  Indicator Saturation. Other possible values are 'NULL' to disable or
  'TIS' or Trend Indicator Saturation. When disabled, estimation will be
  carried out using the 'arx' function from the 'gets' package.

- saturation.tpval:

  The target p-value of the saturation methods (e.g. SIS and IIS, see
  the 'isat' function in the 'gets' package). Default is 0.01.

- max.block.size:

  Integer. Maximum size of block of variables to be selected over,
  default = 20.

- gets_selection:

  Logical. Whether general-to-specific selection using the 'getsm'
  function from the 'gets' package should be done on the final
  saturation model. Default is TRUE.

- selection.tpval:

  Numeric. The target p-value of the model selection methods (i.e.
  general-to-specific modelling, see the 'getsm' function in the 'gets'
  package). Default is 0.01.

- opts_df:

  Internal object containing detailed options and information on
  individual modules.

- keep:

  Character. A string that will be used as regex (in
  [`grepl()`](https://rdrr.io/r/base/grep.html)) when selection is
  carried out. This argument therefore requires `gets_selection = TRUE`.
  Variables that match this character will not be selected over (see
  [`getsm`](https://rdrr.io/pkg/gets/man/getsm.html) for details).

- pretest_steps:

  Logical. Default is `FALSE`. This argument controls whether isat
  should first be run for SIS in isolation before other saturation
  methods are added (IIS, TIS). This can lead to better results if there
  are many IIS identified at the end of the estimation sample/forecast
  origin. If `TRUE` then
  [`isat`](https://rdrr.io/pkg/gets/man/isat.html) is first carried out
  just for SIS (if activated using 'sis = TRUE'), then the SIS breaks
  are pre-entered to another
  [`isat`](https://rdrr.io/pkg/gets/man/isat.html) estimation but not
  selected over. After both isat runs, a union model selection is done
  using [`gets`](https://rdrr.io/pkg/gets/man/gets.html).

- quiet:

  Logical. Should messages about the forecast procedure be suppressed?

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

## Value

Returns a list with two named elements.

- model:

  For endogenously modeled modules (`type == "n"`), an object of class
  [`isat`](https://rdrr.io/pkg/gets/man/isat.html) or
  [`arx`](https://rdrr.io/pkg/gets/man/arx.html). For
  identity/definition modules (`type == "d"`), `NULL`.

- data:

  The data (including transformations) that was used to estimate the
  model and the fitted values of the dependent variable.
