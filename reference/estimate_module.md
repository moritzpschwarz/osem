# Estimate the specific module using indicator saturation

Estimate the specific module using indicator saturation

## Usage

``` r
estimate_module(
  clean_data,
  dep_var_basename,
  x_vars_basename,
  use_logs = "both",
  trend = TRUE,
  ardl_or_ecm = "ardl",
  max.ar = 4,
  max.dl = 2,
  saturation = c("IIS", "SIS"),
  saturation.tpval = 0.01,
  max.block.size = 20,
  gets_selection = TRUE,
  selection.tpval = 0.01,
  keep,
  pretest_steps,
  quiet = FALSE,
  module
)
```

## Arguments

- clean_data:

  An input data.frame or tibble. Must be the output of clean_data() to
  fit all requirements.

- dep_var_basename:

  A character string of the name of the dependent variable as contained
  in clean_data() in a level form (i.e. no ln or D in front of the
  name).

- x_vars_basename:

  A character vector of the name(s) of the independent variable(s) as
  contained in clean_data() in a level form (i.e. no ln or D in front of
  the name).

- use_logs:

  To decide whether to log any variables. Must be one of 'both', 'y',
  'x', or 'none'. Default is 'both'.

- trend:

  Logical. To determine whether a trend should be added. Default is
  TRUE.

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

- module:

  A row of the specification table.

## Value

A list containing all estimated models, with the model with the smallest
BIC under 'best_model'.
