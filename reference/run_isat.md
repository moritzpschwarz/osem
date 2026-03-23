# Internal function to run and prepare isat estimation

Internal function to run and prepare isat estimation

## Usage

``` r
run_isat(
  yvar,
  y.name,
  xvars,
  clean_data,
  ar = NULL,
  mc,
  saturation,
  saturation.tpval,
  max.block.size,
  pretest_steps,
  determine.blocksize
)
```

## Arguments

- yvar:

  The y variable

- y.name:

  The name of the y variable.

- xvars:

  All x variables.

- clean_data:

  An input data.frame or tibble. Must be the output of clean_data() to
  fit all requirements.

- ar:

  numeric vector or `NULL`. The AR order to be used.

- mc:

  Logical. Whether to use an intercept.

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

- determine.blocksize:

  Logical. Whether max.block.size or needs to be determined.

## Value

An isat object.
