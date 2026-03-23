# Runs the OSEM model

Runs the OSEM model according to the given specification of modules.

## Usage

``` r
run_model(
  specification,
  dictionary = NULL,
  input = NULL,
  primary_source = c("download", "local"),
  save_to_disk = NULL,
  use_logs = "both",
  trend = TRUE,
  ardl_or_ecm = "ardl",
  max.ar = 4,
  max.dl = 4,
  saturation = c("IIS", "SIS"),
  saturation.tpval = 0.01,
  max.block.size = 20,
  gets_selection = TRUE,
  selection.tpval = 0.01,
  constrain.to.minimum.sample = TRUE,
  keep = NULL,
  pretest_steps = FALSE,
  present = FALSE,
  quiet = FALSE,
  plot = TRUE,
  inputdata_directory = NULL,
  cvar.ar = 2,
  coint_seasonal = FALSE,
  coint_deterministic = "const",
  coint_significance = "5pct"
)
```

## Arguments

- specification:

  A tibble or data.frame with three columns. Column names must be:
  'type', 'dependent', and 'independent'. The column 'type' must contain
  for each row a character of either 'd' (Identity) or 'n' (Definition -
  i.e. will be estimated). The column 'dependent' must contain the LHS
  (Y variables) and the column named 'independent' containing the RHS (x
  variables separated by + and -).

- dictionary:

  A tibble or data.frame storing the Eurostat variable code in column
  'eurostat_code' and the model variable name in 'model_varname'. If
  `download == TRUE` then the dictionary also requires a column named
  'dataset_id' that stores the Eurostat dataset id. When `NULL`, the
  [default
  dictionary](http://www.moritzschwarz.org/osem/reference/dict.md) is
  used.

- input:

  Character vector or list. An argument to directly pass input files for
  the OSEM model to be run. Can include a character path to .xlsx, .csv
  or .rds input files in which the data is stored. Cannot be `NULL` if
  `download == FALSE`.

- primary_source:

  A string. Determines whether `"download"` or `"local"` data loading
  takes precedence.

- save_to_disk:

  A path to a directory where the final dataset will be saved, including
  the file name and ending. Not saved when `NULL`.

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

- constrain.to.minimum.sample:

  Logical. Should all data series be constrained to the minimum data
  series? Default is `TRUE`.

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

- present:

  A logical value whether the final OSEM model output should be
  presented or not.

- quiet:

  Logical with default = FALSE. Should messages be displayed? These
  messages are intended to give more information about the estimation
  and data retrieval process.

- plot:

  Logical with default = TRUE. Should plots be displayed?

- inputdata_directory:

  Deprecated. Use 'input' instead. Functionality of specifying a
  directory is retained for now but this argument will be removed in the
  future.

- cvar.ar:

  Number of lags of the VAR system in levels. Must be \> 2.

- coint_seasonal:

  Logical value whether cointegration analysis should include seasonal
  dummies.

- coint_deterministic:

  Character specifying whether and if yes which deterministic component
  to include in the cointegrating relationship.

- coint_significance:

  Significance level for the rank test for cointegration. Can only be
  one of '1pct', '5pct', '10pct'.

## Value

An object of class
[osem](http://www.moritzschwarz.org/osem/reference/new_osem.md).

## Examples

``` r
spec <- dplyr::tibble(
  type = c(
    "d",
    "d",
    "n"
  ),
  dependent = c(
    "StatDiscrep",
    "TOTS",
    "Import"
  ),
  independent = c(
    "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm"
  )
)
# \donttest{
run_model(specification = spec,
          primary_source = "local",
          input = sample_input)
#> 
#> --- Estimation begins ---
#> Estimating Import = FinConsExpHH  +  GCapitalForm 
#> Constructing TOTS = GValueAdd  +  Import 
#> Constructing StatDiscrep = TOTS  -  FinConsExpHH  -  FinConsExpGov  -  GCapitalForm  -  Export 

#> OSEM Model Output
#> -----------------------
#> 
#> Estimation Options:
#> Sample: 2010-01-01 to 2022-04-01
#> Max AR Considered: 4
#> Estimation Option: ardl
#> 
#> Relationships considered: 
#> # A tibble: 3 × 3
#>   Model `Dep. Var.` `Ind. Var`                                                  
#> 1     1 Import      FinConsExpHH  +  GCapitalForm                               
#> 2     2 TOTS        GValueAdd  +  Import                                        
#> 3     3 StatDiscrep TOTS  -  FinConsExpHH  -  FinConsExpGov  -  GCapitalForm  -…
#> 
#> 
#> Relationships estimated in the order:  1,2,3
#> 
#> Diagnostics:
#>  # A tibble: 1 × 8
#>   `Dependent Variable` AR    ARCH  `Super Exogeneity`   IIS   SIS     n
#>   <chr>                <chr> <chr> <chr>              <int> <int> <int>
#> 1 Import               0.308 0.790 0.016**                0     3    50
#> # ℹ 1 more variable: `Share of Indicators` <dbl>

# }
```
