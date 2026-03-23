# Loads or downloads the necessary data for all modules

Either downloads the necessary data for all modules from Eurostat using
dataset ids or loads the files provided in the user-specified input
directory and selects the required variables.

## Usage

``` r
load_or_download_variables(
  specification,
  dictionary = NULL,
  primary_source = c("download", "local"),
  input = NULL,
  save_to_disk = NULL,
  quiet = FALSE,
  constrain.to.minimum.sample = TRUE
)
```

## Arguments

- specification:

  A tibble or data.frame as returned by
  [`check_config_table`](http://www.moritzschwarz.org/osem/reference/check_config_table.md).

- dictionary:

  A tibble or data.frame storing the Eurostat variable code in column
  'eurostat_code'. If `download == TRUE` then the dictionary also
  requires column named 'dataset_id' that stores the Eurostat dataset
  id. When `NULL`, the [default
  dictionary](http://www.moritzschwarz.org/osem/reference/dict.md) is
  used.

- primary_source:

  A character of length 1 determining whether the required variables are
  first obtained from `"download"` and then `"local"`ly or vice versa.

- input:

  A character vector of file paths, a data.frame, or a list of
  data.frames or file paths. Required if `download == FALSE` and at
  least one variable is specified as coming from a local source.

- save_to_disk:

  A path to a directory where the final dataset will be saved, including
  file name and ending. Not saved when `NULL`.

- quiet:

  Logical with default = `FALSE`. Should messages be displayed? These
  messages are intended to give more information about the estimation
  and data retrieval process.

- constrain.to.minimum.sample:

  Logical. Should all data series be constrained to the minimum data
  series? Default is `TRUE`.
