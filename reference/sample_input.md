# Sample Input for the OSEM Model

A dataset containing input data that has been downloaded using the
functions in the osem package. This dataset is supposed to be used for
testing and to enable out-of-the-box estimation.

## Usage

``` r
sample_input
```

## Format

A tibble with 1200 rows and 9 variables:

- unit:

  Unit according to Eurostat.

- s_adj:

  Indication whether variable was seasonally adjusted or not. SCA refers
  to seasonal and calendar adjusted data.

- na_item:

  Variable Name according to the osem dictionary.

- geo:

  Geographic location for which the data was downloaded.

- time:

  The date of the observation.

- nace_r2:

  If applicable, the NACE2 sector code.

- p_adj:

  If applicable, price adjustment.

- cpa2_1:

  If applicable, Classification of Products by Activity.

## Source

Own compilation, data from <https://ec.europa.eu/eurostat>, downloaded
with osem::run_model()
