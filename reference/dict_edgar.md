# Dictionary for translating EDGAR model variable names

A dataset containing EDGAR (Emissions Database for Global Atmospheric
Research) model variable names and all information required for
obtaining emissions data. For details see
[create_dictionaries](http://www.moritzschwarz.org/osem/reference/create_dictionaries.md).

## Usage

``` r
dict_edgar
```

## Format

A tibble with 45 rows and 7 variables:

- ipcc_sector:

  EDGAR IPCC National Greenhouse Gas Inventories sector code, see
  <https://www.ipcc-nggip.iges.or.jp/efdb/find_ef.php?reset=> and
  <https://www.ipcc-nggip.iges.or.jp/public/2006gl/index.html>

- full_name:

  Full name/description of the emission source

- model_varname:

  Variable name in the model equations, must be unique

- dataset_id:

  URL link to the dataset file on the web

- database:

  Name of the database, always "edgar" for this dictionary

- geo:

  ISO 3166-1 alpha-2 country code, e.g. "DE" for Germany

- freq:

  Frequency of the variable in the dataset, e.g. `"m"` for monthly

## Source

EDGAR database, available at
<https://edgar.jrc.ec.europa.eu/dataset_ghg80>.

## Details

To see all details on IPCC sectors see
<https://www.ipcc-nggip.iges.or.jp/efdb/find_ef.php?reset=> and
<https://www.ipcc-nggip.iges.or.jp/public/2006gl/index.html>

## Note

Should new filters be necessary, users can simply add additional columns
to the dictionary with names that exactly match how they appear in the
EDGAR data retrieval functions.
