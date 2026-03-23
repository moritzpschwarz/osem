# Dictionary for translating StatCan model variable names

A dataset containing StatCan model variable names and metadata for data
retrieval. For details see
[create_dictionaries](http://www.moritzschwarz.org/osem/reference/create_dictionaries.md).

## Usage

``` r
dict_statcan
```

## Format

A tibble with 5 rows and 12 variables:

- model_varname:

  Unique variable name used in model equations

- full_name:

  Full description of the variable

- database:

  Name of the data source, here `"statcan"`

- dataset_id:

  Identifier of the StatCan dataset

- freq:

  Frequency of the data, with `"m"` for monthly

- GEO:

  Geographical location, typically `"Canada"`

- Seasonal adjustment:

  Indicates if data is seasonally adjusted, where applicable

- North American Industry Classification System (NAICS):

  Classification of industries, based on NAICS

- North American Product Classification System (NAPCS):

  Product classification system

- Prices:

  Price level for specific categories, if applicable

- Type of fuel:

  Specifies the type of fuel, for example `"Regular unleaded"`

- Products and product groups:

  Specific product or group as classified by StatCan

## Source

Data retrieved from StatCan, available at <https://www.statcan.gc.ca/>.

## Note

Should new filters be necessary, users can simply add additional columns
to the dictionary with names that exactly match how they appear in the
statcanR package. For example, if a statcanR query returns a column
called `Socio-economic objectives`, users should add that exact column
name (including all spaces and other characters) to the dictionary.
