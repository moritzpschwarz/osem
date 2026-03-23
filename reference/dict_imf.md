# Dictionary for translating IMF model variable names

A dataset containing IMF model variable names and metadata for data
retrieval. For details see
[create_dictionaries](http://www.moritzschwarz.org/osem/reference/create_dictionaries.md).

## Usage

``` r
dict_imf
```

## Format

A tibble with 1 row and 8 variables:

- model_varname:

  Unique variable name used in model equations

- full_name:

  Full description of the variable

- database:

  Name of the data source, here `"imf"`

- dataset_id:

  Identifier of the IMF dataset

- freq:

  Frequency of the data, with `"M"` for monthly

- ref_area:

  Geographical reference area, e.g., `"W00"` for world

- commodity:

  Type of commodity, such as `"POILAPSP"` for oil

- unit_measure:

  Unit of measurement, e.g., `"USD"` for U.S. dollars

## Source

Data retrieved from IMF, available at <https://www.imf.org/en/Data>.

## Note

Should new filters be necessary, users can simply add additional columns
to the dictionary with names that exactly match how they appear in the
imf.data package. For example, if an imf.data query returns a column
with a specific name, that exact name should be used as a column in this
dictionary.
