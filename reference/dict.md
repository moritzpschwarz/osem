# Dictionary for translating model variable names

A combined dataset containing all model variable dictionaries merged
using bind_rows(). For details see
[create_dictionaries](http://www.moritzschwarz.org/osem/reference/create_dictionaries.md).
Includes variable names and metadata required for obtaining data from
different sources.

## Usage

``` r
dict
```

## Format

A tibble with 226 rows and 33 variables:

- model_varname:

  Variable name in the model equations, must be unique

- full_name:

  Full name/description of the variable

- variable_code:

  Identifier of the variable if applicable, e.g. Eurostat variable code

- database:

  Name of the database (e.g., "eurostat", "edgar", "statcan", "imf")

- dataset_id:

  Identifier of the dataset where the variable is available

- var_col:

  Name of variable/column in dataset

- freq:

  Frequency of the variable in the dataset. `"m"` for monthly, `"q"` for
  quarterly, `"a"` for annual

- geo:

  ISO 3166-1 alpha-2 country code or region identifier (lowercase)

- unit:

  Unit in which the variable is measured

- s_adj:

  Seasonal adjustment indicator

- nace_r2:

  Identifier for NACE Rev. 2 classification where applicable

- cpa2_1:

  Eurostat identifier for Classification of Products by Activity (CPA)

- sector:

  Sector classification where applicable

- direct:

  Direction indicator where applicable

- age:

  Age category where applicable

- partner:

  Partner country or entity where applicable

- finpos:

  Financial position indicator where applicable

- p_adj:

  Price adjustment indicator where applicable

- meat:

  Meat type classification where applicable

- citizen:

  Citizenship status where applicable

- wstatus:

  Work status where applicable

- tra_oper:

  Transport operation classification where applicable

- ipcc_sector:

  EDGAR IPCC National Greenhouse Gas Inventories sector code

- GEO:

  Geographical location for StatCan data (uppercase)

- Seasonal adjustment:

  Seasonal adjustment indicator for StatCan data

- North American Industry Classification System (NAICS):

  Classification of industries for StatCan data

- North American Product Classification System (NAPCS):

  Product classification for StatCan data

- Prices:

  Price level for specific categories in StatCan data

- Type of fuel:

  Fuel type specification for StatCan data

- Products and product groups:

  Product categorization for StatCan data

- ref_area:

  Geographical reference area for IMF data

- commodity:

  Type of commodity for IMF data

- unit_measure:

  Unit of measurement for IMF data

## Source

Combined from multiple data sources including Eurostat, EDGAR, StatCan,
and IMF.

## Note

Should new filters be necessary, users can simply add additional columns
to the dictionary with names that exactly match how they appear in the
relevant retrieval function. For example, if using the eurostat R
package, statcanR, or imf.data, the column names should match exactly as
they appear in the returned data from these packages. If for example a
statcanR query returns a column called `Socio-economic objectives`,
users should add that exact column name (including all spaces and other
characters) to the dictionary.
