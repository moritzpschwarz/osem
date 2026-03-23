# Dictionary for translating Eurostat model variable names

A dataset containing Eurostat model variable names and all information
required for retrieving data from the Eurostat database. For details see
[create_dictionaries](http://www.moritzschwarz.org/osem/reference/create_dictionaries.md).

## Usage

``` r
dict_eurostat
```

## Format

A tibble with 167 rows and 22 variables:

- model_varname:

  Variable name in the model equations, must be unique

- full_name:

  Full name/description of the variable

- database:

  Name of the database, always "eurostat" for this dictionary

- variable_code:

  Identifier of the variable, e.g. Eurostat variable code

- dataset_id:

  Identifier of the Eurostat dataset where the variable is available

- var_col:

  Name of variable/column in dataset

- freq:

  Frequency of the variable in the dataset. `"a"` for annual, `"q"` for
  quarterly, `"m"` for monthly

- geo:

  ISO 3166-1 alpha-2 country code, e.g. "DE" for Germany

- unit:

  Eurostat unit in which the variable is measured

- s_adj:

  Eurostat seasonal adjustment indicator

- nace_r2:

  Eurostat identifier for NACE Rev. 2 classification

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

## Source

Eurostat database, available at <https://ec.europa.eu/eurostat>.

## Note

Should new filters be necessary, users can simply add additional columns
to the dictionary with names that exactly match how they appear in the
eurostat R package. For example, if a eurostat query returns a column
with a specific name, that exact name should be used as a column in this
dictionary.
