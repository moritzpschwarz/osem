# creates a dictionary to translate variables names from model to Eurostat
# (key,value)-pairs should be unique, not sure how to deal with different version of "JL", for example

dict <- tibble::tribble(
  ~`eurostat_code`, ~`model_varname`, ~`full_name`, ~`dataset_id`,
  "TOTS", "TOTS", "Total Supply", NA,
  "B1GQ", "Y", "Gross domestic product at market prices", "namq_10_gdp",
  "B1G", "YF", "Value added, gross", "namq_10_gdp",
  "P6", "A", "Exports of goods and services", "namq_10_gdp",
  "P7", "B", "Imports of goods and services", "namq_10_gdp",
  "P5G", "J","Gross capital formation", "namq_10_gdp",
  "P3", "CP + CO", "Final consumption expenditure", "namq_10_gdp",
  "P3_S13", "CO", "Final consumption expenditure of general government", "namq_10_gdp",
  "P31_S14_S15", "CP", "Household and NPISH final consumption expenditure", "namq_10_gdp",
  "YA0", "JL", "Statistical discrepancy (expenditure approach)", "namq_10_gdp"
)

usethis::use_data(x = dict, overwrite = TRUE)

