
# created using datapasta::dpasta - but this messes up the names
# so then need to use dput and setNames
dict_statcan <- tibble::tribble(
  ~model_varname,                                                      ~full_name, ~database,     ~dataset_id, ~freq,     ~GEO,                  ~Seasonal.adjustment, ~North.American.Industry.Classification.System..NAICS., ~North.American.Product.Classification.System..NAPCS.,                ~Prices,                                                ~Type.of.fuel, ~Products.and.product.groups,
  "FISH", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", "18-10-0004-01",   "m", "Canada",                                    NA,                                                     NA,                                                    NA,                     NA,                                                           NA,                     "Energy",
  "GAS",                          "Monthly Average Retail Price for gas", "statcan", "18-10-0001-01",   "m", "Canada",                                    NA,                                                     NA,                                                    NA,                     NA, "Regular unleaded gasoline at self service filling stations",                           NA,
  "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", "18-10-0004-01",   "m", "Canada",                                    NA,                                                     NA,                                                    NA,                     NA,                                                           NA,                     "Energy",
  "HICP_GAS",    "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", "18-10-0004-01",   "m", "Canada",                                    NA,                                                     NA,                                                    NA,                     NA,                                                           NA,                   "Gasoline",
  "IndProd",                  "Total, Industrial product price index (IPPI)", "statcan", "18-10-0266-01",   "m", "Canada",                                    NA,                                                     NA,        "Total, Industrial product price index (IPPI)",                     NA,                                                           NA,                           NA,
  "IndProdGDP",          "Industrial production [T010] in 2017 constant prices", "statcan", "36-10-0434-01",   "m", "Canada", "Seasonally adjusted at annual rates",                         "Industrial production [T010]",                                                    NA, "2017 constant prices",                                                           NA,                           NA
) %>% setNames(c("model_varname", "full_name", "database", "dataset_id", "freq",
                 "GEO", "Seasonal adjustment", "North American Industry Classification System (NAICS)",
                 "North American Product Classification System (NAPCS)", "Prices",
                 "Type of fuel", "Products and product groups"))




usethis::use_data(x = dict_statcan, overwrite = TRUE)










