# creates a dictionary to translate variables names from model to Eurostat
# (key,value)-pairs should be unique, not sure how to deal with different version of "JL", for example

dict <- tibble::tribble(
  ~eurostat_code, ~model_varname, ~full_name, ~dataset_id, ~var_col, ~nace_r2, ~cpa2_1,
  "TOTS", "TOTS", "Total Supply", NA, NA, NA, NA,
  "B1GQ", "GDP", "Gross domestic product at market prices", "namq_10_gdp", "na_item", NA, NA,
  "B1G", "GValueAdd", "Value added, gross", "namq_10_a10", "na_item", "TOTAL", NA,
  "P6", "Export", "Exports of goods and services", "namq_10_gdp", "na_item", NA, NA,
  "P7", "Import", "Imports of goods and services", "namq_10_gdp", "na_item", NA, NA,
  "P5G", "GCapitalForm", "Gross capital formation", "namq_10_gdp", "na_item", NA, NA,
  "P3", "FinConsExp", "Final consumption expenditure", "namq_10_gdp", "na_item", NA, NA,
  "P3_S13", "FinConsExpGov", "Final consumption expenditure of general government", "namq_10_gdp", "na_item", NA, NA,
  "P31_S14_S15", "FinConsExpHH", "Household and NPISH final consumption expenditure", "namq_10_gdp", "na_item", NA, NA,
  "YA0", "StatDiscrep", "Statistical discrepancy (expenditure approach)", "namq_10_gdp", "na_item", NA, NA,
  "GHG", "Emissions", "Greenhouse Gas Emissions (All NACE and HH)", "env_ac_aigg_q", "airpol", "TOTAL_HH", NA,
  "LM-LCI-TOT", "LabCostManuf", "Manufacturing Labour cost index - Total Labour Cost", "ei_lmlc_q", "indic", "C", NA,
  "DomDemand", "DomDemand", "Domestic Demand", NA, NA, NA, NA,
  "B1G", "GValueAddManuf", "Value added, gross Manufacturing", "namq_10_a10", "na_item", "C", NA,
  "B1G", "GValueAddGov", "Value added, gross Government", "namq_10_a10", "na_item", "O-Q", NA,
  "LM-LCI-TOT", "LabCostConstr", "Construction Labour cost index - Total Labour Cost", "ei_lmlc_q", "indic", "F", NA,
  "PSQM", "BuildingPermits", "Building permits - m^2 useful floorspace - Buildings", "sts_cobp_q", "indic_bt", "F_CC1", "CPA_F41001_41002",
  "LM-LCI-TOT", "LabCostService", "Service Labour cost index - Total Labour Cost", "ei_lmlc_q", "indic", "G-N", NA,
  "B1G", "GValueAddConstr", "Value added, gross Construction", "namq_10_a10", "na_item", "F", NA,
  "B1G", "GValueAddAgri", "Value added, gross Agriculture", "namq_10_a10", "na_item", "A", NA,
  "B1G", "GValueAddIndus", "Value added, gross Industry", "namq_10_a10", "na_item", "B-E", NA,
  "B1G", "GValueAddInfocom", "Value added, gross Information and Communication", "namq_10_a10", "na_item", "J", NA,
  "B1G", "GValueAddFinance", "Value added, gross Financial Services", "namq_10_a10", "na_item", "K", NA,
  "B1G", "GValueAddRealest", "Value added, gross Real Estate", "namq_10_a10", "na_item", "L", NA,
  "B1G", "GValueAddResearch", "Value added, gross Scientific and Professional Services", "namq_10_a10", "na_item", "M_N", NA,
  "B1G", "GValueAddArts", "Value added, gross Arts and Entertainment", "namq_10_a10", "na_item", "R-U", NA,
  "B1G", "GValueAddWholesaletrade", "Value added, gross Wholesale and retail trade and Tourism", "namq_10_a10", "na_item", "G-I", NA
)
dict <- as.data.frame(dict)

usethis::use_data(x = dict, overwrite = TRUE)


