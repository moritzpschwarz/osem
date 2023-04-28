# creates a dictionary to translate variables names from model to Eurostat
# (key,value)-pairs should be unique, not sure how to deal with different version of "JL", for example

dict <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col, ~freq, ~geo, ~unit, ~s_adj, ~nace_r2, ~ipcc_sector, ~cpa2_1, ~siec,
  "TOTS", "Total Supply", NA, "TOTS", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  "GDP", "Gross domestic product at market prices", "eurostat", "B1GQ", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "GValueAdd", "Value added, gross", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "TOTAL", NA, NA, NA,
  "Export", "Exports of goods and services", "eurostat","P6", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "Import", "Imports of goods and services", "eurostat", "P7", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "GCapitalForm", "Gross capital formation", "eurostat", "P5G", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "FinConsExp", "Final consumption expenditure", "eurostat", "P3", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "FinConsExpGov", "Final consumption expenditure of general government", "eurostat", "P3_S13", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "FinConsExpHH", "Household and NPISH final consumption expenditure", "eurostat", "P31_S14_S15", "namq_10_gdp", "na_item", "q", "AT", "CLV05_MEUR", "SCA", NA, NA, NA, NA,
  "StatDiscrep", "Statistical discrepancy (expenditure approach)", "eurostat", "YA0", "namq_10_gdp", "na_item", "q", "AT", "CP_MEUR", "SCA", NA, NA, NA, NA,
  "Emissions", "Greenhouse Gas Emissions (All NACE and HH)", "eurostat", "GHG", "env_ac_aigg_q", "airpol", "q", "AT", "THS_T", "SCA", "TOTAL_HH", NA, NA, NA,
  "LabCostManuf", "Manufacturing Labour cost index - Total Labour Cost", "eurostat", "LM-LCI-TOT", "ei_lmlc_q", "indic", "q", "AT", "I16", "SCA", "C", NA, NA, NA,
  "DomDemand", "Domestic Demand", NA, "DomDemand", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  "GValueAddManuf", "Value added, gross Manufacturing", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "C", NA, NA, NA,
  "GValueAddGov", "Value added, gross Government", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "O-Q", NA, NA, NA,
  "LabCostConstr", "Construction Labour cost index - Total Labour Cost", "eurostat", "LM-LCI-TOT", "ei_lmlc_q", "indic", "q", "AT", "I16", "SCA", "F", NA, NA, NA,
  "BuildingPermits", "Building permits - m^2 useful floorspace - Buildings", "eurostat", "PSQM", "sts_cobp_q", "indic_bt", "q", "AT", "I15", "SCA", "F_CC1", NA,  "CPA_F41001_41002", NA,
  "LabCostService", "Service Labour cost index - Total Labour Cost", "eurostat", "LM-LCI-TOT", "ei_lmlc_q", "indic", "q", "AT", "I16", "SCA", "G-N", NA, NA, NA,
  "GValueAddConstr", "Value added, gross Construction", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "F", NA, NA, NA,
  "GValueAddAgri", "Value added, gross Agriculture", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "A", NA, NA, NA,
  "GValueAddIndus", "Value added, gross Industry", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "B-E", NA, NA, NA,
  "GValueAddInfocom", "Value added, gross Information and Communication", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "J", NA, NA, NA,
  "GValueAddFinance", "Value added, gross Financial Services", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "K", NA, NA, NA,
  "GValueAddRealest", "Value added, gross Real Estate", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "L", NA, NA, NA,
  "GValueAddResearch", "Value added, gross Scientific and Professional Services", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "M_N", NA, NA, NA,
  "GValueAddArts", "Value added, gross Arts and Entertainment", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "R-U", NA, NA, NA,
  "GValueAddWholesaletrade", "Value added, gross Wholesale and retail trade and Tourism", "eurostat", "B1G", "namq_10_a10", "na_item", "q", "AT", "CLV05_MEUR", "SCA", "G-I", NA, NA, NA,
  "HICP", "Harmonised Index of Consumer Prices, all items, index 100 = 2015", "eurostat", "CP00", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2015", "eurostat", "NRG", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HICP_Electricity", "Harmonised Index of Consumer Prices, Electricity, index 100 = 2015", "eurostat", "CP0451", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HICP_Gas", "Harmonised Index of Consumer Prices, Gas, index 100 = 2015", "eurostat", "CP0452", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HICP_Liquid_Fuels", "Harmonised Index of Consumer Prices, Liquid Fuels, index 100 = 2015", "eurostat", "CP0453", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HICP_Solid_Fuels", "Harmonised Index of Consumer Prices, Solid Fuels, index 100 = 2015", "eurostat", "CP0454", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HICP_Heat", "Harmonised Index of Consumer Prices, Heat Energy, index 100 = 2015", "eurostat", "CP0455", "prc_hicp_midx", "coicop", "m", "AT", "I15", NA, NA, NA, NA, NA,
  "HDD", "Heating Degree Days", "eurostat", "HDD", "nrg_chdd_m", "indic_nrg", "m", "AT", "NR", NA, NA, NA, NA, NA,
  "CDD", "Cooling Degree Days", "eurostat", "CDD", "nrg_chdd_m", "indic_nrg", "m", "AT", "NR", NA, NA, NA, NA, NA,
  "EmiCH4Livestock", "Methane Emissions from Livestock", "edgar", NA, "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CH4_m_2000_2021.zip", NA, "m", "AT", NA, NA, NA, "3.A", NA, NA,
  "EmiCO2Industry", "Carbon Emissions from Industrial Processes and Product Use", "edgar", NA, "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v70_FT2021_GHG/v70_FT2021_CO2_m_2000_2021.zip", NA, "m", "AT", NA, NA, NA, "2", NA, NA
)
dict <- as.data.frame(dict)

usethis::use_data(x = dict, overwrite = TRUE)










