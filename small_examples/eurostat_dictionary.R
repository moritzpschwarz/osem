library(tidyverse)

# Comprehensive eurostat dictionary

add_dict_entry <- function(dict, model_varname, full_name, database, variable_code, dataset_id, var_col, freq, geo,
                           unit = NA, s_adj = NA, nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
                           tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA,
                           partner = NA, finpos = NA) {
  dict %>%
    add_row(model_varname = model_varname, full_name = full_name, database = database, variable_code = variable_code,
            dataset_id = dataset_id, var_col = var_col, freq = freq, geo = geo, unit = unit, s_adj = s_adj,
            nace_r2 = nace_r2, ipcc_sector = ipcc_sector, cpa2_1 = cpa2_1, siec = siec, sector = sector, tax = tax,
            currency = currency, direct = direct, co_nco = co_nco, train = train, vehicle = vehicle, age = age,
            partner = partner, finpos = finpos) %>%
    return()
}

dictionary <- tibble(model_varname = "Supply", full_name = "Total Supply", database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, geo = NA, unit = NA, s_adj = NA, nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA, tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA, partner = NA, finpos = NA)

dictionary <- dictionary %>%
  # annual data
  add_dict_entry("Demand", "Total Demand", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("GDPOutput", "GDP Output Approach", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("GDPExpenditure", "GDP Expenditure Approach", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ATaxesA", "Product Taxes", "eurostat", "D21", "nama_10_gdp", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ASubsidiesA", "Product Subsidies", "eurostat", "D31", "nama_10_gdp", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAAgriculture", "Gross Value Added Agriculture", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "A", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAIndustry", "Gross Value Added Industry except Construction", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "B-E", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAConstruction", "Gross Value Added Construction", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "F", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VARetail", "Gross Value Added Wholesail and Retail Trade, Transportation and Storage, Accommodation and Food Service Activities", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "G-I", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAInformation", "Gross Value Added Information and Communication", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "J", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAFinance", "Gross Value Added Financial and Insurance Activities", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "K", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VARealEstate", "Gross Value Added Real Estate Services", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "L", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAService", "Gross Value Added Professional, Scientific, Technical, Administration and Support Service Activities", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "M_N", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAGov", "Gross Value Added Public Administration, Defence, Education, Human Health and Social Work Activities", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "O-Q", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAOther", "Gross Value Added Other Services", "eurostat", "B1G", "nama_10_a10", "na_item", "A", "DE", "CP_MEUR", NA, "R-U", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ConsHH", "Household and NPISH Final Consumption Expenditure", "eurostat", "P31_S14_S15", "nama_10_gdp", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ConsGov", "Government Final Consumption Expenditure", "eurostat", "P3_S13", "nama_10_gdp", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("Exports", "Exports of Goods and Services", "eurostat", "P6", "nama_10_gdp", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("Imports", "Imports of Goods and Services", "eurostat", "P7", "nama_10_gdp", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("REER", "Real Effective Exchange Rate", "eurostat", "REER_IC42_CPI", "ert_eff_ic_a", "exch_rt", "A", "DE", "I15", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("PriceETS", "Average Annual EU ETS Price, Nominal", "local", "PriceETS", NA, "na_item", NA, "DE", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("PriceOil", "Average Crude Brent Oil Price in USD, Nominal", "local", "PriceOil", NA, "na_item", NA, "DE", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Agriculture", "CO2 Emissions Agriculture in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "A", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Industry", "Co2 Emissions Industry in Tonnes", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Mining", "CO2 Emissions Mining and Quarrying in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "B", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Manufacturing", "CO2 Emissions Manufacturing in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "C", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Energy", "CO2 Emissions Electricity, Gas, Steam and Air Conditioning Supply in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "D", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Water", "CO2 Emissions Water Supply, Sewerage, Waste Management and Remediation Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "E", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Construction", "CO2 Emissions Construction in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "F", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Wholesale", "CO2 Emissions Wholesale and Retail Trade, Repair of Motor Vehicles and Motorcycles in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "G", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Transport", "CO2 Emissions Transportation and Storage in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "H", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Accommodation", "CO2 Emissions Accommodation and Food Service Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "I", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Information", "CO2 Emissions Information and Communication in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "J", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Finance", "CO2 Emissions Financial and Insurance Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "K", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2RealEstate", "CO2 Emissions Real Estate Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "L", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Science", "CO2 Emissions Professional, Scientific and Technical Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "M", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Support", "CO2 Emissions Administrative and Support Service Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "N", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Public", "CO2 Emissions Public Administration and Defence, Compulsory Social Security in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "O", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Education", "CO2 Emissions Education in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "P", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Health", "CO2 Emissions Human Health and Social Work Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "Q", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Arts", "CO2 Emissions Arts, Entertainment and Recreation in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "R", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Service", "CO2 Emissions Other Service Activities in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "S", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2HHEmployment", "CO2 Emissions Activities of Households as Employers, Undifferentiated Goods- and Services-Producing Activities of Households for Own Use in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "T", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2Extraterritorial", "CO2 Emissions Activities of Extraterritorial Organisations and Bodies in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "U", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CO2HH", "CO2 Emissions Total Activities by Households in Tonnes", "eurostat", "CO2", "env_ac_ainah_r2", "airpol", "A", "DE", "T", NA, "TOTAL_HH", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("IncomeHH", "Gross Disposable Income Households and Non-Profit Institutions Serving Households in Mio EUR", "eurostat", "B6G", "nasa_10_nf_tr", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S14_S15", NA, NA, "RECV", NA) %>%
  add_dict_entry("FinWealthHH", "Household Financial Net Worth in Mio EUR", "eurostat", "BF90", "nasa_10_f_bs", "na_item", "A", "DE", "MIO_EUR", NA, NA, NA, NA, NA, "S14_S15", NA, NA, NA, "CO") %>%
  add_dict_entry("HouseWealthHH", "Household Dwellings Assets", "eurostat", "N111N", "nama_10_nfa_bs", "asset10", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S14_S15", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("LandWealthHH", "Household Land Assets", "eurostat", "N211N", "nama_10_nfa_bs", "asset10", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S14_S15", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("HCPI", "Harmonised Consumer Price Index All Goods", "eurostat", "CP00", "prc_hicp_aind", "coicop", "A", "DE", "INX_A_AVG", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("CapFormHH", "Gross Fixed Capital Formation Households", "eurostat", "P51G", "nasa_10_nf_tr", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S14_S15", NA, NA, "PAID", NA) %>%
  add_dict_entry("CapFormGov", "Gross Fixed Capital Formation Government", "eurostat", "P51G", "nasa_10_nf_tr", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S13", NA, NA, "PAID", NA) %>%
  add_dict_entry("CapFormFirm", "Gross Fixed Capital Non-Financial Corporations", "eurostat", "P51G", "nasa_10_nf_tr", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S11", NA, NA, "PAID", NA) %>%
  add_dict_entry("CapFormFinFirm", "Gross Fixed Capital Financial Corporations", "eurostat", "P51G", "nasa_10_nf_tr", "na_item", "A", "DE", "CP_MEUR", NA, NA, NA, NA, NA, "S12", NA, NA, "PAID", NA) %>%
  add_dict_entry("ExportsAgric", "Exports of Agriculture, forestry and fishing", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR", NA, "A", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ExportsMining", "Exports of mining and quarrying", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"B", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsManufac", "Exports of manufacturing", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"C", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsElectricity", "Exports of electricity, gas, steam and air conditioning supply", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"D", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsWater", "Exports of Water supply; sewerage, waste management and remediation activites", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"E", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsConstruction", "Exports of construction", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"F", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsRetailMotor", "Exports of wholesale and retail trade; repair of motor vehicles and motor cycles", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"G", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsTransportStorage", "Exports of transportation and storage", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"H", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsAccomServices", "Exports of accomodation and services", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"I", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsInfoComms", "Exports of information and communication", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"J", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsFinnanceInsure", "Exports of finnance and insurance", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"K", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsRealestate", "Exports of Real estate activites", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"L", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsSci", "Exports of professional, scientific and technical activites", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"M", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsAdminSuport", "Exports of administrative and support service activites", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"N", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsAdminDefence", "Exports of public administrative and defence; compulsory social security", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"O", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsEducation", "Exports of education", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"P", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsHealth", "Exports of Human health", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"Q", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsArts", "Exports of Arts, entertainment and recreation", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"R", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsOther", "Exports of other services", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"S", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsHouseholdEmployeers", "Exports of activities of households as employers", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"T", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("ExportsExtraterritorial", "Exports of extraterritorial organizations and bodies", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"U", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("WageCostsIndustry", "Wage costs for Industry except Construction", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "B-E", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("HoursWorkedPerEmployeeIndustry", "Hours worked per employed person for Industry except Construction", "eurostat", "HW_EMP", "nama_10_lp_a21", "na_item", "A", "DE", "HW", NA, "B-E", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsGovArtsOther", "Wage costs Public administration and defence; compulsory social security; education; human health and social work activities; arts, entertainment and recreation; other service activities", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "O-S", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>% # actually no data
  add_dict_entry("WageCostsArts", "Wage costs arts", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "R", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsOther", "Wage costs other services", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "S", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsWholesale", "Wage costs wholesale and retail trade", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "G", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsTransport", "Wage costs transport", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "H", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsAccommodation", "Wage costs food and accomodation", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "I", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("HoursWorkedPerEmployeeRetail", "Hours worked per employed person for Retail", "eurostat", "HW_EMP", "nama_10_lp_a21", "na_item", "A", "DE", "HW", NA, "G-I", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsRealEstate", "Wage costs real estate activities", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "L", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%  # actually no data
  add_dict_entry("WageCostsScience", "Wage costs professional, scientific, techincal wage costs", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("WageCostsSupport", "Wage costs Admin and support services", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "N", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("HoursWorkedPerEmployeeScience", "Hours worked per employed person for professionals", "eurostat", "HW_EMP", "nama_10_lp_a21", "na_item", "A", "DE", "HW", NA, "M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("HoursWorkedPerEmployeeSupport", "Hours worked per employed person for admin and support", "eurostat", "HW_EMP", "nama_10_lp_a21", "na_item", "A", "DE", "HW", NA, "N", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("HousePriceIndex", "House Price Index", "eurostat", "TOTAL", "prc_hpi_a", "purchase", "A", "DE", "I15_A_AVG", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("LabourCostsEconomy", "Economy wide labour cost industry, construction and services (except pub admin, defense, compulsory social security)", "eurostat", "D1_D4_MD5", "lc_lci_lev", "lcstruct", "A", "DE", "EUR", NA, "B-S_X_O", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("BuildingPermits", "New Residential Building Permits Square Metre Useful Floor Area", "eurostat", "BPRM_SQM", "sts_cobp_a", "indic_bt", "A", "DE", "MIO_M2", "NSA", NA, NA, "CPA_F41001", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%

  # quarterly data (variable names re-used, so need to filter on frequency when using it in practice)
  add_dict_entry("Demand", "Total Demand", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("GDPOutput", "GDP Output Approach", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("GDPExpenditure", "GDP Expenditure Approach", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("Taxes", "Product Taxes", "eurostat", "D21REC", "gov_10q_ggnfa", "na_item", "Q", "DE", "MIO_EUR", "NSA", NA, NA, NA, NA, "S13", NA, NA, NA, NA) %>%
  add_dict_entry("Subsidies", "Product Subsidies", "eurostat", "D31PAY", "gov_10q_ggnfa", "na_item", "Q", "DE", "MIO_EUR", "NSA", NA, NA, NA, NA, "S13", NA, NA, NA, NA) %>%
  add_dict_entry("VAAgriculture", "Gross Value Added Agriculture", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "A", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAIndustry", "Gross Value Added Industry except Construction", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "B-E", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAConstruction", "Gross Value Added Construction", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "F", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VARetail", "Gross Value Added Wholesail and Retail Trade, Transportation and Storage, Accommodation and Food Service Activities", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "G-I", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAInformation", "Gross Value Added Information and Communication", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "J", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAFinance", "Gross Value Added Financial and Insurance Activities", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "K", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VARealEstate", "Gross Value Added Real Estate Services", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "L", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAService", "Gross Value Added Professional, Scientific, Technical, Administration and Support Service Activities", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "M_N", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAGov", "Gross Value Added Public Administration, Defence, Education, Human Health and Social Work Activities", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "O-Q", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("VAOther", "Gross Value Added Other Services", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "CP_MEUR", "NSA", "R-U", NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ConsHH", "Household and NPISH Final Consumption Expenditure", "eurostat", "P31_S14_S15", "namq_10_gdp", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ConsGov", "Government Final Consumption Expenditure", "eurostat", "P3_S13", "namq_10_gdp", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("Exports", "Exports of Goods and Services", "eurostat", "P6", "namq_10_gdp", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("Imports", "Imports of Goods and Services", "eurostat", "P7", "namq_10_gdp", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("REER", "Real Effective Exchange Rate", "eurostat", "REER_IC42_CPI", "ert_eff_ic_q", "exch_rt", "Q", "DE", "I15", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("IncomeHH", "Gross Disposable Income Households and Non-Profit Institutions Serving Households in Mio EUR", "eurostat", "B6G", "nasq_10_nf_tr", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, "S14_S15", NA, NA, "RECV", NA) %>%
  add_dict_entry("CapFormHH", "Gross Fixed Capital Formation Households", "eurostat", "P51G", "nasq_10_nf_tr", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, "S14_S15", NA, NA, "PAID", NA) %>%
  add_dict_entry("CapFormGov", "Gross Fixed Capital Formation Government", "eurostat", "P51G", "nasq_10_nf_tr", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, "S13", NA, NA, "PAID", NA) %>%
  add_dict_entry("CapFormFirm", "Gross Fixed Capital Non-Financial Corporations", "eurostat", "P51G", "nasq_10_nf_tr", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, "S11", NA, NA, "PAID", NA) %>%
  add_dict_entry("CapFormFinFirm", "Gross Fixed Capital Financial Corporations", "eurostat", "P51G", "nasq_10_nf_tr", "na_item", "Q", "DE", "CP_MEUR", "NSA", NA, NA, NA, NA, "S12", NA, NA, "PAID", NA) %>%
  add_dict_entry("FinWealthHH", "Household Financial Net Worth in Mio EUR", "eurostat", "BF90", "nasq_10_f_bs", "na_item", "Q", "DE", "MIO_EUR", NA, NA, NA, NA, NA, "S14_S15", NA, NA, NA, NA, NA, NA, NA, NA, "LIAB") %>% # caution: unlike annual data, this only exists non-consolidated
  add_dict_entry("HousePriceIndex", "House Price Index", "eurostat", "TOTAL", "prc_hpi_q", "purchase", "Q", "DE", "I15_Q", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("BuildingPermits", "New Residential Building Permits Square Metre Useful Floor Area", "eurostat", "BPRM_SQM", "sts_cobp_q", "indic_bt", "Q", "DE", "I15", "NSA", NA, NA, "CPA_F41001", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%  # is index now compared to annual data, not area
  add_dict_entry("DFVAAgriculture", "Deflator VAAgriculture", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "A", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAIndustry", "Deflator VAIndustry", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "B-E", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAConstruction", "Deflator VAConstruction", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "F", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVARetail", "Deflator VARetail", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "G-I", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAInformation", "Deflator VAInformation", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "J", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAFinance", "Deflator VAFinance", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "K", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVARealEstate", "Deflator VARealEstate", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "L", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAService", "Deflator VAService", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "M_N", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAGov", "Deflator VAGov", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "O-Q", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFVAOther", "Deflator VAOther", "eurostat", "B1G", "namq_10_a10", "na_item", "Q", "DE", "PD15_EUR", "NSA", "R-U", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFExports", "DF Exports", "eurostat", "P6", "namq_10_gdp", "na_item", "Q", "DE", "PD15_EUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFImports", "DF Imports", "eurostat", "P7", "namq_10_gdp", "na_item", "Q", "DE", "PD15_EUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFTaxesLessSubsidies", "Deflator TaxesLessSubsidies", "eurostat", "D21X31", "namq_10_gdp", "na_item", "Q", "DE", "PD15_EUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFConsHH", "Deflator ConsHH", "eurostat", "P31_S14_S15", "namq_10_gdp", "na_item", "Q", "DE", "PD15_EUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFCapForm", "Deflator CapForm", "eurostat", "P51G", "namq_10_gdp", "na_item", "Q", "DE", "PD15_EUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("DFGDP", "Deflator GDP", "eurostat", "B1GQ", "namq_10_gdp", "na_item", "Q", "DE", "PD15_EUR", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("URate", "Total Unemployment Rate Labour Force", "eurostat", "T", "une_rt_q", "sex", "Q", "DE", "PC_ACT", "NSA", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Y15-74", NA, NA)


# adding in additional variables for inflation

dictionary <- dictionary %>%
  add_dict_entry("LabourProd", "Real Labour Productivity per Person", "eurostat", "RLPR_PER", "namq_10_lp_ulc", "na_item", "Q", "DE",
                 unit = "I15", s_adj = "NSA", nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
                 tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA,
                 partner = NA, finpos = NA) %>%
  add_dict_entry("EfExchange", "Effective Exchange Rate", "eurostat", "NEER_IC42", "ert_eff_ic_q", "exch_rt", "Q", "DE",
unit = "I15", s_adj = NA, nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA,
partner = NA, finpos = NA) %>%
  add_dict_entry("LabourCI", "Labour Cost Index", "eurostat", "D1_D4_MD5", "teilm140", "lcstruct", "Q", "DE",
                 unit = "I20", s_adj = "SCA", nace_r2 = "B-S", ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA,
                 tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA,
                 partner = NA, finpos = NA)
#end of adding inflation variables

# Changed by Moritz -------------------------------------------------------------

jonas_quarterly <- structure(list(model_varname = c("Supply", "Demand", "GDPOutput",
                                 "GDPExpenditure", "Taxes", "Subsidies", "TaxesLessSubsidies", "VAAgriculture", "VAIndustry",
                                 "VAConstruction", "VARetail", "VAInformation", "VAFinance", "VARealEstate",
                                 "VAService", "VAGov", "VAOther", "ConsHH", "ConsGov", "Exports",
                                 "Imports", "REER", "IncomeHH", "CapFormHH", "CapFormGov", "CapFormFirm",
                                 "CapFormFinFirm", "FinWealthHH", "HousePriceIndex", "BuildingPermits",
                                 "HCPI", "PriceETS", "PriceOil"),
               full_name = c("Total Supply",
                             "Total Demand", "GDP Output Approach", "GDP Expenditure Approach",
                             "Product Taxes", "Product Subsidies", "Product Taxes Less Product Subsidies", "Gross Value Added Agriculture",
                             "Gross Value Added Industry except Construction", "Gross Value Added Construction",
                             "Gross Value Added Wholesail and Retail Trade, Transportation and Storage, Accommodation and Food Service Activities",
                             "Gross Value Added Information and Communication", "Gross Value Added Financial and Insurance Activities",
                             "Gross Value Added Real Estate Services", "Gross Value Added Professional, Scientific, Technical, Administration and Support Service Activities",
                             "Gross Value Added Public Administration, Defence, Education, Human Health and Social Work Activities",
                             "Gross Value Added Other Services", "Household and NPISH Final Consumption Expenditure",
                             "Government Final Consumption Expenditure", "Exports of Goods and Services",
                             "Imports of Goods and Services", "Real Effective Exchange Rate",
                             "Gross Disposable Income Households and Non-Profit Institutions Serving Households in Mio EUR",
                             "Gross Fixed Capital Formation Households", "Gross Fixed Capital Formation Government",
                             "Gross Fixed Capital Non-Financial Corporations", "Gross Fixed Capital Financial Corporations",
                             "Household Financial Net Worth in Mio EUR", "House Price Index",
                             "New Residential Building Permits Square Metre Useful Floor Area",
                             "Harmonised Consumer Price Index All Goods", "Average Annual EU ETS Price, Nominal",
                             "Average Crude Brent Oil Price in USD, Nominal"),
               database = c(NA,
                            NA, NA, NA, "eurostat", "eurostat", "eurostat", "eurostat", "eurostat",
                            "eurostat", "eurostat", "eurostat", "eurostat", "eurostat", "eurostat",
                            "eurostat", "eurostat", "eurostat", "eurostat", "eurostat", "eurostat",
                            "eurostat", "eurostat", "eurostat", "eurostat", "eurostat", "eurostat",
                            "eurostat", "eurostat", "eurostat", "local", "local", "local"),
               variable_code = c(NA,
                                 NA, NA, NA, "D21REC", "D31PAY", "D21X31", "B1G", "B1G", "B1G", "B1G", "B1G",
                                 "B1G", "B1G", "B1G", "B1G", "B1G", "P31_S14_S15", "P3_S13", "P6",
                                 "P7", "REER_IC42_CPI", "B6G", "P51G", "P51G", "P51G", "P51G",
                                 "BF90", "TOTAL", "BPRM_SQM", NA, "PriceETS", "PriceOil"),
               dataset_id = c(NA,
                              NA, NA, NA, "gov_10q_ggnfa", "gov_10q_ggnfa", "namq_10_gdp", "namq_10_a10",
                              "namq_10_a10", "namq_10_a10", "namq_10_a10", "namq_10_a10", "namq_10_a10",
                              "namq_10_a10", "namq_10_a10", "namq_10_a10", "namq_10_a10", "namq_10_gdp",
                              "namq_10_gdp", "namq_10_gdp", "namq_10_gdp", "ert_eff_ic_q",
                              "nasq_10_nf_tr", "nasq_10_nf_tr", "nasq_10_nf_tr", "nasq_10_nf_tr",
                              "nasq_10_nf_tr", "nasq_10_f_bs", "prc_hpi_q", "sts_cobp_q", NA,
                              NA, NA),
               var_col = c(NA, NA, NA, NA, "na_item", "na_item", "na_item", "na_item",
                           "na_item", "na_item", "na_item", "na_item", "na_item", "na_item",
                           "na_item", "na_item", "na_item", "na_item", "na_item", "na_item",
                           "na_item", "exch_rt", "na_item", "na_item", "na_item", "na_item",
                           "na_item", "na_item", "purchase", "indic_bt", NA, "na_item",
                           "na_item"),
               freq = c(NA, NA, NA, NA, "Q", "Q", "Q", "Q", "Q", "Q",
                        "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q", "Q",
                        "Q", "Q", "Q", "Q", "Q", "Q", "Q", NA, NA, NA),
               geo = c(NA, NA,
                       NA, NA, "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE",
                       "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE",
                       "DE", "DE", "DE", "DE", "DE", NA, "DE", "DE"),
               unit = c(NA, NA,
                        NA, NA, "MIO_EUR", "MIO_EUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR",
                        "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR",
                        "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "I15",
                        "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "CP_MEUR", "MIO_EUR",
                        "I15_Q", "I15", NA, NA, NA),
               s_adj = c(NA, NA, NA, NA, "NSA", "NSA",
                         "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", "NSA",
                         "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", NA, "NSA", "NSA", "NSA",
                         "NSA", "NSA", NA, NA, "NSA", NA, NA, NA),
               nace_r2 = c(NA, NA,
                           NA, NA, NA, NA, NA, "A", "B-E", "F", "G-I", "J", "K", "L", "M_N",
                           "O-Q", "R-U", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA, NA, NA, NA),
               ipcc_sector = c(NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                               NA, NA, NA, NA, NA, NA, NA, NA, NA),
               cpa2_1 = c(NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, "CPA_F41001", NA, NA, NA),
               siec = c(NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
               sector = c(NA, NA, NA, NA, "S13", "S13", NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "S14_S15", "S14_S15",
                          "S13", "S11", "S12", "S14_S15", NA, NA, NA, NA, NA),
               tax = c(NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA),
               currency = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                            NA, NA, NA, NA, NA, NA, NA),
               direct = c(NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, "RECV", "PAID", "PAID", "PAID", "PAID", NA, NA, NA, NA,
                          NA, NA), co_nco = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                              NA, NA, NA, NA, NA, NA, NA),
               train = c(NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                         NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
               vehicle = c(NA,
                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA), age = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                                        NA, NA, NA, NA, NA, NA),
               partner = c(NA, NA, NA, NA, NA, NA,
                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
               finpos = c(NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "LIAB", NA, NA, NA,
                          NA, NA)), row.names = c(NA, -33L), class = c("tbl_df", "tbl",
                                                                       "data.frame"))


# https://github.com/moritzpschwarz/osem/issues/47
from_issues_47 <- structure(list(
  model_varname = c("MetalProd", "IndProd", "PlasticProd",
                    "EmiN2OSoils", "Price_AgriProducts", "Price_AnimalProducts",
                    "EmiN2OManure_indirect", "Flights", "ConstructionManuf", "EmiCO2Aviation",
                    "EmiCO2RoadsBio", "EmiCO2Manuf", "EmiCO2Residential", "ConstructionBuildings",
                    "Lab_Inp_Constr", "Lab_Inp_Indus", "ElectrCons", "AnimalImports",
                    "TurnoverInd", "PPI", "Unemp", "PT_Emp", "EmpCompensation", "ExchangeRate",
                    "GDP_Deflator", "LabCost_ICS"),
  full_name = c("Index of Manufacture of basic metals and fabricated metal products",
                "Index of Industrial Production", "Index of Plastic Production",
                "Direct N2O Emissions from managed soils", "Price indices of agricultural products, output",
                "Price indices of agricultural products - Agricultural goods output",
                "Indirect N2O Emissions from manure management", "National air passenger transport by reporting country",
                "Production in Construction in Manufacturing Sectors", "CO2 (fossil) emissions from Civil Aviation",
                "CO2 (bio) emissions from Road", "CO2 (fossil) emissions from Manufacturing Industries and Construction",
                "CO2 (fossil) emissions from Residential and other sectors",
                "Production in Construction in the Building sector", "Labour input in construction",
                "Labour input in industry", "Consumption of electricity - GWh",
                "Total imports of live animals", "Turnover in industry, domestic market - monthly data",
                "Output prices of the domestic market index (Producer price index) (NSA)",
                "Unemployment", "Part-time employment and temporary contracts",
                "Compensation of employees", "Nominal effective exchange rate - 42 trading partners (industrial countries)",
                "GDP deflator", "labour cost index in Industry, construction and services "
  ),
  database = c("eurostat", "eurostat", "eurostat", "edgar",
               "eurostat", "eurostat", "edgar", "eurostat", "eurostat", "edgar",
               "edgar", "edgar", "edgar", "eurostat", "eurostat", "eurostat",
               "eurostat", "eurostat", "eurostat", "eurostat", "eurostat", "eurostat",
               "eurostat", "eurostat", "eurostat", "eurostat"),
  variable_code = c("PROD",
                    "PROD", "PROD", NA, "140000", "140000", NA, "CAF_PAS", "PRD",
                    NA, NA, NA, NA, "PROD", "EMPL", "EMPL", "IS-CEL-GWH", "SL", "TOVD",
                    "IS-PPI", "T", "T", "D1", "NEER_IC42", "B1GQ", "LM-LCI-TOT"),
  dataset_id = c("sts_inpr_q", "sts_inpr_q", "sts_inpr_q",
                 "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022b.zip",
                 "apri_pi20_outq", "apri_pi15_outq", "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022b.zip",
                 "avia_panc", "sts_copr_m", "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
                 "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
                 "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
                 "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
                 "sts_copr_m", "sts_colb_m", "sts_inlb_m", "ei_isen_m", "apro_mt_pheadm",
                 "sts_intvd_m", "ei_isir_m", "lfsq_urgan", "lfsi_pt_q", "tipslm14",
                 "ert_eff_ic_q", "namq_10_gdp", "ei_lmlc_q"),
  var_col = c("indic_bt",
              "indic_bt", "indic_bt", NA, "product", "product", NA, "tra_meas",
              "indic_bt", NA, NA, NA, NA, "indic_bt", "indic_bt", "indic_bt",
              "indic", "meatitem", "indic_bt", "indic", "sex", "sex", "na_item",
              "exch_rt", "na_item", "indic"), freq = c("q", "q", "q", "m",
                                                       "q", "q", "m", "m", "m", "m", "m", "m", "m", "m", "m", "m",
                                                       "m", "m", "m", "m", "q", "q", "q", "q", "q", "q"),
  unit = c("I15",
           "I15", "I15", NA, "I20", "I15", NA, "FLIGHT", "I21", NA,
           NA, NA, NA, "I21", "I21", "I21", "NA", "THS_T", "I21", "RT1",
           "PC", "PC_EMP", "CP_MNAC", "I15", "PD10_EUR", "I20"),
  s_adj = c("NSA",
            "NSA", "NSA", NA, "NSA", NA, NA, NA, "NSA", NA, NA, NA, NA,
            "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", "NSA", "NSA",
            "NSA", "NSA", "NSA", "NSA"), nace_r2 = c("C24_C25", "B-D",
                                                     "C222", NA, "B-D", NA, NA, NA, "B-D_F", NA, NA, NA, NA, "F41",
                                                     "F", "B-E36", "B-D", "B-D", "B_C", "B-E36", NA, " ", NA,
                                                     NA, NA, "B-S"),
  ipcc_sector = c(NA, NA, NA, "3.C.4", NA,
                  NA, "3.C.6", NA, NA, "1.A.3.a", "1.A.3.b_noRES", "1.A.2",
                  "1.A.4", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                  NA), geo = c(NA, NA, NA, "AT", "AT", "AT", "AT", "AT", "AT",
                               "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
                               "AT", "AT", "AT", "AT", "AT", "AT", "AT"),
  p_adj = c(NA,
            NA, NA, NA, NA, "NI", NA, NA, NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  meat = c(NA,
           NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           NA, "B1000", NA, NA, NA, NA, NA, NA, NA, NA),
  age = c(NA,
          NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
          NA, NA, NA, NA, "Y20-64", "Y20-64", NA, NA, NA, NA),
  citizen = c(NA,
              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
              NA, NA, NA, NA, "TOTAL", NA, NA, NA, NA, NA),
  wstatus = c(NA,
              NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
              NA, NA, NA, NA, NA, "EMP_PT", NA, NA, NA, NA)),
  class = c("tbl_df",
            "tbl", "data.frame"), row.names = c(NA, -26L)) %>%

  mutate(across(where(is.character), ~na_if(., "NA"))) %>%
  filter(!model_varname %in% c("IndProd", "PT_Emp", "LabCost_ICS"))



# from EMCC presentation
from_EMCC <- read_csv("small_examples/illustration de/moritz_dict.csv", na = "NA") %>%
  filter(!model_varname %in% c("BuildingPermits"))

from_EMCC %>% filter(model_varname %in% c("BuildingPermits")) %>% pull(full_name)
from_issues_47 %>% filter(model_varname %in% c("BuildingPermits"))


# Merge all ---------------------------------------------------------------

dictionary_intermed <- dictionary %>%
  filter(!model_varname %in% c("FinWealthHH")) %>%
  select(-c("finpos", "co_nco")) %>%

  bind_rows(jonas_quarterly %>%
              mutate(freq = case_when(model_varname == "HCPI" ~ "q",TRUE ~ freq))) %>%

  bind_rows(from_issues_47) %>%
  bind_rows(from_EMCC)

dictionary_intermed %>%
  distinct %>%

  mutate(n = n(), .by = model_varname) %>%
  mutate(freq = tolower(freq)) %>%

  # removing all annual data when there is more than one available
  # this resolves all duplicates
  filter(n == 1 | (n > 1 & freq != "a")) -> dictionary_done

# check for remainig duplicates (should be empty)
dictionary_done %>%
  mutate(n = n(), .by = model_varname) %>%
  filter(n > 1)


# separate out the eurostat and the edgar data

dict_eurostat <- dictionary_done %>% filter(database == "eurostat")
dict_identities <- dictionary_done %>% filter(is.na(database))
dict_edgar <- dictionary_done %>% filter(database == "edgar")
dict_local <- dictionary_done %>% filter(database == "local")

usethis::use_data(dict_eurostat, overwrite = TRUE)
usethis::use_data(dict_identities, overwrite = TRUE)
usethis::use_data(dict_edgar, overwrite = TRUE)

# dict <- dplyr::bind_rows(dict_identities, dict_eurostat, dict_edgar, dict_imf, dict_statcan)
# usethis::use_data(dict, overwrite = TRUE)

