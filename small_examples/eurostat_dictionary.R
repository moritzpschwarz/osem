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

dictionary <- tibble(model_varname = "Supply", full_name = "Total Supply", database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, geo = NA, unit = NA, s_adj = NA, nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA, tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA, partner = NA)

dictionary <- dictionary %>%
  # annual data
  add_dict_entry("Demand", "Total Demand", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("GDPOutput", "GDP Output Approach", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("GDPExpenditure", "GDP Expenditure Approach", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("Taxes", "Product Taxes", "eurostat", "D21REC", "gov_10a_main", "na_item", "A", "DE", "MIO_EUR", NA, NA, NA, NA, NA, "S13", NA, NA, NA, NA) %>%
  add_dict_entry("Subsidies", "Product Subsidies", "eurostat", "D31PAY", "gov_10a_main", "na_item", "A", "DE", "MIO_EUR", NA, NA, NA, NA, NA, "S13", NA, NA, NA, NA) %>%
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
  add_dict_entry("BuildingPermits", "New Residential Building Permits Square Metre Useful Floor Area", "eurostat", "BPRM_SQM", "sts_cobp_a", "indic_bt", "A", "DE", "MIO_M2", "NSA", NA, NA, "CPA_F41001", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
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
  add_dict_entry("BuildingPermits", "New Residential Building Permits Square Metre Useful Floor Area", "eurostat", "BPRM_SQM", "sts_cobp_q", "indic_bt", "Q", "DE", "I15", "NSA", NA, NA, "CPA_F41001", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) # is index now compared to annual data, not area


