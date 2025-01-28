devtools::load_all()

#devtools::install_github("moritzpschwarz/osem", ref = "mixed_freq_bug")
library(osem)
library(tidyverse)

# economic specification
spec_econ <- tibble(type = c("d", "d"), dependent = c("Supply", "Demand"), independent = c("GDPOutput + Imports", "GDPExpenditure + Imports")) %>%
  add_row(type = "d", dependent = "GDPOutput", independent = "VA + TaxesLessSubsidies") %>%
  add_row(type = "d", dependent = "VA", independent = "VAAgriculture + VAIndustry + VAConstruction + VARetail + VAInformation + VAFinance + VARealEstate + VAService + VAGov + VAOther") %>%
  add_row(type = "n", dependent = "VAIndustry", independent = "ConsHH + ConsGov + CapForm + PriceOil + PriceETS") %>%
  add_row(type = "n", dependent = "VARetail", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARealEstate", independent = "ConsHH + ConsGov + CapForm + HousePriceIndex") %>%
  add_row(type = "n", dependent = "VAService", independent = "ConsHH + ConsGov") %>%
  add_row(type = "n", dependent = "VAGov", independent = "ConsHH + ConsGov") %>%
  add_row(type = "n", dependent = "Imports", independent = "REER") %>%
  add_row(type = "d", dependent = "GDPExpenditure", independent = "ConsHH + ConsGov + CapForm + DInventories + Exports - Imports") %>%
  add_row(type = "d", dependent = "DInventories", independent = "Supply - ConsHH - ConsGov - CapForm - Exports") %>%
  add_row(type = "n", dependent = "ConsHH", independent = "IncomeHH + HICPlocal + FinWealthHH") %>%
  add_row(type = "d", dependent = "CapForm", independent = "CapFormHH + CapFormFirm + CapFormFinFirm + CapFormGov") %>%
  add_row(type = "n", dependent = "CapFormHH", independent = "IncomeHH + HICPlocal + HousePriceIndex + BuildingPermits") %>%
  add_row(type = "n", dependent = "Exports", independent = "REER") %>%
  add_row(type = "d", dependent = "RealVAAgriculture", independent = "VAAgriculture / DFVAAgriculture * Factor") %>%
  add_row(type = "d", dependent = "RealVAIndustry", independent = "VAIndustry / DFVAIndustry * Factor") %>%
  add_row(type = "d", dependent = "RealVAConstruction", independent = "VAConstruction / DFVAConstruction * Factor") %>%
  add_row(type = "d", dependent = "RealVARetail", independent = "VARetail / DFVARetail * Factor") %>%
  add_row(type = "d", dependent = "RealVAInformation", independent = "VAInformation / DFVAInformation * Factor") %>%
  add_row(type = "d", dependent = "RealVAFinance", independent = "VAFinance / DFVAFinance * Factor") %>%
  add_row(type = "d", dependent = "RealVARealEstate", independent = "VARealEstate / DFVARealEstate * Factor") %>%
  add_row(type = "d", dependent = "RealVAService", independent = "VAService / DFVAService * Factor") %>%
  add_row(type = "d", dependent = "RealVAGov", independent = "VAGov / DFVAGov * Factor") %>%
  add_row(type = "d", dependent = "RealVAOther", independent = "VAOther / DFVAOther * Factor") %>%
  add_row(type = "d", dependent = "RealCapForm", independent = "CapForm / DFCapForm * Factor") %>%
  add_row(type = "d", dependent = "RealCapFormHH", independent = "CapFormHH / DFCapForm * Factor") %>% # cannot find deflator specific for households, use general deflator for  Gross Fixed Capital Formation (better to use house prices?)
  add_row(type = "d", dependent = "RealConsHH", independent = "ConsHH / DFConsHH * Factor") %>%
  # can explicitly model deflators if want to (example GDP deflator)
  add_row(type = "n", dependent = "DFGDP", independent = "") %>%
  # add simple Phillips curve
  add_row(type = "n", dependent = "Inflation", independent = "URate + PriceOil")


spec_envi <- tibble(
  type = "n", dependent = "EmiCO2ManInd", independent = "RealVAIndustry + HICPlocal") %>%

  add_row(type = "n", dependent = "Flights", independent = "RealConsHH + FinWealthHH +  HICP_AviaInt") %>%
  add_row(type = "n", dependent = "EmiCO2CivAvi", independent = "HICPlocal + RealConsHH + RealVAService") %>%
  add_row(type = "n", dependent = "RoadFreight", independent = "RealConsHH + HICPlocal + RealVAService") %>%
  add_row(type = "n", dependent = "EmiCO2RoaTra", independent = "RoadFreight + RealConsHH + HICPlocal") %>%
  add_row(type = "n", dependent = "EmiCO2Residential", independent = "RealConsHH + HICP_Electricity") %>%
  add_row(type = "n", dependent = "ElectrCons", independent = "RealConsHH + HICP_Electricity") %>%
  add_row(type = "n", dependent = "EmiCO2ElecHeat", independent = "ElectrCons + RealVAIndustry + RealCapFormHH") %>%
  add_row(type = "d", dependent = "EmiCO2Total", independent = "EmiCO2ElecHeat + EmiCO2RoaTra + EmiCO2ManInd + EmiCO2OilandGas + EmiCO2PetrRef + EmiCO2WatNav + EmiCO2CivAvi + EmiCO2GlassProd + EmiCO2LimeProd + EmiCO2OthTransp + EmiCO2Rail + EmiCO2Residential + EmiCO2SolidFuel + EmiCO2OthCarb + EmiCO2CemPro + EmiCO2ChemInd + EmiCO2Liming + EmiCO2MetalInd + EmiCO2NonEnergProd + EmiCO2UreaApp + EmiCO2Waste") %>%
  add_row(type = "d", dependent = "EmiCH4Total", independent = "EmiCH4ElecHeat + EmiCH4PetrRef + EmiCH4ManInd + EmiCH4CivAvi + EmiCH4RoaTra + EmiCH4Rail + EmiCH4WatNav + EmiCH4OthTransp + EmiCH4Residential + EmiCH4SolidFuel + EmiCH4OilandGas + EmiCH4ChemInd + EmiCH4MetalInd + EmiCH4EntericFerment + EmiCH4ManureMgmt + EmiCH4BiomassBurn + EmiCH4SolidWaste + EmiCH4BioWasteTreat + EmiCH4Waste + EmiCH4Wastewater") %>%
  add_row(type = "d", dependent = "EmiCH4Livestock", independent = "EmiCH4EntericFerment + EmiCH4ManureMgmt") %>%
  add_row(type = "n", dependent = "EmiCH4EntericFerment", independent = "Slaughter + RealConsHH + RealVAAgriculture")

spec_extended <- spec_econ %>% bind_rows(spec_envi)


# dictionary
dict_local <- structure(
  list(
    model_varname = c("PriceETS", "PriceOil", "HICPlocal", "Factor", "Inflation"),
    full_name = c(
      "Average EU ETS Price, Nominal",
      "Average Crude Brent Oil Price in EUR, Nominal",
      "Harmonised Index of Consumer Prices, All Goods",
      "100",
      "Quarter-on-Quarter HICP Inflation"
    ),
    database = c("local", "local", "local", "local", "local"),
    variable_code = c("PriceETS", "PriceOil", "HICPlocal", "Factor", "Inflation"),
    var_col = c("na_item", "na_item", "na_item", "na_item", "na_item"),
    freq = c(NA, NA, NA, NA, NA),
    geo = c("DE", "DE", "DE", "DE", "DE"),
    n = c(1L, 1L, 1L, 1L, 1L)
  ),
  row.names = c(NA, -5L),
  class = c("tbl_df", "tbl", "data.frame")
)


dictionary <- dplyr::bind_rows(dict_identities,
                               dict_eurostat,
                               dict_edgar,
                               dict_local)

dictionary <- dplyr::bind_rows(dict_identities, dict_eurostat, dict_edgar, dict_local)


# Full cycle for all countries ------------------------------------------------

for(country in c("DE","AT","FR", "DK")){

  if(file.exists(paste0("./small_examples/IJF/", country, "/model.RData"))){next}

  # country = "DE"

  # jobs to do:
  # add equation from Felix
  # add environmental equations --> DONE
  # transfer to other countries --> DONE
  # change to EDGAR v9 --> DONE
  # change environmental variables to real values --> DONE
  # fix Flights and Inflation

  # create data ---------------

  # ets data that has higher frequency than annual (but intervals irregular, unsure how to aggregate -> mean potentially misleading)
  ets <- read_csv(file = "./small_examples/IJF/general_data/international_carbon_action_partnership.csv", skip = 1) %>%
    select(Date, `Secondary Market`) %>%
    rename(time = Date) %>%
    rename(PriceETS = `Secondary Market`)
  ets <- ets %>%
    mutate(year = year(time),
           quarter = quarter(time))
  ets <- ets %>%
    group_by(year, quarter) %>%
    summarise(PriceETS = mean(PriceETS, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(month = case_when(quarter == 1 ~ "01",
                             quarter == 2 ~ "04",
                             quarter == 3 ~ "07",
                             quarter == 4 ~ "10"),
           time = as.Date(paste0(year, "-", month, "-01"))) %>%
    select(time, PriceETS) %>%
    rename(values = PriceETS) %>%
    mutate(geo = country,
           na_item = "PriceETS") %>%
    arrange(time)
  write_csv(ets, file = paste0("./small_examples/IJF/",country,"/ets.csv"))

  # quarterly Brent oil prices in USD -> later convert to EUR
  oil <- read_csv("small_examples/IJF/general_data/fred_brent_oil_prices.csv") %>%
    rename(time = observation_date) %>%
    rename(PriceOilUSD = DCOILBRENTEU)
  # exchange rate USD/EUR to convert
  xrt <- eurostat::get_eurostat("ert_bil_eur_q", filters = list(freq = "Q", unit = "NAC", statinfo = "AVG", currency = "USD")) %>%
    select(time, values) %>%
    rename(USDEUR = values)
  oil <- inner_join(x = oil, y = xrt, by = "time")
  oil <- oil %>%
    mutate(PriceOil = PriceOilUSD / USDEUR) %>%
    select(time, PriceOil) %>%
    rename(values = PriceOil) %>%
    mutate(geo = country,
           na_item = "PriceOil") %>%
    arrange(time)
  write_csv(oil, file = paste0("./small_examples/IJF/",country,"/oil.csv"))

  # factor when converting nominal to real (otherwise "100" is interpreted as a variable name)
  fct <- tibble(time = seq(as.Date("1980-01-01"), as.Date("2024-12-31"), by = "quarter"),
                geo = country,
                na_item = "Factor",
                values = 100)
  write_csv(fct, file = paste0("./small_examples/IJF/",country,"/fct.csv"))

  # hicp
  hicp <- eurostat::get_eurostat("prc_hicp_midx", filters = list(geo = country, freq = "M", coicop = "CP00", unit = "I15")) %>%
    select(time, values) %>%
    mutate(quarter = quarter(time),
           year = year(time)) %>%
    group_by(year, quarter) %>%
    summarise(values = mean(values)) %>%
    ungroup() %>%
    mutate(month = case_when(quarter == 1L ~ "01",
                             quarter == 2L ~ "04",
                             quarter == 3L ~ "07",
                             quarter == 4L ~ "10"),
           time = as.Date(paste0(year, "-", month, "-01"))) %>%
    select(time, values) %>%
    mutate(geo = country,
           na_item = "HICPlocal") %>%
    arrange(time)
  write_csv(hicp, file = paste0("./small_examples/IJF/",country,"/hicp.csv"))

  # inflation, ideally we calculate this inside the model from HICP level data: (HICP - L1.HICP) / L1.HICP
  # but I think we have not implemented more general algebra at the moment
  inf <- hicp %>%
    pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
    mutate(L1HICPlocal = lag(HICPlocal, 1)) %>%
    mutate(Inflation = (HICPlocal - L1HICPlocal) / L1HICPlocal * 100) %>%
    select(time, Inflation) %>%
    drop_na(Inflation) %>%
    rename(values = Inflation) %>%
    mutate(geo = country,
           na_item = "Inflation") %>%
    arrange(time)
  write_csv(inf, file = paste0("./small_examples/IJF/",country,"/inflation.csv"))

  cap_form_manual <- eurostat::get_eurostat("nasq_10_nf_tr") %>%
    filter(geo == country) %>%
    filter(unit == "CP_MEUR", na_item == "P51G", s_adj == "NSA")

  capform_total <- cap_form_manual %>%
    filter(sector == "S1")

  capform_remainder <- cap_form_manual %>%
    filter(sector != "S1") %>%
    summarise(remainder = sum(values), .by = "TIME_PERIOD")

  capform_total %>%
    full_join(capform_remainder,by = "TIME_PERIOD") %>%
    mutate(CapFormGov = values - remainder) %>%
    select(time = TIME_PERIOD, CapFormGov) %>%
    pivot_longer(-time, names_to = "na_item", values_to = "values") %>%

    write_csv(file = paste0("./small_examples/IJF/", country, "/capformgov.csv"))

  # Run the model -----------------------------------------------------------

  model_result_ext <- run_model(
    specification = spec_extended,
    dictionary = dictionary %>%
      mutate(geo = country),
    inputdata_directory = paste0("./small_examples/IJF/", country),
    save_to_disk = paste0("./small_examples/IJF/", country, "/data.csv"),
    primary_source = "local",
    trend = TRUE,
    saturation.tpval = 0.01,
    gets_selection = FALSE,
    constrain.to.minimum.sample = FALSE,
    plot = FALSE
  )

  save(model_result_ext, file = paste0("./small_examples/IJF/", country, "/model.RData"))

  #fc_ext <- forecast_model(model_result_ext, exog_fill_method = "auto")
}





# to do after call
# EU ETS to ElectrCons
# Policy variable for EU ETS
# EmiCO2Road ManuInd ElecHeat
# ElectrCons
# Email to all: enviro equations and ask to look at different combinations --> Moritz --> DONE
# add methane --> Moritz --> DONE
# solve asinh --> Moritz --> DONE
# start presentation --> Moritz

# modelling real --> Jonas
# think about inflation --> Jonas



# go through the document and accept changes --> Moritz --> DONE
