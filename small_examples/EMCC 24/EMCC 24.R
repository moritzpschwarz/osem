library(tidyverse)
library(kableExtra)
library(ggtext)
library(modelsummary)
devtools::load_all()


#
#   Share of Fuel Costs in Flight Costs -> Elasticity of Demand for Flight prices -> Flights -> CO2 Emissions Aviation
#
#
# Diesel CO2 content -> Diesel Price -> Transport -> CO2 Emissions Transport
#
# Elasticity for Meat Prices -> Slaughterings -> CO2 Emissions Agriculture
#
# CBAM -> shock to
#
#
#
#
# Share of Fuel Costs in Flight Costs -> cost of international travel -> flights -> CO2 emissions aviation
#
# Diesel Price + Petrol Price -> Transport -> CO2 Emissions Transport
#
# Elasticity for Meat Prices -> Slaughterings -> CO2 Emissions Agriculture
#
# CBAM -> shock to
#
#
# Export -> Industry -> Industry Emissions

# Dict

## New Dict


tibble() %>%

  bind_rows(tibble(
    model_varname = "MetalProd",
    full_name = "Index of Manufacture of basic metals and fabricated metal products",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt",
    freq = "q",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "C24_C25")) %>%

  bind_rows(tibble(
    model_varname = "IndProd", # this is free to choose but must be unique
    full_name = "An index of Industrial Production",
    database  = "eurostat",
    variable_code = "PRD", # in this case use the bt_indicator information here
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt", # here we specify what the column with the variables is called
    freq = "q", # for quarterly data, 'm' would be monthly
    geo = "AT",
    unit = "I15", # for index of 2015 = 100
    s_adj = "NSA", # not seasonally adjusted
    nace_r2 = "B-D")) %>%


  bind_rows(tibble(
    model_varname = "PlasticProd",
    full_name = "Index of Plastic Production",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_inpr_q",
    var_col = "indic_bt",
    freq = "q",
    unit = "I15",
    s_adj = "NSA",
    nace_r2 = "C222")) %>%




  bind_rows(tibble(
    model_varname = "EmiN2OSoils", #choose a name for the variable that is unique
    full_name = "Direct N2O Emissions from managed soils", #Enter the full name/describe the variable
    database  = "edgar", #specify the data source
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022b.zip", #include the link to the data set
    freq = "m", #indicate the time frequency (can be m for monthly or q for quarterly)
    ipcc_sector = "3.C.4", #for Edgar variables, specify the ipcc_sector information (to be found in the excel data file the variable is contained in)
    geo = "AT" )) %>% #set the geographical region for the model, add a pipe operator to add multiple variables to the dictionary at once

  #This is an example of a variable added from Eurostat:
  bind_rows(tibble(
    model_varname = "Price_AgriProducts",
    full_name = "Price indices of agricultural products, output",
    database  = "eurostat", #define database as eurostat
    variable_code = "140000", #add the variable code (this specifies which details the variable should represent)
    dataset_id = "apri_pi20_outq", #add the dataset ID
    var_col = "product", #add the column name the variable code specification was chosen from
    freq = "q",
    geo = "AT",
    unit = "I20", #if given, define the unit
    s_adj = "NSA", #chose between seasonally adjusted or unadjusted data (if given)
    nace_r2 = "B-D")) %>% #define the nace_r2 sector specification, if applicable

  bind_rows(tibble(
    model_varname = "Price_AnimalProducts",
    full_name = "Price indices of agricultural products - Agricultural goods output",
    database  = "eurostat",
    variable_code = "140000",
    dataset_id = "apri_pi15_outq",
    var_col = "product",
    freq = "q",
    geo = "AT",
    unit = "I15",
    p_adj = "NI")) %>%

  bind_rows(tibble(
    model_varname = "EmiN2OManure_indirect",
    full_name = "Indirect N2O Emissions from manure management",
    database  = "edgar",
    ipcc_sector = "3.C.6",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/EDGAR_N2O_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%

  bind_rows(tibble(
    model_varname = "Flights",
    full_name = "National air passenger transport by reporting country",
    database  = "eurostat",
    variable_code = "CAF_PAS",
    dataset_id = "avia_panc",
    var_col = "tra_meas",
    freq = "m",
    geo = "AT",
    unit = "FLIGHT")) %>%

  bind_rows(tibble(
    model_varname = "ConstructionManuf",
    full_name = "Production in Construction in Manufacturing Sectors",
    database  = "eurostat",
    variable_code = "PRD",
    dataset_id = "sts_copr_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "B-D_F")) %>%

  bind_rows(tibble(
    model_varname = "EmiCO2Aviation",
    full_name = "CO2 (fossil) emissions from Civil Aviation",
    database  = "edgar",
    ipcc_sector = "1.A.3.a",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%

  bind_rows(tibble(
    model_varname = "EmiCO2Road",
    full_name = "CO2 emissions from Road Transport",
    database  = "edgar",
    ipcc_sector = "1.A.3.b_noRES",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%

  bind_rows(tibble(
    model_varname = "EmiCO2Manuf",
    full_name = "CO2 (fossil) emissions from Manufacturing Industries and Construction",
    database  = "edgar",
    ipcc_sector = "1.A.2",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%

  bind_rows(tibble(
    model_varname = "EmiCO2Residential",
    full_name = "CO2 (fossil) emissions from Residential and other sectors",
    database  = "edgar",
    ipcc_sector = "1.A.4",
    dataset_id = "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/EDGAR/datasets/v80_FT2022_GHG/IEA_EDGAR_CO2_m_1970_2022b.zip",
    freq = "m",
    geo = "AT")) %>%

  bind_rows(tibble(
    model_varname = "ConstructionBuildings",
    full_name = "Production in Construction in the Building sector",
    database  = "eurostat",
    variable_code = "PROD",
    dataset_id = "sts_copr_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "F41")) %>%

  bind_rows(tibble(
    model_varname = "Lab_Inp_Constr",
    full_name = "Labour input in construction",
    database  = "eurostat",
    variable_code = "EMPL",
    dataset_id = "sts_colb_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "F")) %>%

  bind_rows(tibble(
    model_varname = "Lab_Inp_Indus",
    full_name = "Labour input in industry",
    database  = "eurostat",
    variable_code = "EMPL",
    dataset_id = "sts_inlb_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "B-E36")) %>%

  bind_rows(tibble(
    model_varname = "ElectrCons",
    full_name = "Consumption of electricity - GWh",
    database  = "eurostat",
    variable_code = "IS-CEL-GWH",
    dataset_id = "ei_isen_m",
    var_col = "indic",
    freq = "m",
    geo = "AT",
    unit = "NA",
    s_adj = "NSA",
    nace_r2 = "B-D")) %>%

  bind_rows(tibble(
    model_varname = "AnimalImports",
    full_name = "Total imports of live animals",
    database  = "eurostat",
    variable_code = "SL",
    dataset_id = "apro_mt_pheadm",
    var_col = "meatitem",
    freq = "m",
    geo = "AT",
    unit = "THS_T",
    s_adj = "NSA",
    nace_r2 = "B-D",
    meat = "B1000")) %>%

  bind_rows(tibble(
    model_varname = "TurnoverInd",
    full_name = "Turnover in industry, domestic market - monthly data",
    database  = "eurostat",
    variable_code = "TOVD",
    dataset_id = "sts_intvd_m",
    var_col = "indic_bt",
    freq = "m",
    geo = "AT",
    unit = "I21",
    s_adj = "NSA",
    nace_r2 = "B_C")) %>%

  bind_rows(tibble(
    model_varname = "PPI",
    full_name = "Output prices of the domestic market index (Producer price index) (NSA)",
    database  = "eurostat",
    variable_code = "IS-PPI",
    dataset_id = "ei_isir_m",
    var_col = "indic",
    freq = "m",
    geo = "AT",
    unit = "RT1",
    s_adj = "NSA",
    nace_r2 = "B-E36")) %>%

  bind_rows(tibble(
    model_varname = "Unemp",
    full_name = "Unemployment",
    database  = "eurostat",
    variable_code = "T",
    dataset_id = "lfsq_urgan",
    var_col = "sex",
    age = "Y20-64",
    citizen = "TOTAL",
    freq = "q",
    geo = "AT",
    unit = "PC",
    s_adj = "NSA")) %>%

  bind_rows(tibble(
    model_varname = "PT_Emp",
    full_name = "Part-time employment and temporary contracts",
    database  = "eurostat",
    variable_code = "T",
    dataset_id = "lfsi_pt_q",
    var_col = "sex",
    age = "Y20-64",
    wstatus = "EMP_PT",
    freq = "q",
    geo = "AT",
    unit = "PC_EMP",
    s_adj = "NSA",
    nace_r2 = " ")) %>%

  bind_rows(tibble(
    model_varname = "EmpCompensation",
    full_name = "Compensation of employees",
    database  = "eurostat",
    variable_code = "D1",
    dataset_id = "tipslm14",
    var_col = "na_item",
    freq = "q",
    geo = "AT",
    unit = "CP_MNAC",
    s_adj = "NSA")) %>%

  bind_rows(tibble(
    model_varname = "ExchangeRate",
    full_name = "Nominal effective exchange rate - 42 trading partners (industrial countries)",
    database  = "eurostat",
    variable_code = "NEER_IC42",
    dataset_id = "ert_eff_ic_q",
    var_col = "exch_rt",
    freq = "q",
    geo = "AT",
    unit = "I15",
    s_adj = "NSA")) %>%

  bind_rows(tibble(
    model_varname = "GDP_Deflator",
    full_name = "GDP deflator",
    database  = "eurostat",
    variable_code = "B1GQ",
    dataset_id = "namq_10_gdp",
    var_col = "na_item",
    freq = "q",
    geo = "AT",
    unit = "PD10_EUR",
    s_adj = "NSA"
  )) %>%

  bind_rows(tibble(
    model_varname = "LabCost_ICS",
    full_name = "labour cost index in Industry, construction and services ",
    database  = "eurostat",
    variable_code = "LM-LCI-TOT",
    dataset_id = "ei_lmlc_q",
    var_col = "indic",
    freq = "q",
    geo = "AT",
    unit = "I20",
    s_adj = "NSA",
    nace_r2 = "B-S")) -> dict_new



## Dict here

dict %>%

  bind_rows(dict_new) %>%

  bind_rows(tibble(
    model_varname = paste0("HICP_",c("Diesel", "Petrol", "FlightsDom", "AviaInt")),
    full_name = paste0("Harmonised Price Index for ",c("Diesel", "Petrol", "Domestic flights", "International flights")),
    variable_code = c("CP07221", "CP07222", "CP07331", "CP07332"),
    unit = "I15",
    dataset_id = "prc_hicp_midx",
    freq = "m",
    geo = "AT",
    var_col = "coicop",
    database = "eurostat"
  )) %>%

  bind_rows(tibble(
    model_varname = "RoadFreight",
    dataset_id = "road_go_tq_tott",
    full_name = "Road freight transport",
    unit = "MIO_TKM",
    database = "eurostat",
    var_col = "tra_type",
    variable_code = "TOTAL",
    tra_oper = "TOTAL",
    geo = "AT",
    freq = "q")) %>%

  bind_rows(tibble(
    model_varname = "Slaughter", # this is free to choose but must be unique
    full_name = "Slaughterings",
    database  = "eurostat",
    variable_code = "SL", # in this case use the bt_indicator information here
    dataset_id = "apro_mt_pwgtm",
    var_col = "meatitem", # here we specify what the column with the variables is called
    freq = "m", # for quarterly data, 'm' would be monthly
    geo = "AT",
    unit = "THS_T",
    meat = "B1000")) -> dict_ready



# Specification


specification <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n",
    "n",
    "n",
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "EmiCO2Industry",
    "IndProd",
    #"EmiCO2Combustion",
    "EmiCH4Livestock",
    #"Consumption",
    "Flights",
    "EmiCO2Aviation",
    "RoadFreight",
    "EmiCO2Road",
    "EmiCO2Residential",
    "EmiCO2Combustion"
  ),
  independent = c(
    "IndProd",
    "GDP + Export + HICP_Electricity",
    #"Consumption + HICP_Electricity + EmiCO2Industry",
    "Export + Slaughter + GValueAddAgri",
    #"FinConsExpGov + HICP_Gas + HICP_Electricity + GDP",
    "HICP_AviaInt",
    "Flights",
    "HICP_Diesel + HICP_Petrol",
    "RoadFreight",
    "HICP_Electricity + FinConsExpHH",
    "EmiCO2Road + EmiCO2Residential + EmiCO2Industry + EmiCH4Livestock + EmiCO2Aviation"
  )
)


specification %>%
  select(-type) %>%
  rename_with(str_to_title) %>%
  kable(format = "latex", booktabs = TRUE) %>%
  kable_styling()


# Modelling



# 2015 price: 1.3 € / litre
# carbon content for Diesel: 2.68 kg CO2 / litre
# carbon content for Petrol: 2.31 kg CO2 / litre


co2_tax_calc <- function(carbon_price, old_price, co2_intensity){
  new_price <- old_price + (co2_intensity/1000 * carbon_price)

  new_price/old_price
}


# 1.12
# https://www.statista.com/statistics/603723/diesel-fuel-prices-austria/
#
#   1.2
# https://www.statista.com/statistics/598062/unleaded-gasoline-prices-austria
#
#
#
# https://www.bluevalet.fr/en/blog/what-is-the-impact-of-rising-oil-prices-on-my-airfare
# 40%
#
# According to Statista, the retail price of aviation kerosene in the UK was 28.8 pence per liter in 2015. The projection also suggests that the price will increase to almost 50 pence per liter by 2035.
#
# 2015 exchange rate to EUR: 1.3773
# https://www.poundsterlinglive.com/history/GBP-EUR-2015
#
# 0.288 * 1.3773 = 0.3968
# co2 per litre of kerosene: 3.15 kg




diesel <- co2_tax_calc(200, 1.12, 2.68)
petrol <- co2_tax_calc(200, 1.2, 2.31)
kerosene <- co2_tax_calc(200, 0.3968, 3.15)







# fcst <- forecast_model(model, exog_fill_method = "AR")
# plot(fcst,first_date = "2015-01-01")
#
# fcst$exog_data_nowcast %>%
#   mutate(HICP_Petrol = HICP_Petrol * petrol,
#          HICP_Diesel = HICP_Diesel * diesel,
#          HICP_AviaInt = HICP_Petrol * kerosene * 0.4) -> exog_data_high
#
# f2_high <- forecast_model(model, exog_predictions = exog_data_high, plot = FALSE,
#                           exog_fill_method = "AR")
#
#




forecast_method = "AR"

#,"DE","FR"
for(country in c("AT")){

  dict_ready %>%
    mutate(geo = country) -> dict_ready

  model <- run_model(specification = specification,
                     dictionary = dict_ready,
                     inputdata_directory = paste0("small_examples/EMCC 24/",country),
                     primary_source = "local",
                     save_to_disk = paste0("small_examples/EMCC 24/",
                                           country,"/EMCC_data_updated.xlsx"),

                     present = FALSE,
                     quiet = FALSE,
                     saturation.tpval = 0.001,
                     gets_selection = FALSE,
                     constrain.to.minimum.sample = FALSE)





  model_list <- lapply(model$module_collection$model, gets::as.lm)

  names(model_list) <- model$module_order$dependent

  table_change <- function(input_table, label){

    # insert caption
    tab <- gsub("\\begin{table}",
                paste0("\\begin{table}\n\\label{",label,"}"), input_table, fixed = TRUE)

    # insert resizebox
    tab <- gsub("\\begin{tabular}",
                "\\resizebox{\\textwidth}{!}{\\begin{tabular}", tab, fixed = TRUE)

    # fixing the footer
    tab <- gsub("\\end{tabular}",
                "\\end{tabular}}",tab, fixed = TRUE)

    return(tab)
  }

  # ensure the correct order of the table
  lapply(model_list, broom::tidy) %>%
    bind_rows() %>%
    distinct(term) %>%
    filter(!str_detect(term, "iis|sis|q_[0-9]+")) %>%
    mutate(base = gsub("L[0-9]+\\.","",term)) %>%
    mutate(base_num = case_when(base == "mconst" ~ 2,
                                base == "trend" ~ 3,
                                grepl("ar[0-9]+",base) ~ 1,
                                TRUE ~ 4)) %>%
    arrange(base_num, base, desc(term)) %>%
    pull(term) -> coef_order

  table_output <- modelsummary::modelsummary(
    model_list,
    #coef_omit = "iis|sis|q_[0-9]+",
    coef_map = coef_order,
    gof_omit = "R",
    #output = "latex",
    title = "Final models run for each sub-module for the illustrative example of Austria.",
    notes = "Quarterly Dummies, Impulse (IIS) and Step Indicators (SIS) are not shown individually but were activated for all models.",
    stars = TRUE,
  )


  # this output can go straight into the latex
  table_change(table_output, label = "tab:regression_summary")


  # -- Diagnostics

  diagnostics_model(model) %>%
    rename(Module = module) %>%
    kable(format = "latex",booktabs = TRUE, digits = 3, label = "diagnostics", caption = "Diagnostic results for each sub-module.") %>%
    kable_styling()


  diagnostics_model(model) %>%
    mutate(across(c(AR,ARCH, `Super Exogeneity`), ~ round(.,4))) %>%
    DT::datatable() %>%
    DT::formatStyle(columns = c("AR", "ARCH", "Super Exogeneity"),
                    backgroundColor = DT::styleInterval(cuts = c(0.01, 0.05), values = c("lightcoral", "lightsalmon", "lightgreen"))) %>%
    DT::formatRound(columns = c("AR", "ARCH", "Share of Indicators"),
                    digits = 4)

  model %>%
    network() -> p
  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/Network",".pdf"))
  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/Network",".png"), bg = "white")


  # -- Forecasting

  f2 <- forecast_model(model, exog_fill_method = forecast_method, plot = FALSE)

  # -- Forecasting Plotting

  plot(f2, first_date = "2015-01-01") +
    labs(title = paste0("Illustrative Example for ",case_when(country == "AT" ~ "Austria",
                                                              country == "FR" ~ "France",
                                                              country == "DE" ~ "Germany",
                                                              country == "UK" ~ "UK")), subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values.") +
    theme(text = element_text(family = "Myriad Pro"),
          plot.subtitle = element_markdown()) -> p

  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/forecast_simple_",country,".pdf"), device = cairo_pdf)
  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/forecast_simple_",country,".png"), bg = "white")



  plot(f2, exclude.exogenous = FALSE, first_date = "2015-01-01") +
    labs(title = paste0("Illustrative Example for ",case_when(country == "AT" ~ "Austria",
                                                              country == "FR" ~ "France",
                                                              country == "DE" ~ "Germany",
                                                              country == "UK" ~ "UK")),
         subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
    theme(text = element_text(family = "Myriad Pro"),
          plot.subtitle = element_markdown()) -> p

  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/forecast_inclexog_",country,".pdf"), device = cairo_pdf)
  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/forecast_inclexog_",country,".png"), bg = "white")




  # --

  f2_insample <- forecast_insample(model, sample_share = .96, exog_fill_method = forecast_method)

  extract_dep_vars <- f2_insample$central %>% distinct(dep_var) %>% pull

  vars_to_plot <- c("EmiCH4Livestock", "EmiCO2Aviation", "EmiCO2Combustion", "EmiCO2Road", "Flights", "RoadFreight")

  ggplot2::ggplot() +
    ggplot2::geom_line(data = model$full_data %>%
                         rename(dep_var = na_item) %>%
                         filter(dep_var %in% extract_dep_vars,
                                time > as.Date("2010-01-01")) %>%
                         filter(dep_var %in% vars_to_plot),
                       ggplot2::aes(x = .data$time, y = .data$values), linewidth = 1) +

    ggplot2::facet_wrap(~dep_var, scales = "free") +
    ggplot2::geom_ribbon(data = f2_insample$uncertainty %>% filter(dep_var %in% vars_to_plot), ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = f2_insample$uncertainty %>% filter(dep_var %in% vars_to_plot), ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = f2_insample$uncertainty %>% filter(dep_var %in% vars_to_plot), ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE) +

    ggplot2::geom_line(data = f2_insample$central %>% filter(dep_var %in% vars_to_plot), ggplot2::aes(y = .data$value, x = .data$time, color = as.factor(.data$start)), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~.data$dep_var, scales = "free") +
    #ggplot2::scale_color_brewer(palette = "PRGn") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_cartesian(expand = TRUE) +

    ggplot2::labs(x = NULL, y = NULL, title = "Automatic Forecasting Hindcasts") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   text = element_text(family = "Myriad Pro"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> p


  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/hindcast_",country,".png"), bg = "white")



  # --- Scenario

  f2$exog_data_nowcast %>%
    mutate(HICP_Petrol = HICP_Petrol * petrol,
           HICP_Diesel = HICP_Diesel * diesel,
           HICP_AviaInt = HICP_AviaInt * (1+((kerosene-1) * 0.4))) -> exog_data_high

  f2_high <- forecast_model(model, exog_predictions = exog_data_high, plot = FALSE,
                            exog_fill_method = forecast_method)

  #
  # plot(f2_high, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity") +
  #   coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
  #   labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
  #   theme(text = element_text(family = "Myriad Pro"),
  #         plot.subtitle = element_markdown()) -> a
  #
  # plot(f2, exclude.exogenous = FALSE, grepl_variables = "Combustion|Electricity") +
  #   coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
  #   labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.") +
  #   theme(text = element_text(family = "Myriad Pro"),
  #         plot.subtitle = element_markdown()) -> b


  f2_high_data <- plot(f2_high, exclude.exogenous = FALSE, return.data = TRUE)
  f2_data <- plot(f2, exclude.exogenous = FALSE,  return.data = TRUE)


  #viridis::viridis(4)
  man_cols <- c(Observation = "#440154FF",
                `Endogenous Forecast` = "#31688EFF",
                `Exogenous Forecast` = "#35B779FF",
                `Insample Fit` = "#FDE725FF")


  f2_data %>%
    mutate(name = "AR Forecast") %>%
    bind_rows(f2_high_data %>%
                mutate(name = NA) %>%
                mutate(name = case_when(na_item %in% c("EmiCO2Combustion", "EmiCO2Road","RoadFreight", "EmiCO2Aviation") & type == "Endogenous Forecast" ~ "Scenario Price",
                                        na_item %in% c("HICP_Diesel","HICP_Petrol","HICP_AviaInt") ~ "Scenario Price",
                                        TRUE ~ name))) %>%

    #filter(na_item %in% c("HICP_Diesel","HICP_Petrol","HICP_AviaInt","EmiCO2Combustion", "EmiCO2Road")) %>%
    filter(na_item %in% c("HICP_Diesel","HICP_Petrol","HICP_AviaInt", "RoadFreight", "EmiCO2Aviation") & time > as.Date("2020-01-01")) %>%

    ggplot(aes(x = time, y = values, linetype = name, color = type)) +
    geom_line() +
    coord_cartesian(xlim = c(as.Date("2020-01-01"),as.Date("2025-01-01"))) +
    facet_wrap(~na_item, scales = "free") +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +

    ggplot2::facet_wrap(~.data$na_item, scales = "free") +

    ggplot2::labs(x = NULL, y = NULL) +

    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    #ggplot2::scale_color_viridis_d() +
    ggplot2::scale_color_manual(values = man_cols) +

    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) +
    labs(title = "Illustrative Example for Austria", subtitle = "Showing the <span style = color:#440154FF>Observed</span>, <span style = color:#FDE725FF>Fitted</span> and <span style = color:#21908CFF>Forecasted</span> Values incl. Exogenous Variables.<br>Dotted line is scenario run with Carbon Price. Solid lines are an AR forecast.") +
    theme(text = element_text(family = "Myriad Pro"),
          plot.subtitle = element_markdown()) -> p


  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/Scenario.pdf"), device = cairo_pdf)
  ggsave(p, width = 7, height = 5, file = paste0("small_examples/EMCC 24/",country,"/Scenario.png"), bg = "white")



}

f2_data %>%
  mutate(name = "AR Forecast") %>%
  bind_rows(f2_high_data %>%
              mutate(name = "Scenario Price")) %>%

  filter(na_item == "EmiCO2Combustion" & type == "Endogenous Forecast") %>%

  summarise(emissions = sum(values, na.rm = TRUE), .by = name) %>%
  mutate(total = sum(emissions),
         diff = c(NA,diff(emissions)),
         rel_diff = diff/total)

# ----


# model_constrained <- run_model(specification = specification,
#                                dictionary = new_dict,
#                                inputdata_directory = "data-raw/test_for_now/",
#                                primary_source = "local",
#                                save_to_disk = "data-raw/test_for_now/EAERE_data.xlsx",
#
#                                present = FALSE,
#                                quiet = FALSE,
#                                constrain.to.minimum.sample = TRUE)
#
# f2_cons <- forecast_model(model_constrained, exog_fill_method = "auto")
#
# f2_cons_insample <- forecast_insample(model_constrained, sample_share = .9, exog_fill_method = "auto")
#
# f2_cons_insample$plot + geom_hline(aes(yintercept = 0))






