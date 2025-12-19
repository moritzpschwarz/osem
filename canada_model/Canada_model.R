# install.packages("devtools")
#devtools::install_github("moritzpschwarz/osem")
library(osem)
library(tidyverse)

# Source the dictionary builder
#source("Canada_model_dictionary.R")

dictionary <- prepare_canada_dictionary()

spec <- dplyr::tibble(
  type = c(
    "d"
  ),
  dependent = c(
    "DInventories"
  ),
  independent = c(
    "VAGov"

  )
)

model_result <- run_model(specification = spec, dictionary = dictionary)

browser()

# economic specification
#Supply = demand equation
spec_econ <- tibble(type = c("d", "d"), dependent = c("Supply", "Demand"), independent = c("GDPOutput + Imports", "GDPExpenditure + Imports")) %>%
  add_row(type = "d", dependent = "GDPOutput", independent = "VA + TaxesLessSubsidies") %>%
  add_row(type = "d", dependent = "DInventories", independent = "Supply - ConsHH - ConsGov - CapForm - Exports") %>%
  #supply side
  add_row(type = "d", dependent = "VA", independent = "VARealEstate + VAFinance + VAWholesaleTrade + VATransportationandWarehousing + VAInformation + VAAgriculture + VAMiningAndOil + VAUtilities + VAManufacturing + VAWasteManagement + VAConstruction + VAEnergy + VAGov") %>%
  add_row(type = "n", dependent = "VAMiningAndOil", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAUtilities", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAManufacturing", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAEnergy", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAConstruction", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAFinance", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARealEstate", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAWholesaleTrade", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VATransportationandWarehousing", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAInformation", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VARetail", independent = "ConsHH + ConsGov + CapForm") %>%
  add_row(type = "n", dependent = "VAGov", independent = "ConsHH + ConsGov") %>%
  #Demand side
  add_row(type = "d", dependent = "GDPExpenditure", independent = "ConsHH + ConsGov + CapForm + Exports - Imports") %>%
  add_row(type = "n", dependent = "CapForm", independent = "CapFormGov + CapFormBusiness") %>%
  add_row(type = "n", dependent = "CapFormGov", independent = "CapFormGovConstruction + CapFormGovResidentialStructures + CapFormGovNonResidentialStructures + CapFormGovMachineryandEquipment + CapFormGovIntellectualProperty") %>%
  add_row(type = "n", dependent = "CapFormBusiness", independent = "CapFormBusinessConstruction + CapFormBusinessResidentialStructures + CapFormBusinessNonResidentialStructures + CapFormBusinessMachineryandEquipment + CapFormBusinessIntellectualProperty") %>%
  add_row(type = "n", dependent = "Imports", independent = "Exports") %>%
  #simple phillips curve for inflation
  add_row(type = "n", dependent = "Inflation", independent = "Unemployment")

# add_row(type = "n", dependent = "EmiCO2OilGas", independent = "VAMiningAndOil") %>%
# add_row(type = "n", dependent = "EmiCO2Mining", independent = "VAMiningAndOil") %>%
# add_row(type = "d", dependent = "TotatofOilGasMining", independent = "EmiCO2Mining + EmiCO2OilGas")

browser()

model_result <- run_model(specification = spec_econ, dictionary = dictionary)

# model_result <- run_model(
#   specification = spec_econ,
#   dictionary = dictionary,
#   primary_source = "download",
#   trend = TRUE,
#   saturation.tpval = 0.01,
#   gets_selection = TRUE,
#   constrain.to.minimum.sample = FALSE,
#   plot = FALSE
# )
browser()
model_forecast <- forecast_model(model_result, n.ahead = 5, exog_fill_method = "auto")

