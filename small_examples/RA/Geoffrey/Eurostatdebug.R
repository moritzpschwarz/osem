library(statcanR)
library(tidyverse)
library(readr)
library(magrittr)

add_dict_entry <- function(dict, model_varname, full_name, database, variable_code, dataset_id, var_col, freq, geo, unit, s_adj, nace_r2, ipcc_sector, cpa2_1, siec, sector, tax, currency, direct, co_nco, train = NA, vehicle = NA, age = NA, partner = NA) {
  dict %>%
    add_row(model_varname = model_varname, full_name = full_name, database = database, variable_code = variable_code, dataset_id = dataset_id, var_col = var_col, freq = freq, geo = geo, unit = unit, s_adj = s_adj, nace_r2 = nace_r2, ipcc_sector = ipcc_sector, cpa2_1 = cpa2_1, siec = siec, sector = sector, tax = tax, currency = currency, direct = direct, co_nco = co_nco, train = train, vehicle = vehicle, age = age, partner = partner) %>%
    return()
}

dictionary <- tibble(model_varname = "Supply", full_name = "Total Supply", database = NA, variable_code = NA, dataset_id = NA, var_col = NA, freq = NA, geo = NA, unit = NA, s_adj = NA, nace_r2 = NA, ipcc_sector = NA, cpa2_1 = NA, siec = NA, sector = NA, tax = NA, currency = NA, direct = NA, co_nco = NA, train = NA, vehicle = NA, age = NA, partner = NA)
dictionary <- dictionary %>%
  add_dict_entry("Demand", "Total Demand", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) %>%
  add_dict_entry("ExportsA", "Exports of Agric", "eurostat", "EXP", "ext_tec09", "stk_flow", "A", "DE", "THS_EUR",NA,"A", NA, NA, NA, NA, NA, NA, NA, NA,NA, NA,NA, "WORLD") %>%
  add_dict_entry("IndustryHoursWorkedPerEmployee", "Hours worked per employed person for Industry except Construction", "eurostat", "HW_EMP", "nama_10_lp_a21", "na_item", "A", "DE", "HW", NA, "B-E", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)

actual_cols = colnames(dictionary)

spec <- tibble(type = c("d"), dependent = c("Demand"), independent = c("ExportsA"))


module_order <- osem:::check_config_table(spec)

to_obtain <- osem:::determine_variables(specification = module_order,
                                        dictionary = dictionary)
#
#
# spec <- tibble(type = c("d"), dependent = c("Demand"), independent = c("ExportsA"))
#
data <- osem:::download_eurostat(to_obtain = to_obtain,additional_filters = c("partner"),quiet=TRUE)
#
