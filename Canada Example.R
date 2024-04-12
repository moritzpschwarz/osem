library(statcanR)
library(tidyverse)
library(readr)
library(magrittr)

#toy specs for example
spec <- dplyr::tibble(
  type = c(
    "d"
  ),
  dependent = c(
    "TOTS"
  ),
  independent = c(
    "Export + GDP"
  )
)

#toy dictionary for example

dict_statCan <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~dataset_id, ~freq, ~var_col, ~found, ~nace_r2, ~unit,  ~geo, ~s_adj, ~Prices,
  "GDP", "Gross domestic product", "statcan", "36-10-0434-02", "q", "na_item", TRUE, NA, "millions", "Canada", "Seasonally adjusted at annual rates", "2017 constant prices",
  "Export", "Exports of goods and services", "statcan", "12-10-0011-01","q", "na_item", TRUE, NA, "millions", "Canada", "Seasonally adjusted", NA
)
dict_statCan$variable_code <- NA
dict_statCan <- as.data.frame(dict_statCan)



dictionary <- dict_statCan #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_statCan)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)

#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)



run_model(specification = spec, dictionary = dict_statCan, primary_source = "download",
          save_to_disk = "data-raw/statcan/test.xlsx", inputdata_directory = "data-raw/statcan")





#build the list of dataframes

#for test we just set column_filters as default cols of database
column_filters <- actual_cols






dictionary <- dict_statCan #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_statCan)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)

# source("determine_codes.R")
# source("auxiliary.R")
# source("calculate_identities.R")
# source("config_table.R")
# source("run_module.R")
# source("identify_module_data.R")
# source("identity_module.R")

#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)

#build the list of dataframes

#for test we just set column_filters as default cols of database
column_filters <- actual_cols
statcan_data <- download_statcan(to_obtain, column_filters, FALSE)

# check whether aggregate model is well-specified
module_order <- check_config_table(spec)
classification <- classify_variables(specification = module_order)


loaded_data <- statcan_data$df

#question about nace codes. do i need them?
# full_data <- calculate_identities(specification = module_order, data = loaded_data, dictionary = dictionary)
full_data <- loaded_data


# initialise storage of estimation results
module_collection <- module_order %>%
  dplyr::mutate(dataset = list(NA_complex_),
                model = list(NA_complex_))

tmp_data <- full_data

print(summary(tmp_data))

tmp_data %>%
  tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") -> dat_tmp

print(dat_tmp[2])

dat_tmp %>% dplyr::mutate(out = "GDP" + "Export")


# loop through all modules
for (i in module_order$order) {

  # print progress update
  if(!FALSE){
    if(i == 1){cat("\n--- Estimation begins ---\n")}
    if(module_order$type[i] == "n") {cat(paste0("Estimating ", module_order$dependent[i], " = ", module_order$independent[i]), "\n")}
    if(module_order$type[i] == "d") {cat(paste0("Constructing ", module_order$dependent[i], " = ", module_order$independent[i]), "\n")}
  }

  # estimate current module, using most up-to-date dataset including predicted values
  module_estimate <- run_module(
    module = module_order[module_order$order == i, ],
    data = tmp_data,
    classification = classification,
    use_logs = use_logs,
    trend = trend,
    ardl_or_ecm = ardl_or_ecm,
    max.ar = max.ar,
    max.dl = max.dl,
    saturation = saturation,
    saturation.tpval = saturation.tpval,
    max.block.size = max.block.size,
    gets_selection = gets_selection,
    selection.tpval = selection.tpval
  )

  # store module estimates dataset, including fitted values
  module_collection[module_collection$order == i, "dataset"] <- dplyr::tibble(dataset = list(module_estimate$data))
  module_collection[module_collection$order == i, "model"] <- dplyr::tibble(dataset = list(module_estimate$model))
  module_collection[module_collection$order == i, "model.args"] <- dplyr::tibble(dataset = list(module_estimate$args))
  module_collection[module_collection$order == i, "indep"] <- dplyr::tibble(dataset = list(module_estimate$indep))
  module_collection[module_collection$order == i, "dep"] <- dplyr::tibble(dataset = list(module_estimate$dep))

  # update dataset for next module by adding fitted values
  tmp_data <- update_data(orig_data = tmp_data, new_data = module_estimate$data)
}



