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
  ~model_varname, ~full_name, ~database, ~dataset_id, ~freq, ~var_col, ~found, ~nace_r2, ~SCALAR_FACTOR,  ~GEO, ~`Seasonal adjustment`, ~Prices,
  "GDP", "Gross domestic product", "statcan", "36-10-0434-02", "q", "na_item", TRUE, NA, "millions", "Canada", "Seasonally adjusted at annual rates", "2017 constant prices",
  "Export", "Exports of goods and services", "statcan", "12-10-0011-01","q", "na_item", TRUE, NA, "millions", "Canada", "Seasonally adjusted", NA
)


dict_statCan <- as.data.frame(dict_statCan)


#' Download StatCan
#'
#' Downloads StatCan data given a data.frame of required variables
#'
#' @inheritParams load_or_download_variables
#' @param to_obtain A data.frame as returned by [determine_variables()].
#' @param column_filters A character vector storing the column names of
#' filters. Length 0 if not present.
#'
#' @return Returns a list with two named elements: \code{$df} stores the
#' downloaded data and \code{$to_obtain} the updated data.frame tracking which
#' variables still need to be obtained.

download_statcan <- function(to_obtain, column_filters, quiet) {


  # initialise empty df
  df_statcan <- data.frame()

  #pulls dataframe of unique database ids
  dataset_id <- to_obtain %>% dplyr::filter(.data$database == "statcan" &
                                                .data$found == FALSE) %>%
    dplyr::distinct(dataset_id)

  #iterate over unique database ids and pull the dataset
  #each iteration
  for (i in 1:nrow(dataset_id)) {

    #download data table according to data_base_id
    id <- dataset_id[i,1]
    df <- statcan_data(id,"eng")
    df <- as.data.frame(df)

    #get the dictionary coordinates that use the following dataset_id
    indices <- which(to_obtain$database == "statcan" & to_obtain$dataset_id == id)

    #look through the rows of the dictionary based on the coordinates for the id
    for (idx in indices) {
      subset_of_data <- df

      #iterate through all the column filter names of the particular dictionary row to build filtered dataframe
      for (k in seq_along(column_filters)) {
        filtername <- as.character(column_filters[k])

        # print(names(subset_of_data))
        if (filtername %in% names(subset_of_data)) {

          subset_of_data <- subset_of_data %>% dplyr::filter(.,.[[filtername]] == to_obtain[idx,filtername])
          #print(head(subset_of_data,2))
        }
      }

      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(subset_of_data) == 0L) {
        stop(paste0("For model variable '", to_obtain[idx,1], "', the dataset is empty after applying filters. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[idx, "found"] <- TRUE
      }



      subset_of_data <- subset_of_data %>% dplyr::mutate(na_item = to_obtain$model_varname[idx])
        # dplyr::rename_with(.cols = dplyr::all_of(to_obtain$var_col[idx]), .fn = ~paste0("na_item")) %>%

      #What is the point of this??
      # sub <- sub %>%
      #   dplyr::rename_with(.cols = dplyr::all_of(varcolname), .fn = ~paste0("na_item")) %>%
      #   dplyr::mutate(na_item = to_obtain$model_varname[j])

      # if have monthly data, need to aggregate to quarterly
      if (to_obtain$freq[idx] == "m") {
        # need to aggregate across all filters
        columns <- colnames(subset_of_data)
        unique_columns <- setdiff(columns, "values") # should be unique across these
        stopifnot(sum(duplicated(subset_of_data[, unique_columns])) == 0L) # sanity check
        groupby_columns <- union(c("year", "quarter"), setdiff(unique_columns, "REF_DATE")) # want to group_by year-quarter, so exclude time column
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(year = lubridate::year(.data$REF_DATE),
                        quarter = lubridate::quarter(.data$REF_DATE)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groupby_columns))) %>%
          dplyr::summarise(values = sum(.data$VALUE),
                           n = dplyr::n(), # record how many months are available in each quarter
                           time = min(.data$REF_DATE)) %>%
          dplyr::ungroup() %>%
          # drop "incomplete" quarters
          dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n")
      }
      # previous if statement turns statCan col REF_DATE into time
      # since data is given in quarerly values we need to still rename the table name
      else {
        subset_of_data <- subset_of_data %>% dplyr::rename("time" = "REF_DATE")

      }

      # ensure column "time" is a Date variable (Moritz had this)
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(time = as.Date(.data$time))

      subset_of_data <- subset_of_data %>% dplyr::rename("values" = "VALUE")


      df_statcan <- dplyr::bind_rows(df_statcan, subset_of_data)
      write.csv(df_statcan, "/Users/geoffreyharper/Desktop/file.csv", row.names=FALSE)
      print(head(subset_of_data,2))
    }


  }

  print(df_statcan)

  return (
    list ( df= df_statcan, to_obtain = to_obtain)
  )


}

dictionary <- dict_statCan #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_statCan)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)

source("determine_codes.R")
source("auxiliary.R")
source("calculate_identities.R")
source("config_table.R")
source("run_module.R")
source("identify_module_data.R")
source("identity_module.R")

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




