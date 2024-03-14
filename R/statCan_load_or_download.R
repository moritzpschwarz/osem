library(statcanR)
library(tidyverse)
library(readr)
library(magrittr)

#toy specs for example
spec <- dplyr::tibble(
  type = c(
    "d",
    "d",
    "n",
    "n"
  ),
  dependent = c(
    "StatDiscrep",
    "TOTS",
    "Import",
    "GDP"
  ),
  independent = c(
    "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
    "GValueAdd + Import",
    "FinConsExpHH + GCapitalForm",
    "GDP"
  )
)

#toy dictionary for example

dict_statCan <- tibble::tribble(
  ~model_varname, ~full_name, ~database, ~dataset_id, ~freq, ~TEST, ~found, ~SCALAR_FACTOR,  ~GEO, ~`Seasonal adjustment`, ~Prices,
  "GDP", "Gross domestic product", "statcan", "36-10-0434-02", "q", TRUE, TRUE, "millions", "Canada", "Seasonally adjusted at annual rates", "2017 constant prices",
  "Export", "Exports of goods and services", "statcan", "12-10-0011-01","q", TRUE, TRUE, "millions", "Canada", "Seasonally adjusted", NA,
  "Import", "Imports of goods and services", "statcan", "12-10-0011-01", "q", TRUE, TRUE, "millions", "Canada", "Seasonally adjusted", NA,

)


dict_statCan <- as.data.frame(dict_statCan)
dictionary <- as.data.frame(dict_statCan)

download_statcan <- function(to_obtain, additional_filters, quiet) {

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


    #generate list for additional_filters
    #look through the rows of the dictionary based on the coordinates for the id
    for (idx in indices) {
      subset_of_data <- df
      #iterate through all the filters of the particular dictionary row to build filtered dataframe
      for (k in seq_along(additional_filters)) {
        filtername <- as.character(additional_filters[k])

        # print(names(subset_of_data))
        if (filtername %in% names(subset_of_data)) {
          print(filtername)
          print(typeof(filtername))
          print(to_obtain[idx,filtername])

          subset_of_data <- subset_of_data %>% dplyr::filter(.,.[[filtername]] == to_obtain[idx,filtername])
          print(head(subset_of_data,2))
        }

        df_statcan <- dplyr::bind_rows(df_statcan, subset_of_data)

        #subset_of_data <- subset_of_data %>%
        #  {if(dplyr::select(., dplyr::any_of(filtername)) %>% ncol == 1){dplyr::filter(., .data[[filtername]] == to_obtain[idx, filtername])}else{.}}
      }
    }


  }

  print(summary(df_statcan))

  # return (
  #   list ( df= df_statcan, to_obtain = to_obtain)
  # )


}

dictionary <- dict_statCan #obtain dictionary


# determine whether user has added additional filters
default_cols <- colnames(dict_statCan)
actual_cols <- colnames(dictionary)
additional_filters <- setdiff(actual_cols, default_cols)

source("determine_codes.R")
source("auxiliary.R")
#extract variables, so that we can search for them in the statsCan database
to_obtain <- determine_variables(specification=spec,dictionary=dictionary)

#build the list of dataframes

#for test we just set additional_filters as default cols of database
additional_filters <- actual_cols
download_statcan(to_obtain, additional_filters, FALSE)


