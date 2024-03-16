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
          print(head(subset_of_data,2))
        }
      }

      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(subset_of_data) == 0L) {
        stop(paste0("For model variable '", to_obtain[idx,1], "', the dataset is empty after applying filters. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[idx, "found"] <- TRUE
      }


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

      df_statcan <- dplyr::bind_rows(df_statcan, subset_of_data)
      print(head(subset_of_data,2))
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

#for test we just set column_filters as default cols of database
column_filters <- actual_cols
download_statcan(to_obtain, column_filters, FALSE)


