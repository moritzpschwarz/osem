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

#library(statcanR)

download_statcan <- function(to_obtain, column_filters, quiet) {


  # initialise empty df
  df_statcan <- data.frame()

  #get the eurodict colnames
  euro_dict = colnames(dict)

  #pulls dataframe of unique database ids
  dataset_id <- to_obtain %>% dplyr::filter(.data$database == "statcan" &
                                                .data$found == FALSE) %>%
    dplyr::distinct(.data$dataset_id) #%>% dplyr::select("dataset_id")
  browser()
  #iterate over unique database ids and pull the dataset
  #each iteration
  for (i in 1:nrow(dataset_id)) {

    #download data table according to data_base_id
    id <- dataset_id[i,1]
    suppressWarnings(suppressMessages(df <- statcanR::statcan_data(id,"eng")))
    df <- as.data.frame(df)

    df %>%
      rename_with(.fn = tolower) -> df

    #get the dictionary coordinates that use the following dataset_id
    indices <- which(to_obtain$database == "statcan" & to_obtain$dataset_id == id)

    #look through the rows of the dictionary based on the coordinates for the id
    for (idx in indices) {
      subset_of_data <- df

      #iterate through all the column filter names of the particular dictionary row to build filtered dataframe
      col_filters <- column_filters[as.character(column_filters) %in% names(subset_of_data)]
      for (k in col_filters) {
          subset_of_data <- subset_of_data %>% dplyr::filter(.,.[[k]] == as.name(to_obtain[idx,k]))
      }

      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(subset_of_data) == 0L) {
        stop(paste0("For model variable '", to_obtain[idx,1], "', the dataset is empty after applying filters. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[idx, "found"] <- TRUE
      }

      subset_of_data <- subset_of_data %>%
        dplyr::mutate(na_item = to_obtain$model_varname[idx])
      # dplyr::rename_with(.cols = dplyr::all_of(to_obtain$var_col[idx]), .fn = ~paste0("na_item")) %>%

      # if have monthly data, need to aggregate to quarterly
      if (to_obtain$freq[idx] == "m") {
        # need to aggregate across all filters
        columns <- colnames(subset_of_data)
        unique_columns <- setdiff(columns, "value") # should be unique across these
        stopifnot(sum(duplicated(subset_of_data[, unique_columns])) == 0L) # sanity check
        groupby_columns <- union(c("year", "quarter"), setdiff(unique_columns, "ref_date")) # want to group_by year-quarter, so exclude time column
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(year = lubridate::year(.data$ref_date),
                        quarter = lubridate::quarter(.data$ref_date)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groupby_columns))) %>%
          dplyr::summarise(value = sum(.data$value),
                           n = dplyr::n(), # record how many months are available in each quarter
                           ref_date = min(.data$ref_date)) %>%
          dplyr::ungroup()  #%>%
          # drop "incomplete" quarters
          subset_of_data <- subset_of_data %>% dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n")

      }

      #rename ref_date to time
      subset_of_data <- subset_of_data %>% dplyr::rename("time" = "ref_date")

      # ensure column "time" is a Date variable (Moritz had this)
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(time = as.Date(.data$time))

      #rename column headers so they are consistent with the headers used through out the rest of the system
      subset_of_data <- subset_of_data %>% dplyr::rename("values" = "value")
      if ("north american industry classification system (naics)" %in% names(subset_of_data)){
        subset_of_data <- subset_of_data %>% dplyr::rename("naics" = "north american industry classification system (naics)")
      }
      if("seasonal adjustment" %in% names(subset_of_data)){
      subset_of_data <- subset_of_data %>% dplyr::rename("s_adj" = "seasonal adjustment")
      }

      #get the columns that we need to drop that will no longer be used in later calculations
      #this is to keep data frame consistent with how the eurostat frames are processed

      cols_to_remove <- setdiff(colnames(subset_of_data),c(euro_dict,"time","values","na_item","naics"))

      #drop columns that we will not be using
      subset_of_data <- subset_of_data %>% dplyr::select(.,-c(cols_to_remove))


      df_statcan <- dplyr::bind_rows(df_statcan, subset_of_data)
    }
  }

  out <- list()
  out$df = df_statcan
  out$to_obtain = to_obtain

  return(out)
}


