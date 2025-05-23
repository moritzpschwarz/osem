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

  #get the dictionary colnames
  dict_colnames = colnames(osem::dict)

  #pulls dataframe of unique database ids
  dataset_id <- to_obtain %>% dplyr::filter(.data$database == "statcan" &
                                                .data$found == FALSE) %>%
    dplyr::distinct(.data$dataset_id) #%>% dplyr::select("dataset_id")

  #iterate over unique database ids and pull the dataset
  #each iteration
  for (i in 1:nrow(dataset_id)) {

    #download data table according to data_base_id
    id_identified <- dataset_id[i,1, drop = TRUE]
    suppressWarnings(suppressMessages(df <- statcanR::statcan_data(id_identified,"eng")))
    df <- as.data.frame(df)

    #get the dictionary coordinates that use the following dataset_id
    indices <- which(to_obtain$database == "statcan" & to_obtain$dataset_id == id_identified)

    #look through the rows of the dictionary based on the coordinates for the id
    for (idx in indices) {
      subset_of_data <- df

      #iterate through all the column filter names of the particular dictionary row to build filtered dataframe
      col_filters <- column_filters[as.character(column_filters) %in% names(subset_of_data)]
      for (k in col_filters) {
          subset_of_data <- subset_of_data %>% dplyr::filter(.,.[[k]] == as.name(to_obtain[idx,k, drop = TRUE]))
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
        unique_columns <- setdiff(columns, "VALUE") # should be unique across these
        stopifnot(sum(duplicated(subset_of_data[, unique_columns])) == 0L) # sanity check
        groupby_columns <- union(c("year", "quarter"), setdiff(unique_columns, "REF_DATE")) # want to group_by year-quarter, so exclude time column
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(year = lubridate::year(.data$REF_DATE),
                        quarter = lubridate::quarter(.data$REF_DATE)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groupby_columns))) %>%
          dplyr::summarise(VALUE = sum(.data$VALUE),
                           n = dplyr::n(), # record how many months are available in each quarter
                           REF_DATE = min(.data$REF_DATE)) %>%
          dplyr::ungroup()  #%>%
          # drop "incomplete" quarters
          subset_of_data <- subset_of_data %>% dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n")

      }

      #rename REF_DATE to time
      subset_of_data <- subset_of_data %>% dplyr::rename("time" = "REF_DATE")

      # ensure column "time" is a Date variable
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(time = as.Date(.data$time))

      if("GEO" %in% names(subset_of_data)){
        subset_of_data <- subset_of_data %>% dplyr::rename("geo" = "GEO")
      }
      if("VALUE" %in% names(subset_of_data)){
        subset_of_data <- subset_of_data %>% dplyr::rename("values" = "VALUE")
      }

      #get the columns that we need to drop that will no longer be used in later calculations
      #this is to keep data frame consistent with how the eurostat frames are processed
      cols_to_remove <- setdiff(colnames(subset_of_data),c(dict_colnames,"time","values","na_item"))

      #drop columns that we will not be using
      subset_of_data <- subset_of_data %>% dplyr::select(.,-c(dplyr::all_of(cols_to_remove)))


      df_statcan <- dplyr::bind_rows(df_statcan, subset_of_data)
    }
  }

  out <- list()
  out$df = df_statcan
  out$to_obtain = to_obtain

  return(out)
}


