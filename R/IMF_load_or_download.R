#' Download IMF
#'
#' Downloads IMF data given a data.frame of required variables
#'
#' @inheritParams load_or_download_variables
#' @param to_obtain A data.frame as returned by [determine_variables()].
#' @param column_filters A character vector storing the column names of
#' filters. Length 0 if not present.
#'
#' @return Returns a list with two named elements: \code{$df} stores the
#' downloaded data and \code{$to_obtain} the updated data.frame tracking which
#' variables still need to be obtained.
#'

download_imf <- function(to_obtain, column_filters, quiet) {

  # initialise empty df
  df_imf <- data.frame()

  #get the eurodict colnames
  euro_dict = colnames(osem::dict)

  #pulls dataframe of unique database ids
  dataset_id <- to_obtain %>% dplyr::filter(.data$database == "imf" &
                                              .data$found == FALSE) %>%
    dplyr::distinct(.data$dataset_id) #%>% dplyr::select("dataset_id")

  #iterate over unique database ids and pull the dataset
  #each iteration
  for (i in 1:nrow(dataset_id)) {
    id_identified <- dataset_id[i,1, drop = TRUE]

    #get the dictionary coordinates that use the following dataset_id
    indices <- which(to_obtain$database == "imf" & to_obtain$dataset_id == id_identified)

    imf_dimensions <- imf.data::list_dimensions(id_identified)

    #get the columns that can be filterable
    query_vars = imf_dimensions$id

    for (idx in indices) {
      query <- list()
      col_filters_idx = seq(column_filters)

      for (col in 1:length(col_filters_idx)) {
        filter_name <- column_filters[col]
        filter = to_obtain[[filter_name]][idx]

        if (filter_name %in% query_vars)
          query[[filter_name]] <- filter
      }

      start_period <- NULL
      end_period <- NULL

      if ("start_period" %in% column_filters)
        start_period <- to_obtain[["start_period"]][idx]

      if ("end_period" %in% column_filters)
        end_period <- to_obtain[["end_period"]][idx]

      #run query
      subset_of_data <- imf.data::get_data(
        dataflow = id_identified,
        filters = query,
        start_period = start_period,
        end_period = end_period
      )

      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(subset_of_data) == 0L) {
        stop(paste0("For model variable '", to_obtain[idx,1], "', the dataset is empty after applying filters. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[idx, "found"] <- TRUE
      }

      # need to aggregate across all filters
      columns <- colnames(subset_of_data)
      value_colname <- "OBS_VALUE"

      #ensure data is TIME_PERIOD is set to data time
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(TIME_PERIOD = as.Date(as.Date(paste(.data$TIME_PERIOD, "-01", sep=""))))

      #convert the value column into numeric
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(VALUE = as.numeric(.data[[value_colname]]))

      #drop old uniquely identified value column
      subset_of_data <- subset_of_data %>%
        dplyr::select(.,-dplyr::all_of(value_colname))


      #if the frequency is monthly we need to aggregate the data to a quarterly level
      if (to_obtain$freq[idx] == "M") {
        unique_columns <- setdiff(columns, value_colname) # should be unique across these. value_colname represents the value column of the dataset
        stopifnot(sum(duplicated(subset_of_data[, unique_columns])) == 0L) # sanity check
        groupby_columns <- union(c("year", "quarter"), setdiff(unique_columns, "TIME_PERIOD")) # want to group_by year-quarter, so exclude time column
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(year = lubridate::year(.data$TIME_PERIOD),
                        quarter = lubridate::quarter(.data$TIME_PERIOD)) %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groupby_columns))) %>%
          dplyr::summarise(VALUE = sum(.data$VALUE),
                           n = dplyr::n(), # record how many months are available in each quarter
                           TIME_PERIOD = min(.data$TIME_PERIOD)) %>%
          dplyr::ungroup()  #%>%
        # drop "incomplete" quarters
        subset_of_data <- subset_of_data %>% dplyr::filter(.data$n == 3L) %>%
          dplyr::select(-"year", -"quarter", -"n")

      }

      #add na_item (model_varname)
      subset_of_data <- subset_of_data %>% dplyr::mutate(na_item = to_obtain$model_varname[idx])
      #add geo
      subset_of_data <- subset_of_data %>% dplyr::mutate(geo = to_obtain$ref_area[idx])

      #rename REF_DATE to time
      subset_of_data <- subset_of_data %>% dplyr::rename("time" = "TIME_PERIOD")

      # rename VALUE to values
      subset_of_data <- subset_of_data %>% dplyr::rename("values" = "VALUE")

      # ensure column "time" is a Date variable
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(time = as.Date(.data$time))

      #get the columns that we need to drop that will no longer be used in later calculations
      #this is to keep data frame consistent with how the eurostat frames are processed
      cols_to_remove <- setdiff(colnames(subset_of_data),c(euro_dict,"time","values","na_item","nace_r2"))

      #drop columns that we will not be using
      subset_of_data <- subset_of_data %>% dplyr::select(.,-c(dplyr::all_of(cols_to_remove)))

      df_imf  <- dplyr::bind_rows(df_imf, subset_of_data)

    }

  }

  out <- list()
  out$df <- df_imf
  out$to_obtain <- to_obtain

  return(out)
}


