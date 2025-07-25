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

    imf_data <- imf.data::load_datasets(id_identified,use_cache = TRUE)
    #get the columns that can be filterable
    query_vars = names(imf_data$dimensions)

    for (idx in indices) {
      query <- list()
      col_filters_idx = seq(column_filters)

      for (col in 1:length(col_filters_idx)) {
        filter_name <- column_filters[col]
        filter = to_obtain[idx,filter_name]

        if (filter_name %in% query_vars | filter_name == "start_period" | filter_name == "end_period")
          query <- c(query,filter)
      }

      #run query
      subset_of_data <- do.call(imf_data$get_series,query)

      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(subset_of_data) == 0L) {
        stop(paste0("For model variable '", to_obtain[idx,1], "', the dataset is empty after applying filters. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[idx, "found"] <- TRUE
      }

      # need to aggregate across all filters
      columns <- colnames(subset_of_data) # columns[1] = TIME_PERIOD, columns[2] = 'the unique identifier that represents the values of the data row'
      value_colname <- columns[2]

      #ensure data is TIME_PERIOD is set to data time
      # first check whether the TIME_PERIOD column is a date object or potentially annual data
      # the TIME_PERIOD column always comes as a character. The question is whether it can be converted to a date or integer without causing an error
      suppressWarnings({
        try({
          timechk_num <- as.numeric(subset_of_data$TIME_PERIOD)
          if(all(is.na(timechk_num))){rm("timechk_num")}
        }, silent = TRUE)
        try(timechk_date <- as.Date(subset_of_data$TIME_PERIOD), silent = TRUE)
        try(timechk_datemod <- as.Date(paste(subset_of_data$TIME_PERIOD, "-01", sep="")), silent = TRUE)
      })

      if(exists("timechk_num")){
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(TIME_PERIOD = as.Date(paste(.data$TIME_PERIOD, "-01-01", sep="")))
      } else if (exists("timechk_date")) {
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(TIME_PERIOD = as.Date(.data$TIME_PERIOD))
      } else if (exists("timechk_datemod")) {
        subset_of_data <- subset_of_data %>%
          dplyr::mutate(TIME_PERIOD = as.Date(paste(.data$TIME_PERIOD, "-01", sep="")))
      } else {
        stop("The TIME_PERIOD column cannot be converted to a date or numeric object. Please check the data source or contact the OSEM Admin to check the IMF function.")

      }

      #convert the value column into numeric
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(VALUE = as.numeric(.data[[value_colname]]))

      #drop old uniquely identified value column
      subset_of_data <- subset_of_data %>%
        dplyr::select(.,-dplyr::all_of(value_colname))


      #if the frequency is monthly we need to aggregate the data to a quarterly level
      if (to_obtain$freq[idx] == "M") {
        unique_columns <- setdiff(columns, columns[2]) # should be unique across these. columns[2] represents the unique value identifier of the dataset
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


