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
  euro_dict = colnames(aggregate.model::dict)

  # browser()
  #pulls dataframe of unique database ids
  dataset_id <- to_obtain %>% dplyr::filter(.data$database == "imf" &
                                              .data$found == FALSE) %>%
    dplyr::distinct(.data$dataset_id) #%>% dplyr::select("dataset_id")

  #iterate over unique database ids and pull the dataset
  #each iteration
  for (i in 1:nrow(dataset_id)) {


    id <- dataset_id[i,1]

    #get the dictionary coordinates that use the following dataset_id
    indices <- which(to_obtain$database == "imf" & to_obtain$dataset_id == id)


    for (idx in indices) {

      query <- list()
      col_filters_idx = seq(column_filters)


      for (col in 11:length(col_filters_idx)) {
        filter_name <- column_filters[col]
        filter = to_obtain[idx,filter_name]

        if ( !(is.na(filter)) ) {
          query <- c(query,filter)
        }
        if (filter_name == "start_period" | filter_name == "end_period")
          query <- c(query,filter)
      }

      #run query
      imf_data <- imf.data::load_datasets(id,use_cache = FALSE)
      subset_of_data <- do.call(imf_data$get_series,query)

      # if after filtering "sub" is not empty, we found the variable and can mark it as such
      if (NROW(subset_of_data) == 0L) {
        stop(paste0("For model variable '", to_obtain[idx,1], "', the dataset is empty after applying filters. Check whether the dictionary and the data source for changes and errors (i.e. name of units, etc.)"))
      } else {
        to_obtain[idx, "found"] <- TRUE
      }

      #browser()
      # need to aggregate across all filters
      columns <- colnames(subset_of_data) # columns[1] = TIME_PERIOD, columns[2] = 'the unique identifier that represents the values of the data row'
      value <- columns[2]

      #ensure data is TIME_PERIOD is set to data time
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(TIME_PERIOD = as.Date(as.Date(paste(.data$TIME_PERIOD, "-01", sep=""))))

      #convert the value column into numeric
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(VALUE = as.numeric(.data[[value]]))

      #drop old uniquely identified value column
      subset_of_data <- subset_of_data %>%
        dplyr::select(.,-c(value))



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

      # ensure column "time" is a Date variable (Moritz had this)
      subset_of_data <- subset_of_data %>%
        dplyr::mutate(time = as.Date(.data$time))

      ##TO DO this needs to be altered in some what when there is s_adj and nace_r2 values present
      # in dataframe
      #rename column headers so they are consistent with the headers used through out the rest of the system

      #for now we will just set these values to NA
      subset_of_data <- subset_of_data %>% dplyr::mutate(nace_r2 = NA)
      subset_of_data <- subset_of_data %>% dplyr::mutate(s_adj = NA)



      subset_of_data <- subset_of_data %>% dplyr::rename("values" = "VALUE")
      if ("North American Industry Classification System (NAICS)" %in% names(subset_of_data)){
        subset_of_data <- subset_of_data %>% dplyr::rename("nace_r2" = "North American Industry Classification System (NAICS)")
      }
      if("Seasonal adjustment" %in% names(subset_of_data)){
        subset_of_data <- subset_of_data %>% dplyr::rename("s_adj" = "Seasonal adjustment")
      }


      #get the columns that we need to drop that will no longer be used in later calculations
      #this is to keep data frame consistent with how the eurostat frames are processed

      cols_to_remove <- setdiff(colnames(subset_of_data),c(euro_dict,"time","values","na_item","nace_r2"))

      #drop columns that we will not be using
      subset_of_data <- subset_of_data %>% dplyr::select(.,-c(cols_to_remove))


      df_imf  <- dplyr::bind_rows(df_imf, subset_of_data)
      #write.csv(df_statcan, "/Users/geoffreyharper/Desktop/statcan_data.csv", row.names=FALSE)
      #print(head(subset_of_data,2))

    }

      #What is the point of this??
      # sub <- sub %>%
      #   dplyr::rename_with(.cols = dplyr::all_of(varcolname), .fn = ~paste0("na_item")) %>%
      #   dplyr::mutate(na_item = to_obtain$model_varname[j])

  }

  return (
    list ( df= df_imf, to_obtain = to_obtain)
  )


}


