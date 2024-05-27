#' An internal function that checks the frequencies of the model values
#'
#' This function checks and deals with different types of mixed frequency models. When there is a mixed frequency within a variable, it tries to remove
#' @param full_data The full dataset that is used in the model
#' @inheritParams run_model
#'
#' @return A list with the updated full_data and the frequency of the data
#'
check_frequencies <- function(full_data, quiet){

  # Within variable frequencies ---------------------------------------------

  full_data %>%
    #filter(na_item == "Unemployment") %>%
    dplyr::arrange(.data$na_item, .data$time) %>%
    dplyr::mutate(next_t = dplyr::lead(.data$time),
                  diff_num = as.numeric(.data$next_t-time),
                  diff = dplyr::case_when(.data$diff_num == 1 ~ "day",
                                          .data$diff_num %in% c(28:31) ~ "month",
                                          .data$diff_num %in% c(90:92) ~ "3 months",
                                          .data$diff_num %in% c(365:366) ~ "year"),
                  .by = "na_item")  -> frq_df



  frq_df_backup <- frq_df

  # check whether for each na_item whether are different frequencies
  # if there are different frequencies, check whether they are only at the start of the sample
  # and then check whether they are increasing in terms of frequency e.g. first year and then quarter
  # if they are not increasing in frequency, throw an error
  # if they are increasing in frequency, then check whether they are only at the start of the sample
  # if they are only at the start of the sample, then remove the observations until the first observation where only the most frequent frequencies follows

  frq_df %>%
    tidyr::drop_na("diff") %>%
    dplyr::distinct(.data$na_item,.data$diff) %>%
    dplyr::mutate(n = dplyr::n(), .by = "na_item") %>%
    dplyr::filter(.data$n > 1) %>%
    dplyr::distinct(.data$na_item) %>%
    dplyr::pull() -> mixed_frq

  # a loop that will one by one remove the lowest observation that is not the highest frequency
  for(i in mixed_frq){
    # i = "Unemployment"
    frq_df %>%
      dplyr::filter(.data$na_item == i) %>%
      dplyr::arrange(.data$na_item, .data$time) %>%
      dplyr::mutate(freq_rank = dplyr::case_when(.data$diff == "year" ~ 1,
                                                 .data$diff == "3 months" ~ 2,
                                                 .data$diff == "month" ~ 3,
                                                 .data$diff == "day" ~ 4),
                    index = 1:dplyr::n()) -> temp_df

    temp_df_backup <- temp_df

    while(any(temp_df %>% tidyr::drop_na(.data$freq_rank) %>% dplyr::pull(.data$freq_rank) < max(temp_df$freq_rank, na.rm = TRUE))){
      temp_df %>%
        # the last observation will never have a freq_rank (as it does not have a next_t)
        # we just use the one from the period before that
        dplyr::mutate(freq_rank = dplyr::case_when(is.na(freq_rank) ~ dplyr::lag(freq_rank),TRUE ~ freq_rank)) %>%

        # find the highest frequency
        dplyr::mutate(max_rank = max(.data$freq_rank, na.rm = TRUE)) %>%
        # remove the lowest index that is not the highest frequency
        dplyr::filter(!(.data$freq_rank != .data$max_rank & .data$index == min(.data$index))) -> temp_df
    }
    temp_df_backup %>%
      dplyr::filter(.data$index %in% temp_df$index) -> temp_df_final

    frq_df %>%
      dplyr::filter(na_item != i) %>%
      dplyr::bind_rows(temp_df_final %>% dplyr::select(-c("index","freq_rank"))) %>%
      dplyr::arrange(.data$na_item,.data$time) -> frq_df
  }

  out <- list()

  # frequencies across all variables ---------------------------------------------
  frq_df %>%
    # the last observation will never have a diff (as it does not have a next_t)
    # we just use the one from the period before that
    dplyr::mutate(diff = dplyr::case_when(is.na(diff) ~ dplyr::lag(diff),TRUE ~ diff)) %>%
    dplyr::distinct(.data$diff) %>%
    dplyr::pull(.data$diff) -> frequency

  if(length(frequency) > 1){

    if(all(c("year","3 months") %in% frequency)){
      full_data %>%
        dplyr::mutate(year = lubridate::year(.data$time),
                      quarter = lubridate::quarter(.data$time)) %>%
        dplyr::mutate(values = sum(.data$values, na.rm = TRUE), .by = c("na_item","year")) %>%
        dplyr::filter(.data$quarter == 1) %>%
        dplyr::select(-dplyr::all_of(c("year", "quarter"))) -> full_data
    } else {
      # frequency == "month" | frequency == "day")
      stop("Mixed frequency models are not yet implemented. Please check to make sure that all data that you supply to the model (incl. in the dictionary) has the same frequency.")
    }
  } else {
    if(nrow(frq_df)/nrow(frq_df_backup) < 0.9){
      if(!quiet){
        warning(paste0("\n",
        "Some observations were removed from the data due to different frequencies.\n",
        "This does not necessarily result in an error but please check the data to make sure that the frequencies are consistent.\n",
        "Suppress this warning by setting quiet = TRUE.\n"))
      }
    }
    temp_ids <- frq_df %>%
      dplyr::mutate(id = paste0(time, na_item)) %>%
      dplyr::pull(id)

    full_data %>%
      dplyr::mutate(id = paste0(time, na_item)) %>%
      dplyr::filter(.data$id %in% temp_ids) %>%
      dplyr::select(-"id") -> full_data
  }

  out$full_data <- full_data
  out$frequency <- frequency

  return(out)

}
