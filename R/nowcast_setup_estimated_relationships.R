#' Internal function to set-up the nowcasting of estimated relationships
#'

#' @param i Current module that is being cycled through
#' @param exog_df_ready Outcome of forecast_exogenous_values() which is the set of forecasted exogenous values
#' @param current_spec The current specification for the module being forecasted
#' @param prediction_list The full list of all predictions. The results of the function will be saved in this list.
#' @param nowcasted_data The full_data element of the model object resulting from the nowcasting() function. Used to substitute missing historical data.
#' @param full_exog_predicted_data An argument to pass a larger data.frame to the function that can contain the entire exogenously predicted data. This is an argument that is needed in the nowcasting() function.
#' @param endog.nowcast An indicator whether the endogenous variable has to be nowcasted before the forecast is carried out. The argument only takes dates.
#' @inheritParams forecast_model
#'
#' @return A list containing, among other elements, the data required to carry out the forecast for this estimated module.
#'
nowcast_setup_estimated_relationships <- function(model,
                                                  i,
                                                  exog_df_ready,
                                                  n.ahead,
                                                  current_spec,
                                                  prediction_list,
                                                  uncertainty_sample,
                                                  nowcasted_data = NULL,
                                                  full_exog_predicted_data = NULL,
                                                  endog.nowcast = NULL) {



  extracted_info <- forecast_extract_info(model = model, i = i, n.ahead = n.ahead, exog_df_ready = exog_df_ready)

  extracted_info$y_names_vec -> y_names_vec
  extracted_info$x_names_vec -> x_names_vec
  extracted_info$x_names_vec_nolag -> x_names_vec_nolag
  extracted_info$ar_vec -> ar_vec
  extracted_info$ylog -> ylog
  extracted_info$xlog -> xlog
  extracted_info$mconst -> mconst
  extracted_info$current_pred_raw -> current_pred_raw
  extracted_info$current_pred_raw_all -> current_pred_raw_all
  extracted_info$is_ardl -> is_ardl
  extracted_info$isat_obj -> isat_obj
  extracted_info$data_obj -> data_obj
  extracted_info$exog_df_ready -> exog_df_ready
  extracted_info$pred_ar_needed -> pred_ar_needed
  extracted_info$pred_dl_needed -> pred_dl_needed

  # checking the data for nowcasted data --------
  data_obj %>%
    dplyr::select("time", dplyr::all_of(x_names_vec_nolag), dplyr::all_of(y_names_vec[1])) -> historical_estimation_data

  # in this section we check whether any of the missing values are present in nowcasted data
  # we first check if there is even any historical data used (would not be true for e.g. AR models)
  if(historical_estimation_data %>% dplyr::select(-"time") %>% ncol() > 0){

    # then we check whether there are any lines in the historical data that are missing (often the case)
    historical_estimation_data %>%
      tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
      dplyr::filter(is.na(.data$values)) -> missing_values_dataobj

    # then we check whether any of the missing values in the historical data are present in nowcasted or in the exog_df_ready data
    if (nrow(missing_values_dataobj) > 0) {

      missing_values_dataobj %>%
        dplyr::mutate(basename = gsub("ln.","",.data$na_item)) %>%

        {if(!is.null(full_exog_predicted_data)){
          dplyr::left_join(.,full_exog_predicted_data %>%
                             tidyr::pivot_longer(-"time",names_to = "basename",
                                                 values_to = "values_exog_full"), by = c("time", "basename")) %>%
            dplyr::mutate(.,values = dplyr::case_when(
              !is.na(.data$values_exog_full) & is.na(.data$values) ~ .data$values_exog_full, TRUE ~ .data$values))

        } else {.}} %>%

        {if(nrow(nowcasted_data)!=0){
          dplyr::left_join(.,nowcasted_data %>%
                             dplyr::rename(basename = "na_item",
                                           values_nowcast = "values"), by = c("time", "basename")) %>%

            # where there are nowcast values but not original ones, take now the nowcast ones
            dplyr::mutate(values = dplyr::case_when(
              !is.na(.data$values_nowcast) & is.na(.data$values) ~ .data$values_nowcast, TRUE ~ .data$values))
        } else {.}} -> missing_vals_intermed

      missing_vals_intermed %>%
        # check if any of the values by na_item are below 0
        dplyr::mutate(log_possible = all(.data$values > 0, na.rm = TRUE), .by = "na_item") %>%
        dplyr::mutate(values_to_log = dplyr::case_when(.data$log_possible & !is.na(.data$values) & grepl("^ln.",.data$na_item) ~ .data$values,
                                                       TRUE ~ NA)) %>%
        # now we check if we need to log them or use asinh
        dplyr::mutate(values = dplyr::case_when(!is.na(.data$values_to_log) ~ log(.data$values_to_log),
                                              !.data$log_possible & !is.na(.data$values) & grepl("^ln.",.data$na_item) ~ asinh(.data$values),
                                              TRUE ~ .data$values)) %>%

        dplyr::select(-c("log_possible","values_to_log")) %>%

        dplyr::select("time", "na_item", new_values = "values") %>%
        tidyr::drop_na() -> values_to_replace

      # Then we take the historical data and add the nowcasted data
      historical_estimation_data %>%
        tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
        dplyr::full_join(values_to_replace, by = c("time","na_item")) %>%
        dplyr::mutate(values = dplyr::case_when(is.na(.data$values) & !is.na(.data$new_values) ~ .data$new_values, TRUE ~ .data$values),
                      new_values = NULL) %>%
        tidyr::pivot_wider(id_cols = "time", names_from = "na_item",values_from = "values") -> historical_estimation_data_w_nowcast

    } else {
      historical_estimation_data_w_nowcast <- historical_estimation_data
    }
  } else {
    historical_estimation_data_w_nowcast <- historical_estimation_data
  }

  # merging nowcasted data with non-x variables (IIS, SIS, etc.) --------
  historical_estimation_data_w_nowcast %>%
    dplyr::bind_rows(current_pred_raw %>%
                       dplyr::select("time", dplyr::all_of(x_names_vec_nolag))) %>%
    dplyr::distinct() -> intermed

  # add the lagged x-variables
  if(ncol(intermed) > 1){
    to_be_added <- dplyr::tibble(.rows = nrow(intermed))
    for (j in pred_dl_needed) {
      if(j == 0){next}
      intermed %>%
        dplyr::transmute(dplyr::across(dplyr::all_of(gsub("L[0-9]+\\.","",j)), ~dplyr::lag(., n = as.numeric(stringr::str_extract(j, "[0-9]+"))))) %>%
        setNames(j) %>%
        dplyr::bind_cols(to_be_added, .) -> to_be_added
    }
    intermed <- dplyr::bind_cols(intermed, to_be_added)
  }

  intermed %>%
    dplyr::left_join(current_pred_raw %>%
                       dplyr::select("time", dplyr::any_of("trend"), dplyr::starts_with("q_"),
                                     dplyr::starts_with("iis"), dplyr::starts_with("sis")),
                     by = "time") %>%

    # only retain the final n.ahead observations
    dplyr::slice(-c(dplyr::n() - n.ahead : dplyr::n())) %>%

    dplyr::select(-"time") %>%
    dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) -> pred_df

  # Final output data -------------------------------------------------------

  final_i_data <- dplyr::tibble(
    data = list(intermed %>%
                  dplyr::left_join(current_pred_raw %>%
                                     dplyr::select("time", dplyr::starts_with("q_"),
                                                   dplyr::starts_with("iis"),
                                                   dplyr::starts_with("sis")),
                                   by = "time") %>%
                  #tidyr::drop_na()))
                  # only retain the final n.ahead observations
                  dplyr::slice(-c(dplyr::n() - n.ahead : dplyr::n()))
    )
  )


  out <- list()
  out$pred_df <- pred_df
  out$isat_obj <- isat_obj
  out$final_i_data <- final_i_data
  out$current_pred_raw <- current_pred_raw
  out$n.ahead <- n.ahead

  return(out)

}
