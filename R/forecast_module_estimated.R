#' Forecast module for estimated relationships
#'
#' This internal function handles forecasts from estimated models (ISAT and CVAR).
#'
#' @param model The full OSEM model object
#' @param i The order of the current module being forecasted
#' @param exog_df_ready The exogenous data frame prepared for forecasting
#' @param exog_df_ready_full The full exogenous data frame prepared for forecasting
#' @param n.ahead Number of steps ahead to forecast
#' @param current_spec The current module specification
#' @param prediction_list The collection of all predictions
#' @param uncertainty_sample The uncertainty sample for forecasts
#' @param nowcasted Nowcasted data for the forecast period
#' @param ci.levels Confidence interval levels for the forecasts
#'
#' @returns A tibble containing the updated prediction_list object with forecasts for the current module
#'
forecast_module_estimated <- function(model,
                                     i,
                                     exog_df_ready,
                                     exog_df_ready_full,
                                     n.ahead,
                                     current_spec,
                                     prediction_list,
                                     uncertainty_sample,
                                     nowcasted,
                                     ci.levels) {

  # get the isat object for this relationship
  isat_obj <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model) %>%
    .[[1]]


  if(inherits(isat_obj, "isat")){

    isat_fcst <- forecast_isat(model = model,
                  i = i,
                  exog_df_ready = exog_df_ready,
                  exog_df_ready_full = exog_df_ready_full,
                  n.ahead = n.ahead,
                  current_spec = current_spec,
                  prediction_list = prediction_list,
                  uncertainty_sample = uncertainty_sample,
                  nowcasted = nowcasted,
                  ci.levels = ci.levels)


    ## 2b.iii. Prepare output for estimated relationships  ------------------------------------------------
    prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::as_tibble(isat_fcst$predict.isat_object)))
    prediction_list[prediction_list$order == i, "data"] <- isat_fcst$final_i_data
    prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(central_estimate = list(isat_fcst$central_estimate))
    prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(all_estimates = list(isat_fcst$pred_draw_matrix))
    prediction_list[prediction_list$order == i, "forecast.metadata"] <- dplyr::tibble(
      forecast.metadata = list(isat_fcst$forecast.metadata)
    )


  }


  if(inherits(isat_obj, "osem.cvar")){

    pred_obj <- forecast_cvar(model = model,
                              i = i,
                              exog_df_ready = exog_df_ready,
                              full_exog_predicted_data = exog_df_ready_full,
                              n.ahead = n.ahead,
                              current_spec = current_spec,
                              prediction_list = prediction_list,
                              uncertainty_sample = uncertainty_sample,
                              nowcasted_data = nowcasted)
    pred_obj$central %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "fcst") %>%
      dplyr::mutate(time = exog_df_ready$time) -> central_estimate

    pred_obj$all %>%
      tidyr::pivot_wider(id_cols = c("time","na_item"), names_from = "iteration", names_prefix = "run_", values_from = "fcst") %>%
      dplyr::mutate(time = rep(exog_df_ready$time,dplyr::n()/length(exog_df_ready$time))) %>%
      dplyr::rename(dep_var = "na_item") -> pred_draw_matrix

    #prediction_list[prediction_list$order == i, "predict.isat_object"] <- NULL
    #prediction_list[prediction_list$order == i, "data"] <- NULL
    prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(central_estimate = list(central_estimate))
    prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(all_estimates = list(pred_draw_matrix))

  }

  return(prediction_list)

}
