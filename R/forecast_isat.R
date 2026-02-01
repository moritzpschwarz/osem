#' Forecast ISAT Module (internal)
#'
#' This internal function generates forecasts from an ISAT (Indicator Saturation) model within the OSEM framework (see \code{\link[gets]{isat}}).
#'
#' @param model The overall 'osem' model as returned by \code{\link[osem]{run_model}}
#' @param i The index of the current module within the model's module collection
#' @param exog_df_ready The exogenous data frame prepared for forecasting
#' @param exog_df_ready_full The full exogenous data frame prepared for forecasting
#' @param n.ahead Number of steps ahead to forecast
#' @param current_spec The current specification for the module being forecasted
#' @param prediction_list The collection of all predictions
#' @param uncertainty_sample The number of uncertainty samples to draw for the prediction
#' @param ci.levels The confidence interval levels for the prediction
#' @param nowcasted The nowcasted data for the model
#'
#' @returns A tibble containing the updated prediction_list object with forecasts for the current module
#'
forecast_isat <- function(
    model,
    i,
    exog_df_ready,
    exog_df_ready_full,
    n.ahead,
    current_spec,
    prediction_list,
    uncertainty_sample,
    ci.levels,
    nowcasted){

  # get the isat object for this relationship
  isat_obj <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model) %>%
    .[[1]]

  pred_setup_list <- forecast_setup_estimated_relationships(
    model = model,
    i = i,
    exog_df_ready = exog_df_ready,
    full_exog_predicted_data = exog_df_ready_full,
    n.ahead = n.ahead,
    current_spec = current_spec,
    prediction_list = prediction_list,
    uncertainty_sample = uncertainty_sample,
    nowcasted_data = nowcasted
  )

  final_i_data <- pred_setup_list$final_i_data
  pred_df <- pred_setup_list$pred_df
  chk_any_listcols <- pred_setup_list$chk_any_listcols
  current_pred_raw <- pred_setup_list$current_pred_raw

  if (!is.null(pred_setup_list$pred_df.all)) {
    pred_df.all <- pred_setup_list$pred_df.all
  }

  ### 2b.i. Predict main estimate for estimated relationships  ------------------------------------------------
  isat_obj$call$ar <- isat_obj$aux$args$ar
  isat_obj$call$mc <- isat_obj$aux$args$mc
  isat_obj$call$tis <- isat_obj$aux$args$tis

  pred_obj <- gets::predict.isat(isat_obj,
                                 newmxreg = as.matrix(utils::tail(
                                   pred_df %>% dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)),
                                   n.ahead
                                 )),
                                 n.ahead = n.ahead,
                                 plot = FALSE,
                                 ci.levels = ci.levels)

  # make samples from the model residuals and add them to the mean prediction
  # use machine precision to determine whether close to zero
  #tolerance <- sqrt(.Machine$double.eps)
  #res_nozero <- as.numeric(isat_obj$residuals)[abs(as.numeric(isat_obj$residuals)) > tolerance] # exclude 0 residuals (due to IIS) to not underestimate uncertainty
  iis_index <- gets::isatdates(isat_obj)$iis$index

  # Only exclude indices if they exist and are numeric
  if (!is.null(iis_index) && length(iis_index) > 0 && is.numeric(iis_index)) {
    res_nozero <- isat_obj$residuals[-iis_index]
  } else {
    res_nozero <- isat_obj$residuals
  }

  if (length(res_nozero) == 0) {
    # if all observations in isat are saturated - all observations are IIS
    # should not happen, but there might be edge cases
    res_draws <- rep(0, uncertainty_sample * n.ahead)
  } else {
    res_draws <- sample(as.numeric(res_nozero), size = uncertainty_sample * n.ahead, replace = TRUE)
  }


  # create a tibble with all res_draws with the same number of rows as n.ahead
  res_names <- paste0("run_", 1:(length(res_draws) / n.ahead))
  dplyr::as_tibble(matrix(res_draws, nrow = n.ahead, dimnames = list(NULL, res_names))) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), cumsum)) %>%
    as.matrix() -> res_draws_matrix

  pred_draw_matrix <- as.vector(pred_obj$yhat) + res_draws_matrix

  ## 2b.ii. Predict uncertainty plume for estimated relationships  ------------------------------------------------

  # if there are any list columns then that means that a preceding variable has uncertainty
  # then the pred_draw_matrix is replaced with the uncertainty estimates
  if (chk_any_listcols) {
    # first find the list columns - these indicate that there has been uncertainty in preceding variables
    list_cols <- names(pred_df.all)[sapply(pred_df.all, "class") == "list"]
    time_values <- current_pred_raw$time

    # in this following dataset we write the list columns properly formatted
    overall_listcols <- dplyr::tibble(time = time_values) %>%
      # dplyr::full_join(dplyr::tibble(run = 1:uncertainty_sample), by = character())
      dplyr::cross_join(dplyr::tibble(run = 1:uncertainty_sample))

    for (m in list_cols) {
      # m = list_cols[1]

      # we extract the list columns into individual lists
      lapply(pred_df.all %>% dplyr::pull(m), FUN = function(x) {
        # if the value is just a number (must be due to it being a lagged observed value), then just take that value
        if (is.numeric(x)) {
          x
        } else if (is.data.frame(x)) {
          # if this is already a dataframe, then pivot it to longer
          tidyr::pivot_longer(x, dplyr::everything(), names_to = "run") %>%
            dplyr::mutate(run = as.numeric(grep("[0-9]+$", .data$run)))
        }
      }) -> listcol_unformatted

      # and now we reformat them to a long dataset
      # now we combine the individual formatted list columns to one data frame
      # for that, we cycle through each element of the formatted list columns
      listcol_formatted <- dplyr::tibble()
      for (l in 1:length(listcol_unformatted)) {
        if (is.numeric(listcol_unformatted[[l]])) {
          dplyr::tibble(
            time = time_values[l],
            run = 1:uncertainty_sample,
            value = listcol_unformatted[[l]]
          ) %>%
            dplyr::bind_rows(., listcol_formatted) -> listcol_formatted
        } else {
          dplyr::tibble(
            time = time_values[l],
            listcol_unformatted[[l]]
          ) %>%
            dplyr::bind_rows(., listcol_formatted) -> listcol_formatted
        }
      }
      listcol_formatted <- dplyr::arrange(listcol_formatted, .data$time, .data$run)
      names(listcol_formatted) <- c("time", "run", m)

      dplyr::full_join(overall_listcols, listcol_formatted, by = c("time", "run")) -> overall_listcols
    }

    # now that we have all list columns properly formatted in one dataset, we join them with the rest of the columns
    pred_df.all_new <- pred_df.all %>%
      dplyr::mutate(time = time_values) %>%
      # we can delete the list_cols, because those will now be added in their formatted version
      dplyr::select(-dplyr::all_of(list_cols)) %>%
      dplyr::full_join(overall_listcols, ., by = "time")

    pred_df.all_new %>%
      # we nest all data so that each run is one line
      tidyr::nest(data = c(dplyr::everything(), -"run")) %>%
      # now for each run-row, we run predict.isat
      dplyr::mutate(prediction = purrr::map(.data$data, function(x) {
        gets::predict.isat(isat_obj,
                           newmxreg = x %>%
                             dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)) %>%
                             utils::tail(n.ahead) %>%
                             as.matrix(),
                           n.ahead = n.ahead, plot = FALSE,
                           ci.levels = ci.levels, n.sim = 1
        )
      })) -> all_preds

    all_preds %>%
      # we get all predictions back into a row format
      dplyr::mutate(prediction = purrr::map(.data$prediction, dplyr::as_tibble)) %>%
      tidyr::unnest("prediction") %>%
      # we add the time dimension
      dplyr::mutate(time = time_values, .by = "run") %>%
      dplyr::select("run", "time", pred = "yhat") %>%
      # here pred only takes into account the uncertainty in the x-variables
      # pred_draws combines the model residual uncertainty for y and the uncertainty of the x-variables
      dplyr::mutate(
        pred_draws = .data$pred + res_draws,
        pred = NULL
      ) %>%
      # now we get them into the final format to add them back to the overall list
      tidyr::pivot_wider(id_cols = "time", names_from = "run", values_from = "pred_draws") %>%
      dplyr::select(-"time") %>%
      as.matrix() -> pred_runs_final_matrix

    dimnames(pred_runs_final_matrix) <- NULL

    # now replace the pred_draw_matrix - this one only survives without being overwritten if there is no preceding uncertainty
    pred_draw_matrix <- pred_runs_final_matrix
  }
  outvarname <- paste0(
    if (model$module_collection %>%
        dplyr::filter(.data$order == i) %>%
        .$model.args %>%
        .[[1]] %>%
        .$use_logs %in% c("both", "y")) {
      "ln."
    } else {
      ""
    },
    current_spec %>% dplyr::pull("dependent") %>% unique()
  )

  dplyr::tibble(
    time = current_pred_raw %>% dplyr::pull(.data$time),
    value = as.numeric(pred_obj[, 1])) %>%
    setNames(c("time", outvarname)) -> central_estimate

  colnames(pred_draw_matrix) <- res_names
  pred_draw_matrix <- dplyr::as_tibble(pred_draw_matrix) %>%
    dplyr::bind_cols(dplyr::tibble(time = current_pred_raw$time), .)

  return(list(central_estimate = central_estimate,
              pred_draw_matrix = pred_draw_matrix,
              predict.isat_object = pred_obj,
              final_i_data  = pred_setup_list$final_i_data))


}
