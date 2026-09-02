#' Forecast ISAT Module (internal)
#'
#' This internal function generates forecasts from an ISAT (Indicator Saturation)
#' model within the OSEM framework (see \code{\link[gets]{isat}}).
#' It forecasts an estimated single-equation module. ARDL and fully differenced
#' equations use \code{\link[gets]{predict.isat}} for the complete
#' forecast horizon. ECM equations are forecast recursively because the
#' forecast for one period is needed to construct the lagged dependent level
#' for the following period.
# @param model The overall 'osem' model as returned by \code{\link[osem]{run_model}}
# @param i The index of the current module within the model's module collection
# @param exog_df_ready The exogenous data frame prepared for forecasting
# @param exog_df_ready_full The full exogenous data frame prepared for forecasting
# @param n.ahead Number of steps ahead to forecast
# @param current_spec The current specification for the module being forecasted
# @param prediction_list The collection of all predictions
# @param uncertainty_sample The number of uncertainty samples to draw for the prediction
# @param ci.levels The confidence interval levels for the prediction
# @param nowcasted The nowcasted data for the model
#' @inheritParams forecast_model
#' @inheritParams forecast_setup_estimated_relationships
#'
#' @returns A list containing the central path, uncertainty paths, prediction
#'   object, and data used for the current module.
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

  # Prepare the data required for forecasting -------------------------------
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

  isat_obj <- pred_setup_list$isat_obj
  recipe <- pred_setup_list$recipe
  # final_i_data <- pred_setup_list$final_i_data
  # pred_df <- pred_setup_list$pred_df
  # chk_any_listcols <- pred_setup_list$chk_any_listcols
  # current_pred_raw <- pred_setup_list$current_pred_raw
  #
  # if (!is.null(pred_setup_list$pred_df.all)) {
  #   pred_df.all <- pred_setup_list$pred_df.all
  # }

  # Restore the arguments required by predict.isat(). These are not always
  # retained in the model call after model selection.
  isat_obj$call$ar <- isat_obj$aux$args$ar
  isat_obj$call$mc <- isat_obj$aux$args$mc
  isat_obj$call$tis <- isat_obj$aux$args$tis

  # Draw residual uncertainty -----------------------------------------------
  # make samples from the model residuals and add them to the mean prediction
  # IIS observations are excluded because their residuals are zero by
  # construction and would therefore understate forecast uncertainty.
  iis_index <- gets::isatdates(isat_obj)$iis$index
  residuals <- as.numeric(isat_obj$residuals)

  # Only exclude indices if they exist and are numeric
  if (!is.null(iis_index) && length(iis_index) > 0 && is.numeric(iis_index)) {
    residuals <- residuals[-iis_index]
  }

  residuals <- residuals[!is.na(residuals)]

  if (length(residuals) == 0) {
    # if all observations in isat are saturated - all observations are IIS
    # should not happen, but there might be edge cases
    residual_draws <- matrix(
      0,
      nrow = n.ahead,
      ncol = uncertainty_sample
    )
  } else {
    residual_draws <- matrix(
      sample(
        residuals,
        size = n.ahead * uncertainty_sample,
        replace = TRUE
      ),
      nrow = n.ahead,
      ncol = uncertainty_sample
    )
  }

  forecast_times <- utils::tail(
    pred_setup_list$current_pred_raw$time,
    n.ahead
  )
  outvarname <- recipe$transformed_level_name
  run_names <- paste0("run_", seq_len(uncertainty_sample))

  # # create a tibble with all res_draws with the same number of rows as n.ahead
  # res_names <- paste0("run_", 1:(length(res_draws) / n.ahead))
  # dplyr::as_tibble(matrix(res_draws, nrow = n.ahead, dimnames = list(NULL, res_names))) %>%
  #   dplyr::mutate(dplyr::across(dplyr::everything(), cumsum)) %>%
  #   as.matrix() -> res_draws_matrix
  # Forecast an ECM recursively ---------------------------------------------
  if (recipe$model_form == "ecm") {
    level_name <- recipe$transformed_level_name

    if (!level_name %in% names(pred_setup_list$state_data)) {
      stop(
        "The stored forecast recipe expects dependent-variable state '",
        level_name,
        "', but it is absent from the prepared module data."
      )
    }

    recursive <- forecast_recursive_isat(
      isat_obj = isat_obj,
      recipe = recipe,
      central_terms = pred_setup_list$pred_df,
      draw_terms = pred_setup_list$pred_df.all,
      level_history = pred_setup_list$state_data[[level_name]],
      residual_draws = residual_draws
    )

    central_level <- recursive$central_level
    pred_draw_matrix <- recursive$draw_level
    pred_obj <- dplyr::tibble(yhat = recursive$central_response)
  } else {

    # Forecast ARDL and differenced equations with predict.isat() ------------

    pred_obj <- gets::predict.isat(isat_obj,
                                   newmxreg = as.matrix(
                                     pred_setup_list$pred_df %>%
                                       dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)) %>%
                                       utils::tail(n.ahead)
                                   ),
                                   quiet = TRUE,
                                   n.ahead = n.ahead,
                                   plot = FALSE,
                                   ci.levels = ci.levels
    )

    central_response <- as.numeric(pred_obj$yhat)

    # If an upstream module has uncertainty, predict the equation once for
    # each of its forecast paths. Otherwise, each run starts from the same
    # central equation prediction.
    if (is.null(pred_setup_list$pred_df.all)) {
      predicted_draws <- matrix(
        central_response,
        nrow = n.ahead,
        ncol = uncertainty_sample
      )
    } else {
      predicted_draws <- vapply(
        pred_setup_list$pred_df.all,
        function(path) {
          path_prediction <- gets::predict.isat(
            isat_obj,
            newmxreg = as.matrix(
              path %>%
                dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)) %>%
                utils::tail(n.ahead)
            ),
            quiet = TRUE,
            n.ahead = n.ahead,
            plot = FALSE,
            ci.levels = ci.levels
          )

          return(as.numeric(path_prediction$yhat))
        },
        numeric(n.ahead)
      )
    }
    # # if there are any list columns then that means that a preceding variable has uncertainty
    # # then the pred_draw_matrix is replaced with the uncertainty estimates
    # if (chk_any_listcols) {
    #   # first find the list columns - these indicate that there has been uncertainty in preceding variables
    #   list_cols <- names(pred_df.all)[sapply(pred_df.all, "class") == "list"]
    #   time_values <- current_pred_raw$time
    #
    #   # in this following dataset we write the list columns properly formatted
    #   overall_listcols <- dplyr::tibble(time = time_values) %>%
    #     # dplyr::full_join(dplyr::tibble(run = 1:uncertainty_sample), by = character())
    #     dplyr::cross_join(dplyr::tibble(run = 1:uncertainty_sample))
    #
    #   for (m in list_cols) {
    #     # m = list_cols[1]
    #
    #     # we extract the list columns into individual lists
    #     lapply(pred_df.all %>% dplyr::pull(m), FUN = function(x) {
    #       # if the value is just a number (must be due to it being a lagged observed value), then just take that value
    #       if (is.numeric(x)) {
    #         x
    #       } else if (is.data.frame(x)) {
    #         # if this is already a dataframe, then pivot it to longer
    #         tidyr::pivot_longer(x, dplyr::everything(), names_to = "run") %>%
    #           dplyr::mutate(run = as.numeric(grep("[0-9]+$", .data$run)))
    #       }
    #     }) -> listcol_unformatted
    #
    #     # and now we reformat them to a long dataset
    #     # now we combine the individual formatted list columns to one data frame
    #     # for that, we cycle through each element of the formatted list columns
    #     listcol_formatted <- dplyr::tibble()
    #     for (l in 1:length(listcol_unformatted)) {
    #       if (is.numeric(listcol_unformatted[[l]])) {
    #         dplyr::tibble(
    #           time = time_values[l],
    #           run = 1:uncertainty_sample,
    #           value = listcol_unformatted[[l]]
    #         ) %>%
    #           dplyr::bind_rows(., listcol_formatted) -> listcol_formatted
    #       } else {
    #         dplyr::tibble(
    #           time = time_values[l],
    #           listcol_unformatted[[l]]
    #         ) %>%
    #           dplyr::bind_rows(., listcol_formatted) -> listcol_formatted
    #       }
    #     }
    #     listcol_formatted <- dplyr::arrange(listcol_formatted, .data$time, .data$run)
    #     names(listcol_formatted) <- c("time", "run", m)
    #
    #     dplyr::full_join(overall_listcols, listcol_formatted, by = c("time", "run")) -> overall_listcols
    #   }
    #
    #   # now that we have all list columns properly formatted in one dataset, we join them with the rest of the columns
    #   pred_df.all_new <- pred_df.all %>%
    #     dplyr::mutate(time = time_values) %>%
    #     # we can delete the list_cols, because those will now be added in their formatted version
    #     dplyr::select(-dplyr::all_of(list_cols)) %>%
    #     dplyr::full_join(overall_listcols, ., by = "time")
    #
    #   pred_df.all_new %>%
    #     # we nest all data so that each run is one line
    #     tidyr::nest(data = c(dplyr::everything(), -"run")) %>%
    #     # now for each run-row, we run predict.isat
    #     dplyr::mutate(prediction = purrr::map(.data$data, function(x) {
    #       gets::predict.isat(isat_obj,
    #                          newmxreg = x %>%
    #                            dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)) %>%
    #                            utils::tail(n.ahead) %>%
    #                            as.matrix(),
    #                          n.ahead = n.ahead, plot = FALSE,
    #                          quiet = TRUE,
    #                          ci.levels = ci.levels, n.sim = 1
    #       )
    #     })) -> all_preds
    #
    #   all_preds %>%
    #     # we get all predictions back into a row format
    #     dplyr::mutate(prediction = purrr::map(.data$prediction, dplyr::as_tibble)) %>%
    #     tidyr::unnest("prediction") %>%
    #     # we add the time dimension
    #     dplyr::mutate(time = time_values, .by = "run") %>%
    #     dplyr::select("run", "time", pred = "yhat") %>%
    #     # here pred only takes into account the uncertainty in the x-variables
    #     # pred_draws combines the model residual uncertainty for y and the uncertainty of the x-variables
    #     dplyr::mutate(
    #       pred_draws = .data$pred + res_draws,
    #       pred = NULL
    #     ) %>%
    #     # now we get them into the final format to add them back to the overall list
    #     tidyr::pivot_wider(id_cols = "time", names_from = "run", values_from = "pred_draws") %>%
    #     dplyr::select(-"time") %>%
    #     as.matrix() -> pred_runs_final_matrix
    #
    #   dimnames(pred_runs_final_matrix) <- NULL
    #
    #   # now replace the pred_draw_matrix - this one only survives without being overwritten if there is no preceding uncertainty
    #   #pred_draw_matrix <- pred_runs_final_matrix
    #   pred_draw_matrix <- pred_runs_final_matrix + res_draws_matrix
    # } else {
    #   pred_draw_matrix <- as.vector(pred_obj$yhat) + res_draws_matrix
    # }

    if (recipe$model_form == "diff") {
      # predict.isat() returns changes for a fully differenced equation.
      # Reconstruct the transformed level from the last observed level.
      level_history <- pred_setup_list$state_data[[recipe$transformed_level_name]]
      initial_level <- utils::tail(level_history[!is.na(level_history)], 1)

      if (length(initial_level) == 0) {
        stop("No observed dependent-variable level is available to initialise the forecast.")
      }

      central_level <- initial_level + cumsum(central_response)
      pred_draw_matrix <- apply(
        predicted_draws + residual_draws,
        2,
        cumsum
      )
      pred_draw_matrix <- matrix(
        pred_draw_matrix,
        nrow = n.ahead,
        ncol = uncertainty_sample
      )
      pred_draw_matrix <- initial_level + pred_draw_matrix
    } else {
      # Retain the established ARDL uncertainty treatment. Residual draws are
      # accumulated over the forecast horizon, but enter the draw only once.
      residual_draws_cumulative <- apply(
        residual_draws,
        2,
        cumsum
      )
      residual_draws_cumulative <- matrix(
        residual_draws_cumulative,
        nrow = n.ahead,
        ncol = uncertainty_sample
      )

      central_level <- central_response
      pred_draw_matrix <- predicted_draws + residual_draws_cumulative
    }
  }

  # Prepare output ----------------------------------------------------------
  central_estimate <- dplyr::tibble(
    time = forecast_times,
    value = central_level
  ) %>%
    stats::setNames(c("time", outvarname))

  colnames(pred_draw_matrix) <- run_names
  pred_draw_matrix <- dplyr::as_tibble(pred_draw_matrix) %>%
    dplyr::bind_cols(dplyr::tibble(time = forecast_times), .)

  return(list(
    central_estimate = central_estimate,
    pred_draw_matrix = pred_draw_matrix,
    predict.isat_object = pred_obj,
    final_i_data = pred_setup_list$final_i_data,
    forecast.metadata = list(
      model_form = recipe$model_form,
      response_scale = recipe$response_scale,
      estimation_transformations = recipe$estimation_transformations,
      forecast_transformations = recipe$forecast_transformations,
      transformation_adjustments = recipe$transformation_adjustments
    )
  ))
}
