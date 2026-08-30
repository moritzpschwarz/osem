#' Forecast ISAT Module (internal)
#'
#' Forecasts an estimated single-equation module. ARDL and fully differenced
#' equations continue to use \code{\link[gets]{predict.isat}} for the complete
#' forecast horizon. ECM equations are forecast recursively because the
#' forecast for one period is needed to construct the lagged dependent level
#' for the following period.
#'
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
    nowcasted) {

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

  # Restore the arguments required by predict.isat(). These are not always
  # retained in the model call after model selection.
  isat_obj$call$ar <- isat_obj$aux$args$ar
  isat_obj$call$mc <- isat_obj$aux$args$mc
  isat_obj$call$tis <- isat_obj$aux$args$tis

  # Draw residual uncertainty -----------------------------------------------
  # IIS observations are excluded because their residuals are zero by
  # construction and would therefore understate forecast uncertainty.
  iis_index <- gets::isatdates(isat_obj)$iis$index
  residuals <- as.numeric(isat_obj$residuals)

  if (!is.null(iis_index) && length(iis_index) > 0 && is.numeric(iis_index)) {
    residuals <- residuals[-iis_index]
  }

  residuals <- residuals[!is.na(residuals)]

  if (length(residuals) == 0) {
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
    pred_obj <- gets::predict.isat(
      isat_obj,
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
    dplyr::bind_cols(
      dplyr::tibble(time = forecast_times),
      .
    )

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
