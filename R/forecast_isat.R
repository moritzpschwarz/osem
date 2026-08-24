#' Forecast ISAT Module (internal)
#'
#' Forecasts ARDL, ECM, and fully differenced single-equation modules from the
#' stored forecast recipe. Dependent-variable state is updated recursively;
#' uncertainty draws are evaluated together at each horizon.
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

  prepared <- forecast_prepare_module(
    model = model,
    i = i,
    exog_df_ready_full = exog_df_ready_full,
    prediction_list = prediction_list
  )
  model <- prepared$model

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

  iis_index <- gets::isatdates(isat_obj)$iis$index
  residuals <- as.numeric(isat_obj$residuals)
  if (!is.null(iis_index) && length(iis_index) > 0L && is.numeric(iis_index)) {
    residuals <- residuals[-iis_index]
  }
  residuals <- residuals[!is.na(residuals)]

  if (length(residuals) == 0L) {
    residual_draws <- matrix(0, nrow = n.ahead, ncol = uncertainty_sample)
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

  forecast_times <- utils::tail(pred_setup_list$current_pred_raw$time, n.ahead)
  outvarname <- recipe$transformed_level_name
  run_names <- paste0("run_", seq_len(uncertainty_sample))

  central_estimate <- dplyr::tibble(
    time = forecast_times,
    value = recursive$central_level
  ) %>%
    stats::setNames(c("time", outvarname))

  pred_draw_matrix <- dplyr::as_tibble(recursive$draw_level)
  names(pred_draw_matrix) <- run_names
  pred_draw_matrix <- dplyr::bind_cols(
    dplyr::tibble(time = forecast_times),
    pred_draw_matrix
  )

  # Preserve the yhat-compatible interface expected by downstream OSEM code.
  pred_obj <- dplyr::tibble(yhat = recursive$central_level)

  list(
    central_estimate = central_estimate,
    pred_draw_matrix = pred_draw_matrix,
    predict.isat_object = pred_obj,
    final_i_data = pred_setup_list$final_i_data,
    reestimation = prepared$reestimation
  )
}
