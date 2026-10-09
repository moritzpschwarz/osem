#' Forecast ISAT Module (internal)
#'
#' This internal function generates forecasts from an ISAT (Indicator Saturation)
#' model within the OSEM framework (see \code{\link[gets]{isat}}).
#' It forecasts an estimated single-equation module.
#' By default, ARDL, ECM, and fully differenced equations are forecast
#' recursively so that innovations propagate through the estimated dynamic
#' structure. For ARDL models, `uncertainty_method = "legacy"` reproduces the
#' historical `predict.isat()` path and residual treatment.
#'
# @param model The overall 'osem' model as returned by \code{\link[osem]{run_model}}
# @param i The index of the current module within the model's module collection
# @param exog_df_ready The exogenous data frame prepared for forecasting
#' @param exog_df_ready_full The full exogenous data frame prepared for forecasting
# @param n.ahead Number of steps ahead to forecast
# @param current_spec The current specification for the module being forecasted
# @param prediction_list The collection of all predictions
# @param uncertainty_sample The number of uncertainty samples to draw for the prediction
# @param ci.levels The confidence interval levels for the prediction
#' @param nowcasted The nowcasted data for the model
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
    uncertainty_method,
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

  if (!recipe$model_form %in% c("ardl", "ecm", "diff")) {
    stop(
      "Recursive single-equation forecasting is not implemented for model form '",
      recipe$model_form,
      "'."
    )
  }

  ## Check whether we need to run legacy code -------
  use_legacy_uncertainty <- uncertainty_method == "legacy" && recipe$model_form == "ardl"

  # The historical implementation calculated the central prediction before
  # sampling residuals. This ordering must be retained for reproducibility
  # because predict.isat() can consume random numbers.
  if (use_legacy_uncertainty) {
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

    central_level <- as.numeric(as.matrix(pred_obj)[, 1])
  }

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

  # Forecast the single-equation model --------------------------------------

  if (use_legacy_uncertainty) {

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

    if (is.null(pred_setup_list$pred_df.all)) {

      predicted_draws <- matrix(
        central_level,
        nrow = n.ahead,
        ncol = uncertainty_sample
      )

      # Historical behaviour without upstream uncertainty.
      pred_draw_matrix <-
        predicted_draws +
        residual_draws_cumulative

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
            ci.levels = ci.levels,
            n.sim = 1
          )

          return(as.numeric(as.matrix(path_prediction)[, 1]))
        },
        numeric(n.ahead)
      )

      # Historical behaviour with upstream uncertainty:
      # the raw residual and its cumulative value were both added.
      pred_draw_matrix <-
        predicted_draws +
        residual_draws +
        residual_draws_cumulative
    }

  } else {

    # Correct recursive treatment. Innovations enter the response equation
    # once and subsequently propagate through the estimated response lags.
    # For ECM and fully differenced equations, changes accumulate into levels.
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

    # Retain the established predict.isat object for ARDL and differenced
    # equations so that the public forecast output remains unchanged.
    if (recipe$model_form %in% c("ardl", "diff")) {
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
    } else {
      pred_obj <- dplyr::tibble(
        yhat = recursive$central_response
      )
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
      uncertainty_method = if (use_legacy_uncertainty) {
        "legacy"
      } else {
        "recursive"
      },
      estimation_transformations = recipe$estimation_transformations,
      forecast_transformations = recipe$forecast_transformations,
      transformation_adjustments = recipe$transformation_adjustments
    )
  ))
}
