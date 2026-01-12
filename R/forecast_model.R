#' Forecast OSEM model
#'
#' @param model A model object of class 'osem'.
#' @param exog_predictions A data.frame or tibble with values for the exogenous values. The number of rows of this data must be equal to n.ahead.
#' @param n.ahead Periods to forecast ahead
#' @param ci.levels Numeric vector. Vector with confidence intervals to be calculated. Default: c(0.5,0.66,0.95)
#' @param ar.fill.max Integer. When no exogenous values have been provided, these must be inferred. If option 'exog_fill_method = "AR"' then an autoregressive model is used to further forecast the exogenous values. This options determines the number of AR terms that should be used. Default is 4.
#' @param exog_fill_method Character, either 'AR', 'auto', or 'last'. When no exogenous values have been provided, these must be inferred. When option 'exog_fill_method = "AR"' then an autoregressive model is used to further forecast the exogenous values. With 'last', simply the last available value is used. 'auto' is an \code{\link[forecast]{auto.arima}} model.
#' @param plot Logical. Should the result be plotted? Default is TRUE.
#' @param uncertainty_sample Integer. Number of draws to be made for the error bars. Default is 100.
#' @param quiet Logical. Should messages about the forecast procedure be suppressed?
#'
#' @return A list of class 'osem.forecast' with the following elements:
#' \describe{
#'  \item{forecast}{A tibble with the forecasted values for each module.}
#'  \item{orig_model}{The original model object of class 'osem'.}
#'  \item{dictionary}{The dictionary used for the model.}
#'  \item{exog_data}{A tibble with the exogenous data used for the forecast.}
#'  \item{exog_data_nowcast}{A tibble with the exogenous data used for the nowcasting.}
#'  \item{nowcast_data}{A tibble with the nowcasted data.}
#'  \item{args}{A list with the arguments used for the forecast.}
#'  \item{full_forecast_data}{A tibble with the full forecast data, if available.}
#'  }
#' @export
#'
#' @examples
#' spec <- dplyr::tibble(
#'   type = c(
#'     "d",
#'     "d",
#'     "n"
#'   ),
#'   dependent = c(
#'     "StatDiscrep",
#'     "TOTS",
#'     "Import"
#'   ),
#'   independent = c(
#'     "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#'     "GValueAdd + Import",
#'     "FinConsExpHH + GCapitalForm"
#'   )
#' )
#'
#' \donttest{
#' a <- run_model(specification = spec)
#' forecast_model(a)
#' }
forecast_model <- function(model,
                           exog_predictions = NULL,
                           n.ahead = 10,
                           ci.levels = c(0.5, 0.66, 0.95),
                           exog_fill_method = "AR",
                           ar.fill.max = 4,
                           plot = TRUE,
                           uncertainty_sample = 100,
                           quiet = FALSE) {
  if (!isa(model, "osem")) {
    stop("Forecasting only possible with an osem object. Execute 'run_model' to get such an object.")
  }
  if (!is.null(exog_fill_method) & !exog_fill_method %in% c("AR", "last", "auto", "ets")) {
    stop("The method to fill exogenous values 'exog_fill_method' can only be either NULL (when data is provided), or 'AR', 'auto', 'ets', or 'last'.")
  }
  if (!is.null(ar.fill.max) & (!is.integer(as.integer(ar.fill.max)) | ar.fill.max < 1)) {
    stop("The option 'ar.fill.max' can either be NULL or must be an integer that is larger than 0.")
  }

  # 1. Determine Exogenous Variables and wrangle future values ---------------
  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = model$module_order)

  classification %>%
    dplyr::filter(.data$class == "x") %>%
    dplyr::pull(.data$var) -> exog_vars

  exog_forecast_list <- forecast_exogenous_values(
    model = model,
    exog_vars = exog_vars,
    exog_predictions = exog_predictions,
    exog_fill_method = exog_fill_method,
    ar.fill.max = ar.fill.max,
    n.ahead = n.ahead,
    quiet = quiet
  )

  # extract the exogenous data that is ready for forecasting
  exog_df_ready_full <- exog_forecast_list$exog_df_ready
  exog_df_ready <- exog_df_ready_full %>% utils::tail(n.ahead)

  ## 1a. Nowcasting --------------------------------------------------------------------
  nowcasted <- nowcasting(model, exog_df_ready = exog_df_ready_full, frequency = exog_forecast_list$frequency)

  # 2. Forecasting step by step according to model order ------------------------------------------------
  # set-up the prediction list that will collect all results
  prediction_list <- dplyr::tibble(
    index = model$module_order$index,
    order = model$module_order$order,
    dep_var = model$module_order$dependent,
    predict.isat_object = list(NA_complex_),
    data = list(NA_complex_),
    central.estimate = list(NA_complex_)
  )

  ## 2a. Start of main loop ------------------------------------------------
  # cycling through each module
  for (i in seq(model$module_order$order)) {
    # i = 1
    current_spec <- model$module_order %>%
      dplyr::filter(.data$order == i) %>%
      # save original form of independent col
      dplyr::mutate(independent_orig = .data$independent) %>%
      # make sure each independent variable has a separate row
      dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(independent = list(strsplits(.data$independent, c("\\-", "\\+", "/", "\\*")))) %>%
      # following line added to deal with AR models when ind_vars is a list of NULL
      dplyr::bind_rows(dplyr::tibble(independent = list(""))) %>%
      tidyr::unnest("independent", keep_empty = TRUE) %>%
      tidyr::drop_na("index") %>%
      dplyr::select("index", "dependent", "independent", "independent_orig")


    ## 2b. Start of loop for estimated relationships  ------------------------------------------------
    if (model$module_order$type[model$module_order$order == i] != "d") {

      # Check if block loop forecasting is needed
      # record number of module orders that model$module_order is unique
      block_forecasting_req <- model$module_order %>%
        dplyr::mutate(block_elements = dplyr::n(), .by = "block_order") %>%
        dplyr::filter(.data$order == i) %>%
        dplyr::pull(block_elements) > 1

      if (block_forecasting_req){

        # check that block is not already filled
        if(!identical(NA_complex_, prediction_list %>%
                     dplyr::filter(.data$order == i) %>%
                     dplyr::pull("predict.isat_object") %>%
                     dplyr::first())){
          next
        }

        forecast_block_loop(
          model = model,
          i = i,
          exog_df_ready = exog_df_ready,
          exog_df_ready_full = exog_df_ready_full,
          n.ahead = n.ahead,
          current_spec = current_spec,
          prediction_list = prediction_list,
          uncertainty_sample = uncertainty_sample,
          nowcasted = nowcasted,
          ci.levels = ci.levels
        ) -> prediction_list
      } else {
        forecast_module_estimated(
          model = model,
          i = i,
          exog_df_ready = exog_df_ready,
          exog_df_ready_full = exog_df_ready_full,
          n.ahead = n.ahead,
          current_spec = current_spec,
          prediction_list = prediction_list,
          uncertainty_sample = uncertainty_sample,
          nowcasted = nowcasted,
          ci.levels = ci.levels
        ) -> prediction_list

      }

    } else {
      ## 2b. Start of loop for identities  ------------------------------------------------

      identity_setup <- forecast_identities(
        model = model,
        exog_df_ready = exog_df_ready,
        current_spec = current_spec,
        prediction_list = prediction_list,
        uncertainty_sample = uncertainty_sample
      )

      identity_pred <- identity_setup$identity_pred
      identity_pred_final <- identity_setup$identity_pred_final
      identity_pred_final.all <- identity_setup$identity_pred_final.all
      central_estimate <- identity_setup$central_estimate
      prediction_list <- identity_setup$prediction_list

      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::tibble(yhat = identity_pred_final[, 1, drop = TRUE])))
      prediction_list[prediction_list$order == i, "data"] <- dplyr::tibble(data = list(dplyr::bind_cols(identity_pred_final, identity_pred)))
      prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(data = list(central_estimate))
      prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(data = list(identity_pred_final.all))
    }
  }


  # 3. Prepare output -------------------------------------------------------

  out <- list()
  out$forecast <- prediction_list
  out$orig_model <- model
  out$dictionary <- model$dictionary
  out$exog_data <- exog_df_ready
  out$exog_data_nowcast <- exog_df_ready_full
  out$nowcast_data <- nowcasted
  out$args <- list(
    n.ahead = n.ahead,
    ci.levels = ci.levels,
    exog_fill_method = exog_fill_method,
    ar.fill.max = ar.fill.max,
    uncertainty_sample = uncertainty_sample
  )

  class(out) <- "osem.forecast"

  try(out$full_forecast_data <- plot(out, return.data = TRUE), silent = TRUE)

  if (plot) {
    try(print(plot(out)))
  }

  return(out)
}
