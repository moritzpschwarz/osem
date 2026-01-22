#' Forecast a lagged relationship using a block forecast (internal)
#'
#' This internal function generates forecasts from an an estimated model within
#' the OSEM framework (see \code{\link[gets]{isat}}) where relationships are only
#' entering each other in a lagged form, requiring forecasting in a loop.
#'
#' @param model The overall 'osem' model as returned by \code{\link[osem]{run_model}}
#' @param i The order of the current module to be forecasted
#' @param exog_df_ready The exogenous data frame prepared for forecasting
#' @param exog_df_ready_full The full exogenous data frame prepared for forecasting
#' @param n.ahead Number of steps ahead to forecast
#' @param current_spec The current specification for the module being forecasted
#' @param prediction_list The collection of all predictions
#' @param uncertainty_sample The number of uncertainty samples to draw for the prediction
#' @param nowcasted The confidence interval levels for the prediction
#' @param ci.levels The nowcasted data for the model
#'
#' @returns A tibble containing the updated prediction_list object with forecasts for the current module
#'
forecast_block_loop <- function(
    model,
    i,
    exog_df_ready,
    exog_df_ready_full,
    n.ahead,
    current_spec,
    prediction_list,
    uncertainty_sample,
    nowcasted,
    ci.levels){

  # create a forecasting loop for all the subelements
  model$module_order %>%
    dplyr::mutate(block_elements = dplyr::n(), .by = "block_order") %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull("block_order") -> cur_block_order

  model$module_order %>%
    dplyr::filter(.data$block_order == cur_block_order) %>%
    dplyr::pull("order") -> indices_in_cur_block

  model$module_order %>%
    dplyr::filter(.data$block_order == cur_block_order) %>%
    dplyr::pull("sub_order") -> sub_order_in_cur_block

  mod_model <- model

  # now run in steps of 1 to n.ahead alternating between the sub-orders

  for(k in 1:n.ahead){
    for(j in sub_order_in_cur_block){
      #print(paste0("Forecasting block ", cur_block_order, " sub-order ", j, " step ", k))

      order_i <- mod_model$module_collection[mod_model$module_collection$block_order %in% cur_block_order &
                                               mod_model$module_collection$sub_order %in% j, "order"][[1]]


      current_spec <- model$module_order %>%
        dplyr::filter(.data$order == order_i) %>%
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


      prediction_list_mod <- prediction_list

      # modify the prediction_list to only keep first k rows for the current order_i
      # this is essential to make sure that the forecast_module_estimated() function works correctly
      # this allows it to go step by step
      slice_or_pad <- function(x, k) {
        if (is.null(x)) {return(NULL)}
        if (identical(x, NA_complex_)) {return(NA_complex_)}
        if (nrow(x) < k) {
          # pad with NA rows to reach k
          x %>% rbind(NA)
        } else {
          dplyr::slice(x, 1:k)
        }
      }

      prediction_list_mod$central.estimate <- lapply(prediction_list_mod$central.estimate, slice_or_pad, k = k)
      prediction_list_mod$all.estimates <- lapply(prediction_list_mod$all.estimates, slice_or_pad, k = k)
      prediction_list_mod$predict.isat_object <- lapply(prediction_list_mod$predict.isat_object, slice_or_pad, k = k)

      forecast_module_estimated(
        model = mod_model,
        i = order_i,
        exog_df_ready = exog_df_ready %>% dplyr::slice(1:k),
        exog_df_ready_full = exog_df_ready_full %>% dplyr::slice(1:k),
        n.ahead = k,
        current_spec = current_spec,
        prediction_list = prediction_list_mod,
        uncertainty_sample = uncertainty_sample,
        nowcasted = if(is.data.frame(nowcasted)) {nowcasted %>% dplyr::slice(1:k)} else {nowcasted},
        ci.levels = ci.levels
      ) -> prediction_list_mod


      prediction_list[prediction_list$order == order_i,] <- prediction_list_mod[prediction_list_mod$order == order_i,]
    }
  }
  return(prediction_list)
}
