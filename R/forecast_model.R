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
                           ci.levels = c(0.5,0.66,0.95),
                           exog_fill_method = "AR",
                           ar.fill.max = 4,
                           plot = TRUE,
                           uncertainty_sample = 100,
                           quiet = FALSE){

  if(!isa(model, "osem")){stop("Forecasting only possible with an osem object. Execute 'run_model' to get such an object.")}
  if(!is.null(exog_fill_method) & !exog_fill_method %in% c("AR","last","auto","ets")){stop("The method to fill exogenous values 'exog_fill_method' can only be either NULL (when data is provided), or 'AR', 'auto', 'ets', or 'last'.")}
  if(!is.null(ar.fill.max) & (!is.integer(as.integer(ar.fill.max)) | ar.fill.max < 1)){stop("The option 'ar.fill.max' can either be NULL or must be an integer that is larger than 0.")}

  # 1. Determine Exogenous Variables and wrangle future values ---------------
  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = model$module_order)

  classification %>%
    dplyr::filter(.data$class == "x") %>%
    dplyr::pull(.data$var) -> exog_vars

  exog_forecast_list <- forecast_exogenous_values(model = model,
                                                  exog_vars = exog_vars,
                                                  exog_predictions = exog_predictions,
                                                  exog_fill_method = exog_fill_method,
                                                  ar.fill.max = ar.fill.max,
                                                  n.ahead = n.ahead,
                                                  quiet = quiet)

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
  for(i in seq(model$module_order$order)){
    # i = 1
    current_spec <- model$module_order %>%
      dplyr::filter(.data$order == i) %>%

      # save original form of independent col
      dplyr::mutate(independent_orig = .data$independent) %>%

      # make sure each independent variable has a separate row
      dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%

      dplyr::rowwise() %>%
      dplyr::mutate(independent = list(strsplits(.data$independent,c("\\-", "\\+", "/", "\\*")))) %>%

      # following line added to deal with AR models when ind_vars is a list of NULL
      dplyr::bind_rows(dplyr::tibble(independent = list(""))) %>%

      tidyr::unnest("independent", keep_empty = TRUE) %>%
      tidyr::drop_na("index") %>%
      dplyr::select("index","dependent","independent","independent_orig")


    ## 2b. Start of loop for estimated relationships  ------------------------------------------------
    if(model$module_order$type[model$module_order$order == i] != "d"){

      # get the isat object for this relationship
      isat_obj <- model$module_collection %>%
        dplyr::filter(.data$order == i) %>%
        dplyr::pull(.data$model) %>%
        .[[1]]

      pred_setup_list <- forecast_setup_estimated_relationships(model = model,
                                                                i = i,
                                                                exog_df_ready = exog_df_ready,
                                                                full_exog_predicted_data = exog_df_ready_full,
                                                                n.ahead = n.ahead,
                                                                current_spec = current_spec,
                                                                prediction_list = prediction_list,
                                                                uncertainty_sample = uncertainty_sample,
                                                                nowcasted_data = nowcasted)

      final_i_data <- pred_setup_list$final_i_data
      pred_df <- pred_setup_list$pred_df
      chk_any_listcols <- pred_setup_list$chk_any_listcols
      current_pred_raw <- pred_setup_list$current_pred_raw

      if(!is.null(pred_setup_list$pred_df.all)){
        pred_df.all <- pred_setup_list$pred_df.all
      }

      ### 2b.i. Predict main estimate for estimated relationships  ------------------------------------------------
      isat_obj$call$ar <- isat_obj$aux$args$ar
      isat_obj$call$mc <- isat_obj$aux$args$mc
      isat_obj$call$tis <- isat_obj$aux$args$tis

      pred_obj <- gets::predict.isat(isat_obj,
                                     newmxreg = as.matrix(utils::tail(pred_df %>% dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)),
                                                                      n.ahead)),
                                     n.ahead = n.ahead,
                                     plot = FALSE,
                                     ci.levels = ci.levels)

      # make samples from the model residuals and add them to the mean prediction
      res_draws <- sample(as.numeric(isat_obj$residuals), size = uncertainty_sample * n.ahead, replace = TRUE)
      # create a tibble with all res_draws with the same number of rows as n.ahead
      res_names <- paste0("run_",1:(length(res_draws)/n.ahead))
      dplyr::as_tibble(matrix(res_draws, nrow = n.ahead, dimnames = list(NULL, res_names))) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), cumsum)) %>%
        as.matrix() -> res_draws_matrix

      pred_draw_matrix <- as.vector(pred_obj$yhat) + res_draws_matrix

      ## 2b.ii. Predict uncertainty plume for estimated relationships  ------------------------------------------------

      # if there are any list columns then that means that a preceding variable has uncertainty
      # then the pred_draw_matrix is replaced with the uncertainty estimates
      if(chk_any_listcols){
        # first find the list columns - these indicate that there has been uncertainty in preceding variables
        list_cols <- names(pred_df.all)[sapply(pred_df.all, "class")=="list"]
        time_values <- current_pred_raw$time

        # in this following dataset we write the list columns properly formatted
        overall_listcols <- dplyr::tibble(time = time_values) %>%
          #dplyr::full_join(dplyr::tibble(run = 1:uncertainty_sample), by = character())
          dplyr::cross_join(dplyr::tibble(run = 1:uncertainty_sample))

        for(m in list_cols){
          # m = list_cols[1]

          # we extract the list columns into individual lists
          lapply(pred_df.all %>% dplyr::pull(m), FUN = function(x){
            # if the value is just a number (must be due to it being a lagged observed value), then just take that value
            if(is.numeric(x)){
              x
            } else if(is.data.frame(x)){
              # if this is already a dataframe, then pivot it to longer
              tidyr::pivot_longer(x, dplyr::everything(), names_to = "run") %>%
                dplyr::mutate(run = as.numeric(grep("[0-9]+$",.data$run)))
            }
          }) -> listcol_unformatted

          # and now we reformat them to a long dataset
          # now we combine the individual formatted list columns to one data frame
          # for that, we cycle through each element of the formatted list columns
          listcol_formatted <- dplyr::tibble()
          for(l in 1:length(listcol_unformatted)){
            if(is.numeric(listcol_unformatted[[l]])){
              dplyr::tibble(time = time_values[l],
                            run = 1:uncertainty_sample,
                            value = listcol_unformatted[[l]]) %>%
                dplyr::bind_rows(.,listcol_formatted) -> listcol_formatted
            } else {
              dplyr::tibble(time = time_values[l],
                            listcol_unformatted[[l]]) %>%
                dplyr::bind_rows(.,listcol_formatted) -> listcol_formatted
            }
          }
          listcol_formatted <- dplyr::arrange(listcol_formatted, .data$time, .data$run)
          names(listcol_formatted) <- c("time", "run", m)

          dplyr::full_join(overall_listcols, listcol_formatted, by = c("time","run")) -> overall_listcols
        }

        # now that we have all list columns properly formatted in one dataset, we join them with the rest of the columns
        pred_df.all_new <- pred_df.all %>%
          dplyr::mutate(time = time_values) %>%
          # we can delete the list_cols, because those will now be added in their formatted version
          dplyr::select(-dplyr::all_of(list_cols))  %>%
          dplyr::full_join(overall_listcols,., by = "time")

        pred_df.all_new %>%
          # we nest all data so that each run is one line
          tidyr::nest(data = c(dplyr::everything(),-"run")) %>%

          # now for each run-row, we run predict.isat
          dplyr::mutate(prediction = purrr::map(.data$data, function(x){
            gets::predict.isat(isat_obj,
                               newmxreg = x %>%
                                 dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)) %>%
                                 as.matrix,
                               n.ahead = n.ahead, plot = FALSE,
                               ci.levels = ci.levels, n.sim = 1)

          })) -> all_preds

        all_preds %>%
          # we get all predictions back into a row format
          dplyr::mutate(prediction = purrr::map(.data$prediction,dplyr::as_tibble)) %>%
          tidyr::unnest("prediction") %>%
          # we add the time dimension
          dplyr::mutate(time = time_values, .by = "run") %>%
          dplyr::select("run","time", pred = "yhat") %>%

          # here pred only takes into account the uncertainty in the x-variables
          # pred_draws combines the model residual uncertainty for y and the uncertainty of the x-variables
          dplyr::mutate(pred_draws = .data$pred + res_draws,
                        pred = NULL) %>%

          # now we get them into the final format to add them back to the overall list
          tidyr::pivot_wider(id_cols = "time", names_from = "run", values_from = "pred_draws") %>%
          dplyr::select(-"time") %>%
          as.matrix() -> pred_runs_final_matrix

        dimnames(pred_runs_final_matrix) <- NULL

        # now replace the pred_draw_matrix - this one only survives without being overwritten if there is no preceding uncertainty
        pred_draw_matrix <- pred_runs_final_matrix
      }

      ## 2b.iii. Prepare output for estimated relationships  ------------------------------------------------

      outvarname <- paste0(if (model$module_collection %>%
                               dplyr::filter(.data$order == i) %>%
                               .$model.args %>%
                               .[[1]] %>%
                               .$use_logs %in% c("both","y")) {"ln."} else {""},
                           current_spec %>% dplyr::pull("dependent") %>% unique)

      dplyr::tibble(time = current_pred_raw %>% dplyr::pull(.data$time),
                    value = as.numeric(pred_obj[,1])) %>%
        setNames(c("time",outvarname)) -> central_estimate

      colnames(pred_draw_matrix) <- paste0("run_",1:uncertainty_sample)
      pred_draw_matrix <- dplyr::as_tibble(pred_draw_matrix) %>%
        dplyr::bind_cols(dplyr::tibble(time = current_pred_raw$time), .)


      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::as_tibble(pred_obj)))
      prediction_list[prediction_list$order == i, "data"] <- final_i_data
      prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(central_estimate = list(central_estimate))
      prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(all_estimates = list(pred_draw_matrix))


    } else {

      ## 2b. Start of loop for identities  ------------------------------------------------

      identity_setup <- forecast_identities(model = model,
                                            exog_df_ready = exog_df_ready,
                                            current_spec = current_spec,
                                            prediction_list = prediction_list,
                                            uncertainty_sample = uncertainty_sample)

      identity_pred <- identity_setup$identity_pred
      identity_pred_final <- identity_setup$identity_pred_final
      identity_pred_final.all <- identity_setup$identity_pred_final.all
      central_estimate <- identity_setup$central_estimate
      prediction_list <- identity_setup$prediction_list

      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::tibble(yhat = identity_pred_final[,1, drop = TRUE])))
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

  try(out$full_forecast_data <- plot(out, return.data = TRUE))

  if(plot){
    try(print(plot(out)))
  }

  return(out)

}
