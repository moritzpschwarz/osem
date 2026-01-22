#' Forecast CVAR Model (internal)
#'
#' This internal function generates forecasts from a CVAR (Cointegrated Vector Autoregression) model within the OSEM framework.
#'
#' @param model The overall 'osem' model as returned by \code{\link[osem]{run_model}}
#' @param i Index of the CVAR model within the module collection
#' @param exog_df_ready Data frame of exogenous variables prepared for forecasting
#' @param full_exog_predicted_data Full data frame of predicted exogenous variables
#' @param n.ahead Number of steps ahead to forecast
#' @param current_spec Current module specification
#' @param prediction_list List of predictions from previous modules
#' @param uncertainty_sample Number of uncertainty samples to generate
#' @param nowcasted_data Nowcasted data for the current module
#'
#' @returns  A list with class 'osem.forecast.module' containing the central and all estimates forecasts.
#'
forecast_cvar <- function(model,
                          i,
                          exog_df_ready,
                          full_exog_predicted_data,
                          n.ahead,
                          current_spec,
                          prediction_list,
                          uncertainty_sample,
                          nowcasted_data) {

  # Check if the model object is of the correct class
  if (!inherits(model$module_collection$model[i][[1]], "osem.cvar")) {
    stop("Input must be a 'osem.cvar' object.")
  }

  varm <- model$module_collection$model[i][[1]]$varm

  if (!inherits(varm, "vec2var")) {
    stop("varm must be a 'vec2var' object.")
  }


  previous_dependent_vars <- model$module_order$dependent[model$module_order$order < i]
  if(any(current_spec$independent %in% previous_dependent_vars)){

    missing_vars <- current_spec$independent[current_spec$independent %in% previous_dependent_vars]

    for (mvar in missing_vars) {
      # mvar = "p5g"
      model$module_order %>%
        dplyr::filter(.data$dependent == mvar) %>%
        dplyr::pull("index") -> mvar_model_index

      prediction_list %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull(.data$predict.isat_object) %>%
        .[[1]] -> mvar_model_obj

      mvar_logs <- model$module_collection %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        .$model.args %>%
        .[[1]] %>%
        .$use_logs

      mvar_euname <- model$module_collection %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull("dependent")

      mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

      # get the uncertainty around it
      prediction_list %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull("all.estimates") %>%
        .[[1]] -> prediction_list.mvar.all

      # if the all estimates are not yet stored, use the central estimate
      if(!is.null(prediction_list.mvar.all)){
        prediction_list.mvar.all %>%
          dplyr::select(-"time") -> mvar_all.estimates

        # name all the individual estimates
        colnames(mvar_all.estimates) <- paste0(mvar_name,".all.",seq(uncertainty_sample))

        # get all the individual estimates into a column of a tibble
        mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
          dplyr::mutate(index = 1:dplyr::n()) %>%
          tidyr::nest(data = -"index") %>%
          dplyr::select(-"index") %>%
          setNames(paste0(mvar_name,".all"))

      } else {
        prediction_list %>%
          dplyr::filter(.data$index == mvar_model_index) %>%
          dplyr::pull("central.estimate") %>%
          .[[1]] %>%
          dplyr::select(-"time") -> mvar_all.estimates

        # name all the individual estimates
        colnames(mvar_all.estimates) <- paste0(mvar_name,".all.",seq(uncertainty_sample))

        # get all the individual estimates into a column of a tibble
        mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
          dplyr::mutate(index = 1:dplyr::n()) %>%
          dplyr::select(-"index") %>%
          setNames(paste0(mvar_name,".all"))
      }

      # add the mean yhat estimates and the all estimates together
      mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
        setNames(mvar_name)

      # this first adds the correct time dimension to mvar_tibble
      exog_df_ready %>%
        dplyr::select("time") %>%
        dplyr::slice((dplyr::n() - (nrow(mvar_tibble)-1)) : dplyr::n()) %>%
        dplyr::bind_cols(mvar_tibble) -> mvar_tibble_time
    }
  }

  # central estimate
  cvar_pred <- stats::predict(varm, dumvar = as.matrix(mvar_tibble))

  dplyr::tibble(names = names(cvar_pred$fcst),
                models = cvar_pred$fcst) %>%
    dplyr::mutate(fcst_values = purrr::map(.data$models, ~ .x[,"fcst"])) %>%
    dplyr::select(-"models") %>%
    tidyr::unnest(.data$fcst_values)  -> cvar_forecasts

  cvar_forecasts <- dplyr::tibble(
    names = names(cvar_pred$fcst),
    models = cvar_pred$fcst
  ) %>%
    dplyr::mutate(
      # Add time index and extract all columns as tibbles
      fcst_df = purrr::map(.data$models, ~ {
        dplyr::as_tibble(.x) %>%
          dplyr::mutate(time = seq_len(nrow(.x)))
      })
    ) %>%
    dplyr::select(-"models") %>%
    tidyr::unnest(.data$fcst_df)

  # all estimates - cycle through mvar_all.estimates
  for (j in seq(uncertainty_sample)) {
    mvar_all.estimates.tibble %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(names(mvar_all.estimates.tibble)), ~ purrr::map(.x, ~ .x[[j]]))) -> mvar_all.estimates.single

    # combine with time index
    exog_df_ready %>%
      dplyr::select("time") %>%
      dplyr::slice((dplyr::n() - (nrow(mvar_all.estimates.single)-1)) : dplyr::n()) %>%
      dplyr::bind_cols(mvar_all.estimates.single) %>%
      tidyr::unnest(2) %>%
      dplyr::rename_with(.cols = 2, ~gsub("\\.all","",.)) -> mvar_all.estimates.single.time

    # predict
    cvar_pred.all <- stats::predict(varm, dumvar = as.matrix(mvar_all.estimates.single.time[,2]))

    # store results
    if(j == 1){
      cvar_forecasts.all <- dplyr::tibble(
        na_item = names(cvar_pred.all$fcst),
        models = cvar_pred.all$fcst
      ) %>%
        dplyr::mutate(
          # Add time index and extract all columns as tibbles
          fcst_df = purrr::map(.data$models, ~ {
            dplyr::as_tibble(.x) %>%
              dplyr::mutate(time = seq_len(nrow(.x)))
          })
        ) %>%
        dplyr::select(-"models") %>%
        tidyr::unnest(.data$fcst_df) %>%
        dplyr::mutate(iteration = j)
    } else {
      cvar_forecasts.j <- dplyr::tibble(
        na_item = names(cvar_pred.all$fcst),
        models = cvar_pred.all$fcst
      ) %>%
        dplyr::mutate(
          # Add time index and extract all columns as tibbles
          fcst_df = purrr::map(.data$models, ~ {
            dplyr::as_tibble(.x) %>%
              dplyr::mutate(time = seq_len(nrow(.x)))
          })
        ) %>%
        dplyr::select(-"models") %>%
        tidyr::unnest(.data$fcst_df) %>%
        dplyr::mutate(iteration = j)

      cvar_forecasts.all <- dplyr::bind_rows(cvar_forecasts.all, cvar_forecasts.j)
    }
  }

  out <- list()
  out$central <- cvar_forecasts
  out$all <- cvar_forecasts.all

  return(out)
}
