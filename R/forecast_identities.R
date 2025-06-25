#' An internal function to forecast identities within forecast_model
#'
#' @param exog_df_ready The outcome of \link[=forecast_exogenous_values]{forecast_exogenous_values} function (potentially modified by the \link[=nowcasting]{nowcasting} function)
#' @param current_spec Current specification. Is defined within \link[=forecast_model]{forecast_model}.
#' @param prediction_list Current List containing all predictions. Is defined within \link[=forecast_model]{forecast_model}.
#'
#' @inheritParams forecast_model
#' @return A list that is then used by forecast_model to set-up the final prediction object. The list contains the identity estimate.
#'
forecast_identities <- function(model, exog_df_ready, current_spec, prediction_list, uncertainty_sample){


  # Assembling the data -----------------------------------------------------

  identity_pred <- dplyr::tibble()
  identity_pred <- exog_df_ready %>%
    dplyr::select(dplyr::all_of(current_spec$independent[current_spec$independent %in% names(exog_df_ready)]))

  identity_pred.all <- identity_pred

  # check which x variables are needed in this module, but are not fully exogenous
  missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

  for(mvar in missing_vars){
    # mvar = "yf"

    model$module_order %>%
      dplyr::filter(.data$dependent == mvar) %>%
      dplyr::pull(.data$index) -> mvar_model_index

    prediction_list %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      dplyr::pull("predict.isat_object") %>%
      .[[1]] -> mvar_model_obj

    mvar_log <- model$opts_df %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      .$log_opts %>%
      .[[1]] %>%
      {if(is.null(.)){
        "none"
      } else {
        dplyr::select(.,dplyr::all_of(mvar)) %>%
          dplyr::pull()}}

    mvar_euname <- model$module_collection %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      dplyr::pull("dependent")

    ## Get earlier data for individual estimates --------------
    mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
      dplyr::mutate(data = dplyr::case_when(mvar_log == "log" ~ exp(data),
                                            mvar_log == "asinh" ~ sinh(data),
                                            mvar_log == "none" ~ data)) %>%
      setNames(mvar_euname)

    ## Get earlier data for all estimates --------------
    prediction_list %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      dplyr::pull("all.estimates") %>%
      .[[1]] %>%
      dplyr::mutate(dplyr::across(-"time", ~dplyr::case_when(mvar_log == "log" ~ exp(.),
                                                             mvar_log == "asinh" ~ sinh(.),
                                                             mvar_log == "none" ~ .)))-> prediction_list.mvar.all

    # if the all estimates are not yet stored, use the central estimate
    if(!is.null(prediction_list.mvar.all)){
      prediction_list.mvar.all %>%
        dplyr::select(-"time") -> mvar_all.estimates

      # name all the individual estimates
      colnames(mvar_all.estimates) <- paste0(mvar_euname,".all.",seq(uncertainty_sample))

      # get all the individual estimates into a column of a tibble
      mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        tidyr::nest(data = -"index") %>%
        dplyr::select(-"index") %>%
        setNames(paste0(mvar_euname,".all"))

    } else {

      prediction_list %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull("central.estimate") %>%
        .[[1]] %>%
        dplyr::select(-"time") %>%
        dplyr::mutate(dplyr::across(1, ~dplyr::case_when(mvar_log == "log" ~ exp(.),
                                                         mvar_log == "asinh" ~ sinh(.),
                                                         mvar_log == "none" ~ .))) -> mvar_all.estimates

      # name all the individual estimates
      colnames(mvar_all.estimates) <- paste0(mvar_euname,".all.",seq(uncertainty_sample))

      # get all the individual estimates into a column of a tibble
      mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        dplyr::select(-"index") %>%
        setNames(paste0(mvar_euname,".all"))
    }


    if(ncol(identity_pred) == 0){
      identity_pred <- mvar_tibble
      identity_pred.all <- mvar_all.estimates.tibble
    } else {
      identity_pred <- dplyr::bind_cols(identity_pred,mvar_tibble)
      identity_pred.all <- dplyr::bind_cols(identity_pred.all,mvar_all.estimates.tibble)
    }
  }

  # Constructing the identity (central) -------------------------------------

  identity_pred_final <- identity_pred %>%
    dplyr::mutate(!!unique(current_spec$dependent) := eval(parse(text = unique(current_spec$independent_orig)))) %>%
    dplyr::select(dplyr::all_of(unique(current_spec$dependent)))

  identity_pred_final.all <- identity_pred.all %>%

    dplyr::rename_with(.cols = dplyr::everything(), .fn = ~gsub("\\.all","",.)) %>%

    dplyr::mutate(
      !!unique(current_spec$dependent) := purrr::pmap(
        .l = dplyr::pick(dplyr::everything()),
        .f = function(...) {
          env <- list2env(list(...))
          eval(parse(text = unique(current_spec$independent_orig)), envir = env)
        }
      )
    ) %>%
    dplyr::select(dplyr::all_of(unique(current_spec$dependent))) %>%
    tidyr::unnest(cols = dplyr::everything())


  # Preparing output --------------------------------------------------------

  outvarname <- paste0(#if(any(identity_logs) && !all(identity_logs)){"ln."} else {""},
    current_spec %>% dplyr::pull("dependent") %>% unique) #%>% tolower)

  dplyr::tibble(time = exog_df_ready %>% dplyr::pull(.data$time),
                value = identity_pred_final[,1, drop = TRUE]) %>%
    setNames(c("time",outvarname)) -> central_estimate

  # if there are uncertainties, then the columns must be larger than 1
  # if not, it means that the one column coincides with the central estimate
  if(ncol(identity_pred_final.all) > 1){
    # just change the type of the object holding all estimates
    identity_pred_final.all <- as.matrix(identity_pred_final.all)
    colnames(identity_pred_final.all) <- paste0("run_",1:uncertainty_sample)
    identity_pred_final.all <- dplyr::as_tibble(identity_pred_final.all) %>%
      dplyr::bind_cols(dplyr::tibble(time = exog_df_ready$time), .)
  } else {
    identity_pred_final.all <- NULL
  }


  out <- list()
  out$identity_pred <- identity_pred
  out$identity_pred_final <- identity_pred_final %>% setNames(outvarname)
  out$identity_pred_final.all <- identity_pred_final.all
  out$central_estimate <- central_estimate
  out$prediction_list <- prediction_list
  return(out)

}

