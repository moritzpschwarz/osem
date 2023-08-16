#' An internal function to forecast identities within forecast_model
#'
#'
#'
#'
#' @inheritParams forecast_model
#' @return A list that is then used by forecast_model to set-up the final prediction obejct. The list contains the identity estimate.
#'
forecast_identities <- function(model, exog_df_ready, current_spec, prediction_list, uncertainty_sample){

  identity_pred <- dplyr::tibble()
  identity_pred <- exog_df_ready %>%
    dplyr::select(dplyr::all_of(current_spec$independent[current_spec$independent %in% names(exog_df_ready)]))

  identity_pred.all <- identity_pred
  identity_logs <- c(rep(FALSE,length(current_spec$independent[current_spec$independent %in% names(exog_df_ready)])))

  # check which x variables are needed in this module, but are not fully exogenous
  missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

  for(mvar in missing_vars){
    # mvar = "yf"

    model$module_order_eurostatvars %>%
      dplyr::filter(.data$dependent == mvar) %>%
      dplyr::pull(.data$index) -> mvar_model_index

    prediction_list %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      dplyr::pull(.data$predict.isat_object) %>%
      .[[1]] -> mvar_model_obj

    mvar_logs <- model$module_collection %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      .$model.args %>%
      .[[1]] %>%
      .$use_logs

    prediction_list %>%
      dplyr::filter(index == mvar_model_index) %>%
      dplyr::pull(all.estimates) %>%
      .[[1]] %>%
      dplyr::select(-"time") -> mvar_all.estimates

    identity_logs <- c(identity_logs, ifelse(mvar_logs %in% c("both","x") || is.null(mvar_logs), TRUE, FALSE))

    mvar_euname <- model$module_collection %>%
      dplyr::filter(.data$index == mvar_model_index) %>%
      dplyr::pull("dependent")

    mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

    mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
      setNames(mvar_name)

    # name all the individual estimates
    colnames(mvar_all.estimates) <- paste0(mvar_name,".all.",seq(uncertainty_sample))

    # get all the individual estimates into a column of a tibble
    mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
      dplyr::mutate(index = 1:dplyr::n()) %>%
      tidyr::nest(data = -index) %>%
      dplyr::select(-index) %>%
      setNames(paste0(mvar_name,".all"))


    if(ncol(identity_pred)==0){
      identity_pred <- mvar_tibble
      identity_pred.all <- mvar_all.estimates.tibble
    } else {
      identity_pred <- dplyr::bind_cols(identity_pred,mvar_tibble)
      identity_pred.all <- dplyr::bind_cols(identity_pred.all,mvar_all.estimates.tibble)
    }


  }

  # log all the exogenous columns, if any of the estimated columns is logged
  if(any(identity_logs) && !all(identity_logs)){
    identity_pred %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(names(identity_pred)[!identity_logs]),
                                  .fns = list(ln = log), .names = "{.fn}.{col}")) %>%
      dplyr::select(-dplyr::all_of(dplyr::all_of(names(identity_pred)[!identity_logs]))) -> identity_pred

    identity_pred.all %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(names(identity_pred.all)[!identity_logs]), ~purrr::map(.,log), .names = "ln.{.col}")) %>%
      dplyr::select(-dplyr::all_of(dplyr::all_of(names(identity_pred.all)[!identity_logs]))) -> identity_pred.all
  }

  # sum the identities
  cols_to_cycle <- gsub(" ","",strsplits(unique(current_spec$independent_orig), c("\\+", "\\-")))
  operators <- stringr::str_extract_all(string = unique(current_spec$independent_orig), pattern = "\\+|\\-")[[1]]

  if(length(cols_to_cycle) != (length(operators)+1)){warning("Identity might be falsely calculated. Check operators.")}

  # get the first column of the identity
  identity_pred_final <- if(grepl("^ln\\.",names(identity_pred[,1, drop = FALSE]))){
    # an identity is never logged - therefore exponentiate the first column if that is logged
    exp(identity_pred[,1, drop = FALSE])
  } else{
    identity_pred[,1, drop = FALSE]}

  for(col_cycle in 2:ncol(identity_pred)){
    cur_col_cycle <- identity_pred[,col_cycle, drop = FALSE]
    # an identity is never logged - therefore exponentiate the first column if that is logged
    if(grepl("^ln\\.",names(cur_col_cycle))){cur_col_cycle <- exp(cur_col_cycle)}

    if(operators[col_cycle-1] == "+"){
      identity_pred_final <- identity_pred_final + cur_col_cycle
    } else if(operators[col_cycle-1] == "-"){
      identity_pred_final <- identity_pred_final - cur_col_cycle
    } else {stop("Error in calculating Identity.")}

  }

  # repeat the same for .all estimates
  # get the first column of the identity
  identity_pred_final.all <- if(grepl("^ln\\.",names(identity_pred.all[,1, drop = FALSE]))){
    # an identity is never logged - therefore exponentiate the first column if that is logged
    identity_pred.all[,1, drop = FALSE] %>%
      dplyr::mutate(dplyr::across(1, ~purrr::map(.,exp)))
  } else{
    identity_pred.all[,1, drop = FALSE]}

  for(col_cycle in 2:ncol(identity_pred.all)){
    cur_col_cycle <- identity_pred.all[,col_cycle, drop = FALSE]
    # an identity is never logged - therefore exponentiate the first column if that is logged
    if(grepl("^ln\\.",names(cur_col_cycle))){
      cur_col_cycle <- cur_col_cycle %>%
        dplyr::mutate(dplyr::across(1, ~purrr::map(.,exp)))
    }

    if(operators[col_cycle-1] == "+"){

      identity_pred_final.all <- tidyr::unnest(identity_pred_final.all, cols = dplyr::everything()) +
        {if(ncol(tidyr::unnest(cur_col_cycle, cols = dplyr::everything())) == 1){
          tidyr::unnest(cur_col_cycle, cols = dplyr::everything()) %>%
            dplyr::pull(1)
        } else {
          tidyr::unnest(cur_col_cycle, cols = dplyr::everything())}}


    } else if(operators[col_cycle-1] == "-"){

      identity_pred_final.all <- tidyr::unnest(identity_pred_final.all, cols = dplyr::everything()) -
        {if(ncol(tidyr::unnest(cur_col_cycle, cols = dplyr::everything())) == 1){
          tidyr::unnest(cur_col_cycle, cols = dplyr::everything()) %>%
            dplyr::pull(1)
        } else {
          tidyr::unnest(cur_col_cycle, cols = dplyr::everything())}}

    } else {stop("Error in calculating Identity.")}
  }

  outvarname <- paste0(#if(any(identity_logs) && !all(identity_logs)){"ln."} else {""},
    current_spec %>% dplyr::pull("dependent") %>% unique) #%>% tolower)

  dplyr::tibble(time = exog_df_ready %>% dplyr::pull(.data$time),
                value = as.numeric(identity_pred_final[,1])) %>%
    setNames(c("time",outvarname)) -> central_estimate

  # just change the type of the object holding all estimates
  identity_pred_final.all <- as.matrix(identity_pred_final.all)
  colnames(identity_pred_final.all) <- paste0("run_",1:uncertainty_sample)
  identity_pred_final.all <- dplyr::as_tibble(identity_pred_final.all) %>%
    dplyr::bind_cols(dplyr::tibble(time = exog_df_ready$time), .)


  out <- list()
  out$identity_pred <- identity_pred
  out$identity_pred_final <- identity_pred_final
  out$identity_pred_final.all <- identity_pred_final.all
  out$central_estimate <- central_estimate
  out$prediction_list <- prediction_list
  return(out)

}

