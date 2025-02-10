#' Internal function to set-up the forecasting of estimated relationships
#'

#' @param i Current module that is being cycled through
#' @param exog_df_ready Outcome of forecast_exogenous_values() which is the set of forecasted exogenous values
#' @param current_spec The current specification for the module being forecasted
#' @param prediction_list The full list of all predictions. The results of the function will be saved in this list.
#' @param nowcasted_data The full_data element of the model object resulting from the nowcasting() function. Used to substitute missing historical data.
#' @param full_exog_predicted_data An argument to pass a larger data.frame to the function that can contain the entire exogenously predicted data. This is an argument that is needed in the nowcasting() function.
#' @inheritParams forecast_model
#'
#' @return A list containing, among other elements, the data required to carry out the forecast for this estimated module.
#'

forecast_setup_estimated_relationships <- function(model,
                                                   i,
                                                   exog_df_ready,
                                                   n.ahead,
                                                   current_spec,
                                                   prediction_list,
                                                   uncertainty_sample,
                                                   nowcasted_data = NULL,
                                                   full_exog_predicted_data = NULL) {



  extracted_info <- forecast_extract_info(model = model, i = i, n.ahead = n.ahead, exog_df_ready = exog_df_ready)

  extracted_info$y_names_vec -> y_names_vec
  extracted_info$x_names_vec -> x_names_vec
  extracted_info$x_names_vec_nolag -> x_names_vec_nolag
  extracted_info$ar_vec -> ar_vec
  extracted_info$ylog -> ylog
  extracted_info$xlog -> xlog
  extracted_info$mconst -> mconst
  extracted_info$current_pred_raw -> current_pred_raw
  extracted_info$current_pred_raw_all -> current_pred_raw_all
  extracted_info$is_ardl -> is_ardl
  extracted_info$isat_obj -> isat_obj
  extracted_info$data_obj -> data_obj
  extracted_info$exog_df_ready -> exog_df_ready
  extracted_info$pred_ar_needed -> pred_ar_needed
  extracted_info$pred_dl_needed -> pred_dl_needed



  # Deal with current_spec not being fully exogenous --------

  previous_dependent_vars <- model$module_order$dependent[model$module_order$order < i]
  # run this loop if any of the independent variables has already been a dependent variable of a preceding module
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

      if (!mvar_name %in% x_names_vec_nolag) {
        if (paste0("ln.",mvar_name) %in% x_names_vec_nolag) {

          log_possible <- all(mvar_tibble[, mvar_euname, drop = TRUE] > 0)
          # TODO record that log_possible chose asinh
          if(log_possible){
            mvar_tibble %>%
              dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), log, .names = "ln.{.col}")) %>%
              dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble
            mvar_all.estimates.tibble %>%
              dplyr::mutate(dplyr::across(dplyr::all_of(paste0(mvar_euname, ".all")), ~purrr::map(.,log), .names = "ln.{.col}")) %>%
              dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname,".all"))) -> mvar_all.estimates.tibble
          } else {
            mvar_tibble %>%
              dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), asinh, .names = "ln.{.col}")) %>%
              dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble
            mvar_all.estimates.tibble %>%
              dplyr::mutate(dplyr::across(dplyr::all_of(paste0(mvar_euname, ".all")), ~purrr::map(.,asinh), .names = "ln.{.col}")) %>%
              dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname,".all"))) -> mvar_all.estimates.tibble
          }
        } else {
          stop("Error occurred in adding missing/lower estimated variables (likely identities) to a subsequent/higher model. This is likely being caused by either log specification or lag specifiction. Check code.")
        }
      }


      # if the variable is already there (e.g. through nowcasting) then combine them
      if(all(names(mvar_tibble) %in% names(current_pred_raw))){

        # this first adds the correct time dimension to mvar_tibble
        exog_df_ready %>%
          dplyr::select("time") %>%
          dplyr::slice((dplyr::n() - (nrow(mvar_tibble)-1)) : dplyr::n()) -> mvar_tibble_time

        mvar_tibble_time %>%
          dplyr::bind_cols(mvar_tibble) %>%

          # now we rename it to then be able to join it
          dplyr::rename_with(.cols = dplyr::all_of(names(mvar_tibble)), .fn = ~paste0("new.",.)) %>%

          # then we join it
          dplyr::full_join(current_pred_raw %>%
                             dplyr::select("time", dplyr::all_of(names(mvar_tibble))), by = "time") %>%

          dplyr::arrange(.data$time) %>%

          # and combine the two columns so that each value is filled
          dplyr::transmute(!!dplyr::sym(names(mvar_tibble)) := dplyr::case_when(
            is.na(.[[names(mvar_tibble)]]) ~ .[[paste0("new.",names(mvar_tibble))]],
            TRUE ~ .[[names(mvar_tibble)]]
          )) -> mvar_tibble

        # now we do the same for the all estimates
        mvar_tibble_time %>%
          dplyr::bind_cols(mvar_all.estimates.tibble) %>%
          dplyr::rename_with(.cols = dplyr::all_of(names(mvar_all.estimates.tibble)), .fn = ~paste0("new.",.)) %>%
          dplyr::full_join(current_pred_raw_all %>%
                             dplyr::select("time", dplyr::all_of(names(mvar_tibble))), by = "time") %>%

          dplyr::arrange(.data$time) %>%

          dplyr::transmute(!!dplyr::sym(names(mvar_all.estimates.tibble)) := dplyr::case_when(
            is.na(.[[names(mvar_tibble)]]) ~ .[[paste0("new.",names(mvar_all.estimates.tibble))]],
            TRUE ~ purrr::map(.[[names(mvar_tibble)]], ~ .x)  # Wrap each individual value in a list
          )) -> mvar_all.estimates.tibble

        current_pred_raw %>%
          dplyr::select(-dplyr::all_of(names(mvar_tibble))) -> current_pred_raw

        current_pred_raw_all %>%
          dplyr::select(-dplyr::all_of(names(mvar_tibble))) -> current_pred_raw_all

      }

      current_pred_raw <- dplyr::bind_cols(current_pred_raw,mvar_tibble)
      current_pred_raw_all <- dplyr::bind_cols(current_pred_raw_all,mvar_all.estimates.tibble)
    }
  }

  # checking the data for nowcasted data --------

  data_obj %>%
    dplyr::select("time", dplyr::all_of(x_names_vec_nolag), dplyr::all_of(y_names_vec[1])) -> historical_estimation_data

  # in this section we check whether any of the missing values are present in nowcasted data
  # we first check if there is even any historical data used (would not be true for e.g. AR models)
  if(historical_estimation_data %>% dplyr::select(-"time") %>% ncol() > 0){

    # then we check whether there are any lines in the historical data that are missing (often the case)
    historical_estimation_data %>%
      tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
      dplyr::filter(is.na(.data$values)) -> missing_values_dataobj

    # then we check whether any of the missing values in the historical data are present in nowcasted or in the exog_df_ready data
    #if (nrow(missing_values_dataobj) > 0 & !is.null(nowcasted_data)) {
    if (nrow(missing_values_dataobj) > 0) {

      missing_values_dataobj %>%
        dplyr::mutate(basename = gsub("ln.","",.data$na_item)) %>%

        {if(!is.null(full_exog_predicted_data)){
          dplyr::left_join(.,full_exog_predicted_data %>%
                             tidyr::pivot_longer(-"time",names_to = "basename",
                                                 values_to = "values_exog_full"), by = c("time", "basename")) %>%
            dplyr::mutate(.,values = dplyr::case_when(
              !is.na(.data$values_exog_full) & is.na(.data$values) ~ .data$values_exog_full, TRUE ~ .data$values))

        } else {.}} %>%

        {if(!is.null(nowcasted_data)){
          dplyr::left_join(.,nowcasted_data %>%
                             dplyr::rename(basename = "na_item",
                                           values_nowcast = "values"), by = c("time", "basename")) %>%

            # where there are nowcast values but not original ones, take now the nowcast ones
            dplyr::mutate(values = dplyr::case_when(
              !is.na(.data$values_nowcast) & is.na(.data$values) ~ .data$values_nowcast, TRUE ~ .data$values))
        } else {.}} %>%

        # check if any of the values by na_item are below 0
        dplyr::mutate(log_possible = all(.data$values > 0, na.rm = TRUE), .by = "na_item") %>%
        dplyr::mutate(values_to_log = dplyr::case_when(.data$log_possible & !is.na(.data$values) & grepl("^ln.",.data$na_item) ~ .data$values,
                                                       TRUE ~ NA)) %>%
        # now we check if we need to log them or use asinh
        dplyr::mutate(values = dplyr::case_when(!is.na(.data$values_to_log) ~ log(.data$values_to_log),
                                                !.data$log_possible & !is.na(.data$values) & grepl("^ln.",.data$na_item) ~ asinh(.data$values),
                                                TRUE ~ .data$values)) %>%

        dplyr::select(-c("log_possible","values_to_log")) %>%

        dplyr::select("time", "na_item", new_values = "values") %>%
        tidyr::drop_na() -> values_to_replace

      # Then we take the historical data and add the nowcasted data
      historical_estimation_data %>%
        tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
        dplyr::full_join(values_to_replace, by = c("time","na_item")) %>%
        dplyr::mutate(values = dplyr::case_when(is.na(.data$values) & !is.na(.data$new_values) ~ .data$new_values, TRUE ~ .data$values),
                      new_values = NULL) %>%
        tidyr::pivot_wider(id_cols = "time", names_from = "na_item",values_from = "values") -> historical_estimation_data_w_nowcast

    } else {
      historical_estimation_data_w_nowcast <- historical_estimation_data
    }
  } else {
    historical_estimation_data_w_nowcast <- historical_estimation_data
  }

  # merging nowcasted data with non-x variables (IIS, SIS, etc.) --------
  historical_estimation_data_w_nowcast %>%
    dplyr::bind_rows(current_pred_raw %>%
                       dplyr::select("time", dplyr::all_of(x_names_vec_nolag))) %>%
    dplyr::distinct() -> intermed

  # add the lagged x-variables
  if(ncol(intermed) > 1){
    to_be_added <- dplyr::tibble(.rows = nrow(intermed))
    for (j in pred_dl_needed) {
      if(j == 0){next}
      intermed %>%
        dplyr::transmute(dplyr::across(dplyr::all_of(gsub("L[0-9]+\\.","",j)), ~dplyr::lag(., n = as.numeric(stringr::str_extract(j, "[0-9]+"))))) %>%
        setNames(j) %>%
        dplyr::bind_cols(to_be_added, .) -> to_be_added
    }
    intermed <- dplyr::bind_cols(intermed, to_be_added)
  }

  intermed %>%
    dplyr::left_join(current_pred_raw %>%
                       dplyr::select("time", dplyr::any_of("trend"), dplyr::starts_with("q_"),
                                     dplyr::starts_with("iis"), dplyr::starts_with("sis")),
                     by = "time") %>%

    # only retain the final n.ahead observations
    dplyr::slice(-c(dplyr::n() - n.ahead : dplyr::n())) %>%

    dplyr::select(-"time") %>%
    dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) -> pred_df

  # doing the same for all uncertainty samples -------

  # if necessary, repeat creating the pred_df with all estimates
  chk_any_listcols <- current_pred_raw_all %>%
    dplyr::summarise_all(class) %>%
    tidyr::gather("variable", "class") %>%
    dplyr::mutate(chk = class == "list") %>%
    dplyr::summarise(chk = any(.data$chk)) %>%
    dplyr::pull("chk")

  if(chk_any_listcols){
    ## repeat the above with all

    historical_estimation_data_w_nowcast %>%
      dplyr::select("time", dplyr::all_of(x_names_vec_nolag)) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(x_names_vec_nolag), ~as.list(.))) %>%

      dplyr::bind_rows(current_pred_raw_all %>%
                         dplyr::rename_with(dplyr::everything(), .fn = ~gsub(".all","",.)) %>%
                         dplyr::mutate(dplyr::across(-"time", .fn = ~as.list(.))) %>%
                         dplyr::select("time", dplyr::all_of(x_names_vec_nolag))) -> intermed.all


    # same for .all: add the lagged x-variables
    to_be_added.all <- dplyr::tibble(.rows = nrow(intermed.all))

    for (j in pred_dl_needed) {
      if(j == 0){next}
      intermed.all %>%
        dplyr::transmute(dplyr::across(dplyr::all_of(gsub("L[0-9]+\\.","",j)), ~dplyr::lag(., n = as.numeric(stringr::str_extract(j, "[0-9]+"))))) %>%
        setNames(j) %>%
        dplyr::bind_cols(to_be_added.all, .) -> to_be_added.all
    }

    dplyr::bind_cols(intermed.all, to_be_added.all) %>%
      dplyr::left_join(current_pred_raw_all %>%
                         dplyr::select("time", dplyr::any_of("trend"), dplyr::starts_with("q_"),
                                       dplyr::starts_with("iis"), dplyr::starts_with("sis")),
                       by = "time") %>%

      # only retain the final n.ahead observations
      dplyr::slice(-c(dplyr::n() - n.ahead : dplyr::n())) %>%

      tidyr::drop_na() %>%
      dplyr::select(-"time") %>%
      dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) %>%
      return() -> pred_df.all
  }


  # Final output data -------------------------------------------------------

  final_i_data <- dplyr::tibble(
    data = list(intermed %>%
                  dplyr::left_join(current_pred_raw %>%
                                     dplyr::select("time", dplyr::starts_with("q_"),
                                                   dplyr::starts_with("iis"),
                                                   dplyr::starts_with("sis")),
                                   by = "time") %>%

                  # only retain the final n.ahead observations
                  dplyr::slice(-c(dplyr::n() - n.ahead : dplyr::n())) %>%

                  # delete all columns that are just NA
                  dplyr::select(-dplyr::where(~all(is.na(.))))
    )
  )


  out <- list()
  out$pred_df <- pred_df
  out$isat_obj <- isat_obj
  out$final_i_data <- final_i_data
  out$chk_any_listcols <- chk_any_listcols
  out$current_pred_raw <- current_pred_raw
  out$current_pred_raw_all <- if(exists("current_pred_raw_all")){current_pred_raw_all}
  out$pred_df.all <- if(exists("pred_df.all")){pred_df.all}
  out$n.ahead <- n.ahead

  return(out)

}
