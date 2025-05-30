#' Nowcast missing data for forecasting
#'
#' The \code{nowcasting()} function checks across all variables in the model whether there are any missing values that need to be nowcasted. If there are, it will nowcast these values and add them to the model.
#' This is done by using the forecasted values of the exogenous variables. The function will return a list with two full model objects. One contains the original model and one contains the nowcasted model.
#'
#' Concretely, the function checks in model$processed_input_data for the maximum time point. It then identifies which variables do not fully extend to this time point. For these variables, it will nowcast the values using the forecasted values of the exogenous variables.
#' So fundamentally these nowcasts depend on the exogenous forecasts.
#' @param exog_df_ready The outcome of the \link[=forecast_exogenous_values]{forecast_exogenous_values} function, as prepared by the \link[=forecast_model]{forecast_model} function..
#' @param frequency Character string that indicates the frequency of the model. Must be compatible with the 'by' argument in \code{seq.Date()}.
#' @inheritParams forecast_model
#'
#' @return Returns a list with two full model objects. One contains the original model and one contains the nowcasted model.
#'
nowcasting <- function(model, exog_df_ready, frequency){

  # save the original model without nowcasts as backup
  orig_model <- model

  # let's first find the time that we need all data up to
  model$processed_input_data %>%
    dplyr::summarise(time = max(.data$time)) %>%
    dplyr::pull("time") -> target_time

  # we now figure out which variables do not fully extend to the final date in the database
  model$processed_input_data %>%
    dplyr::as_tibble() %>%
    dplyr::filter(.data$na_item %in% model$module_collection$dependent) %>%
    tidyr::drop_na("values") %>%
    dplyr::summarise(max_time = max(.data$time), .by = "na_item") %>%
    dplyr::filter(.data$max_time != target_time) -> vars_not_full

  # if all are to the end, then we can skip
  if(nrow(vars_not_full) == 0){return(NULL)} else{
    # if not, then we need to nowcast

    # for these variables, we now must nowcast the values
    vars_not_full %>%
      dplyr::rename("dependent" = "na_item") %>%
      dplyr::left_join(model$module_collection, by = "dependent") %>%
      dplyr::arrange(order) -> vars_not_full_analysis

    collected_nowcasts <- dplyr::tibble()
    for(ord in vars_not_full_analysis$order){

      dep_var <- vars_not_full_analysis[vars_not_full_analysis$order == ord,"dependent", drop = TRUE]

      # define the prediction interval
      vars_not_full_analysis %>%
        dplyr::filter(.data$order == ord) %>%
        dplyr::pull("max_time") -> current_max_time

      # removing the first one in the sequence (using [-1]) as that would be the last available value
      cur_target_dates <- seq.Date(as.Date(current_max_time), as.Date(target_time), by = frequency)[-1]

      # if this relationship is an estimated relationship
      if(vars_not_full_analysis %>% dplyr::filter(.data$order == ord) %>% dplyr::pull("type") == "n"){

        # find out which independent variables we need
        vars_not_full_analysis %>%
          dplyr::filter(.data$order == ord) %>%
          tidyr::unnest("indep") %>%
          dplyr::pull("indep") -> indep_vars_to_get

        model$processed_input_data %>%

          # add in any variables that might already have been nowcasted
          dplyr::bind_rows(collected_nowcasts) %>%

          dplyr::filter(.data$na_item %in% indep_vars_to_get) %>% # get the independent variables
          dplyr::filter(.data$time %in% cur_target_dates) %>% # for the appropriate interval

          # make sure we deal with any duplicates that might come from the collected nowcasts
          dplyr::mutate(count = dplyr::n(), .by = c("time","na_item"),
                        keep = dplyr::if_else(.data$count > 1 & is.na(.data$values), FALSE, TRUE)) %>%
          dplyr::filter(.data$keep) %>%
          dplyr::select(-"count",-"keep") %>%

          tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
          dplyr::mutate(q = lubridate::quarter(.data$time),
                        q = factor(.data$q, levels = c(1,2,3,4))) %>%
          dplyr::arrange("time") %>%
          {if(nrow(.) > 0){
            fastDummies::dummy_cols(.,
                                    select_columns = "q", remove_first_dummy = FALSE,
                                    remove_selected_columns = TRUE)} else {.}} -> exog_data_nowcasting

        # if there are no variables available, we just record them as NA (needed for below)
        if(nrow(exog_data_nowcasting) == 0){

          vars_missing <- indep_vars_to_get
          matrix(NA_integer_,
                 nrow = length(cur_target_dates),
                 ncol = length(vars_missing), dimnames = list(NULL, vars_missing)) %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(time = cur_target_dates) %>%
            dplyr::mutate(q = lubridate::quarter(.data$time),
                          q = factor(.data$q, levels = c(1,2,3,4))) %>%
            dplyr::arrange("time") %>%
            dplyr::relocate("time") %>%
            fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = FALSE,
                                    remove_selected_columns = TRUE) -> exog_data_nowcasting
        }

        # we check whether all relevant variables are even in this subset
        # we need to do this as sometimes the appropriate interval that we filtered for
        # above will mean that one (or more) variable(s) is not included in the exog_data_nowcasting at all
        if(!all(indep_vars_to_get %in% names(exog_data_nowcasting))){
          vars_missing <- indep_vars_to_get[!indep_vars_to_get %in% names(exog_data_nowcasting)]
          matrix(NA_integer_,
                 nrow = nrow(exog_data_nowcasting),
                 ncol = length(vars_missing), dimnames = list(NULL, vars_missing)) %>%
            dplyr::as_tibble() %>%
            dplyr::bind_cols(exog_data_nowcasting,.) %>%
            dplyr::relocate("time",dplyr::all_of(indep_vars_to_get)) -> exog_data_nowcasting
        }

        # we check whether all relevant time periods are even in this subset
        # we need to do this as sometimes the appropriate interval that we filtered for
        # above will mean that one (or more) time period(s) is not included in the exog_data_nowcasting at all
        if(!all(cur_target_dates %in% exog_data_nowcasting$time)){
          target_dates_missing <- cur_target_dates[!cur_target_dates %in% exog_data_nowcasting$time]
          matrix(NA_integer_,
                 nrow = length(target_dates_missing),
                 ncol = ncol(exog_data_nowcasting), dimnames = list(target_dates_missing, names(exog_data_nowcasting))) %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(time = target_dates_missing) %>%
            dplyr::mutate(q = lubridate::quarter(.data$time),
                          q = factor(.data$q, levels = c(1,2,3,4))) %>%
            dplyr::arrange("time") %>%
            dplyr::relocate("time") %>%
            fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = FALSE,
                                    remove_selected_columns = TRUE) %>%
            dplyr::bind_rows(exog_data_nowcasting,.) %>%
            dplyr::relocate("time",dplyr::all_of(indep_vars_to_get)) -> exog_data_nowcasting
        }

        # above we made sure that all variables appear in the exog_data_nowcasting
        # here we check whether any of the exogenous values are NA and need to be replaced
        # this can happen when co-variates were not available up to the final time
        # this would mean that those values are not available for nowcasting
        exog_data_nowcasting %>%
          tidyr::pivot_longer(-"time", values_to = "values_historical") %>%
          dplyr::filter(is.na(.data$values_historical)) %>%
          dplyr::left_join(exog_df_ready %>%
                             tidyr::pivot_longer(-"time"), by = c("time","name")) %>%
          dplyr::mutate(values_forecasted_exogenously = .data$value) %>%
          dplyr::select(-"value", -"values_historical") -> exog_data_to_replace

        if(!identical(exog_data_to_replace$values_forecasted_exogenously,numeric(0)) & all(is.na(exog_data_to_replace$values_forecasted_exogenously))){

          stop(paste0("Forecasting has failed, likely due to a nowcasting issue. ",
                      "This typically happens when exogenous values are provided using the 'exog_predictions' option of 'forecast_model()'. ",
                      "There the source of this error is likely that the provided values do not cover values for the situation that there is a need to nowcast some values. ",
                      "The most reliable process to avoid this is to use: ",
                      "result <- run_model(specification); ",
                      "forecast <- forecast_model(result); ",
                      "modified_exog_data <- forecast$exog_data_nowcast * some_kind_of_change(e.g. multiplying by 1.05 to increase by 5%); ",
                      "forecast_adapted <- forecast_model(result, exog_predictions = modified_exog_data)", collapse = "\n"))}

        if(nrow(exog_data_to_replace) > 0){
          exog_data_nowcasting %>%
            tidyr::pivot_longer(-"time") %>%
            dplyr::left_join(exog_data_to_replace, by = c("time","name")) %>%
            dplyr::mutate(value = dplyr::case_when(
              is.na(.data$value) & !is.na(.data$values_forecasted_exogenously) ~ .data$values_forecasted_exogenously,
              TRUE ~ .data$value)) %>%
            dplyr::select(-"values_forecasted_exogenously") %>%
            tidyr::pivot_wider(id_cols = "time",names_from = "name", values_from = "value") -> exog_data_nowcasting
        }

        current_spec <- model$module_order %>%
          dplyr::filter(.data$order == ord) %>%

          # save original form of independent col
          dplyr::mutate(independent_orig = .data$independent) %>%

          # make sure each independent variable has a separate row
          dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%

          dplyr::rowwise() %>%
          dplyr::mutate(independent = list(strsplits(.data$independent,c("\\-", "\\+")))) %>%

          # following line added to deal with AR models when ind_vars is a list of NULL
          dplyr::bind_rows(dplyr::tibble(independent = list(""))) %>%

          tidyr::unnest("independent", keep_empty = TRUE) %>%
          tidyr::drop_na("index") %>%
          dplyr::select("index","dependent","independent","independent_orig")

        pred_setup_list <- nowcast_setup_estimated_relationships(
          model = model,
          i = ord,
          exog_df_ready = exog_data_nowcasting,
          n.ahead = length(cur_target_dates),
          current_spec = current_spec,
          full_exog_predicted_data = exog_df_ready,
          nowcasted_data = collected_nowcasts,
        )

        final_i_data <- pred_setup_list$final_i_data
        pred_df <- pred_setup_list$pred_df
        isat_obj <- pred_setup_list$isat_obj
        current_pred_raw <- pred_setup_list$current_pred_raw

        isat_obj$call$ar <- isat_obj$aux$args$ar
        isat_obj$call$mc <- isat_obj$aux$args$mc

        pred_obj <- gets::predict.isat(isat_obj,
                                       newmxreg = as.matrix(utils::tail(pred_df %>% dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)), length(cur_target_dates))),
                                       n.ahead = length(cur_target_dates), plot = FALSE,
                                       ci.levels = NULL, n.sim = 1)

        model$opts_df %>%
          dplyr::filter(.data$order == ord) %>%
          dplyr::pull("log_opts") %>%
          dplyr::first() %>%
          dplyr::select(dplyr::all_of(dep_var)) %>%
          dplyr::pull() -> log_opts

        # add the data to the full data and the processed input data
        dplyr::tibble(time = as.Date(cur_target_dates), values = as.numeric(pred_obj)) %>%
          dplyr::mutate(values = dplyr::case_when(log_opts == "log" ~ exp(.data$values),
                                                  log_opts == "asinh" ~ sinh(.data$values),
                                                  log_opts == "none" ~ .data$values),
                        na_item = dep_var,.after = "time") -> data_to_add


        collected_nowcasts %>%
          dplyr::bind_rows(data_to_add) %>%
          dplyr::distinct() -> collected_nowcasts
      }

      # Identities
      if(vars_not_full_analysis %>% dplyr::filter(.data$order == ord) %>% dplyr::pull("type") == "d"){

        # steps for identity nowcasting
        # 1. get the default data from identify module data
        # 2. get the historical data to fill in any values that are available in the historical data (I don't think there ever are any)
        # 3. fill in any exogenous values from the exogenous forecasts
        # 4. fill in any nowcasted data from previous modules

        identity_data <- identify_module_data(data = model$full_data,
                                              classification = classify_variables(model$module_order),
                                              module = vars_not_full_analysis %>% dplyr::filter(.data$order == ord))

        # we now look where we have historical data but not estimated (.hat) data
        # we would not want to use those for estimation but for nowcasting this makes sense


        model$full_data %>%
          dplyr::rename(values_full = "values") %>%
          dplyr::filter(.data$time %in% cur_target_dates) %>%
          dplyr::filter(!grepl("\\.hat$",.data$na_item)) -> historical_data

        # so we identify where in identity_data there is data missing (is NA) for the dates that we are nowcasting here
        # we then replace those with the historical data

        identity_data %>%
          dplyr::filter(.data$time %in% cur_target_dates) %>%
          dplyr::filter(is.na(.data$values)) %>%
          dplyr::left_join(historical_data, by = c("time","na_item")) %>%
          dplyr::mutate(values = .data$values_full,
                        values_full = NULL) -> data_to_substitute


        # we then check whether any of the exogenous values need to be filled in from the exogenous forecasts
        # this can happen when co-variates were not available up to the final time
        data_to_substitute %>%
          dplyr::filter(is.na(.data$values)) %>%

          # check with exog_df_ready
          dplyr::left_join(exog_df_ready %>%
                             tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values_forecasted_exogenously"), by = c("time","na_item")) %>%
          dplyr::select(-"values") -> exog_data_to_replace


        if(nrow(exog_data_to_replace) > 0){
          data_to_substitute %>%
            dplyr::left_join(exog_data_to_replace, by = c("time","na_item")) %>%
            dplyr::mutate(values = dplyr::case_when(
              is.na(.data$values) &
                !is.na(values_forecasted_exogenously) ~ values_forecasted_exogenously, TRUE ~ .data$values)) %>%
            dplyr::select(-"values_forecasted_exogenously") -> data_to_substitute

        }

        # we then check whether any of the values need to be filled in from already nowcasted data
        if(data_to_substitute %>%
           dplyr::filter(is.na(.data$values)) %>%
           nrow > 0 & nrow(collected_nowcasts) > 0){
          data_to_substitute %>%
            dplyr::filter(is.na(.data$values)) %>%

            # check with already nowcasted data
            dplyr::left_join(collected_nowcasts %>%
                               dplyr::rename(values_nowcasted = "values") %>%
                               dplyr::mutate(na_item = paste0(.data$na_item, ".hat")), by = c("time","na_item")) %>%

            dplyr::select(-"values") -> nowcast_data_to_replace
        } else {
          nowcast_data_to_replace <- dplyr::tibble()
        }

        if(nrow(nowcast_data_to_replace) > 0){
          data_to_substitute %>%
            dplyr::left_join(nowcast_data_to_replace, by = c("time","na_item")) %>%
            dplyr::mutate(values = dplyr::case_when(
              is.na(.data$values) &
                !is.na(values_nowcasted) ~ values_nowcasted, TRUE ~ .data$values)) %>%
            dplyr::select(-"values_nowcasted") -> data_to_substitute

        }

        # we then combine the default identity data with the additional data
        identity_data %>%
          dplyr::filter(.data$time %in% cur_target_dates) %>%
          dplyr::filter(!is.na(.data$values)) %>%
          dplyr::bind_rows(data_to_substitute) -> identity_data_subst

        identity_data %>%
          dplyr::distinct(.data$na_item) %>%
          dplyr::rowwise() %>%
          dplyr::mutate(hat = grepl("\\.hat$",.data$na_item),
                        orig_name = .data$na_item,
                        na_item = gsub("\\.hat","",.data$na_item)) %>%
          dplyr::ungroup() -> identity_naming

        # now get the naming with .hat right
        identity_data_subst %>%
          dplyr::left_join(identity_naming, by = "na_item") %>%
          dplyr::rowwise() %>%
          dplyr::mutate(na_item = dplyr::case_when(.data$hat ~ paste0(.data$na_item,".hat"), TRUE ~ .data$na_item)) %>%
          dplyr::select(-"hat",-"orig_name") %>%
          dplyr::distinct() -> identity_data_subst_ready

        identity_pred <- identity_module(data = identity_data_subst_ready,
                                         module = vars_not_full_analysis %>% dplyr::filter(order == ord),
                                         classification = classify_variables(model$module_order))

        identity_pred %>%
          dplyr::select("time",dplyr::starts_with(dep_var)) %>%
          dplyr::select("time", dplyr::ends_with(".hat")) %>%
          dplyr::rename_with(.cols = dplyr::everything(), .fn = ~gsub(".level|.hat","",.)) %>%
          tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") -> data_to_add

        collected_nowcasts %>%
          dplyr::bind_rows(data_to_add) %>%
          dplyr::distinct()-> collected_nowcasts

      }
    }

    collected_nowcasts %>%
      dplyr::arrange(dplyr::desc(.data$time), .data$na_item) %>%
      return()
  }



}
