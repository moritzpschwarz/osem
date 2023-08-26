#' Nowcast missing data for forecasting
#'
#' @inheritParams forecast_model
#'
#' @return Returns a list with two full model objects. One contains the original model and one contains the nowcasted model.
#'
nowcasting <- function(model, exog_df_ready){

  # save the original model without nowcasts as backup
  orig_model <- model

  # let's first find the time that we need all data up to
  # model$processed_input_data %>%
  #   dplyr::filter(na_item %in% model$module_collection$dependent) %>%
  #   tidyr::drop_na(values) %>%
  #   dplyr::summarise(max_time = max(time)) %>%
  #   dplyr::pull(max_time) -> target_time
  model$processed_input_data %>%
    dplyr::summarise(time = max(time)) %>%
    dplyr::pull(time) -> target_time

  # we now figure out which variables do not fully extend to the final date in the database
  model$processed_input_data %>%
    dplyr::filter(na_item %in% model$module_collection$dependent) %>%
    tidyr::drop_na(values) %>%
    dplyr::summarise(max_time = max(time), .by = na_item) %>%
    dplyr::filter(max_time != target_time) -> vars_not_full

  # if all are to the end, then we can skip
  if(nrow(vars_not_full) == 0){return(NULL)} else{
    # if not, then we need to nowcast

    # for these variables, we now must now-cast the values
    vars_not_full %>%
      dplyr::rename("dependent" = "na_item") %>%
      dplyr::left_join(model$module_collection, by = "dependent") %>%
      dplyr::arrange(order) -> vars_not_full_analysis

    collected_nowcasts <- dplyr::tibble()
    for(ord in vars_not_full_analysis$order){
      # ord <- vars_not_full_analysis$order[1]
      # ord <- vars_not_full_analysis$order[2]

      dep_var <- vars_not_full_analysis[vars_not_full_analysis$order == ord,"dependent", drop = TRUE]

      # define the prediction interval
      vars_not_full_analysis %>%
        dplyr::filter(.data$order == ord) %>%
        dplyr::pull(max_time) -> current_max_time

      # removing the first one in the sequence (using [-1]) as that would be the last available value
      cur_target_dates <- seq.Date(as.Date(current_max_time),as.Date(target_time), "q")[-1]

      if(vars_not_full_analysis %>% dplyr::filter(.data$order == ord) %>% dplyr::pull(type) == "n"){

        # find out which independent variables we need
        vars_not_full_analysis %>%
          dplyr::filter(.data$order == ord) %>%
          tidyr::unnest(indep) %>%
          dplyr::pull(indep) -> indep_vars_to_get

        model$processed_input_data %>%
          dplyr::filter(na_item %in% indep_vars_to_get) %>% # get the independent values
          dplyr::filter(time %in% cur_target_dates) %>% # for the appropriate interval
          tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
          dplyr::mutate(q = lubridate::quarter(.data$time),
                        q = factor(q, levels = c(1,2,3,4))) %>%
          dplyr::arrange("time") %>%
          fastDummies::dummy_cols(
            select_columns = "q", remove_first_dummy = FALSE,
            remove_selected_columns = TRUE) -> exog_data_nowcasting

        # here we check whether any of the exogenous values need to be replaced
        # this can happen when co-variates were not available up to the final time
        # this would mean that those values are not available for nowcasting
        exog_data_nowcasting %>%
          pivot_longer(-time, values_to = "values_historical") %>%
          filter(is.na(values_historical)) %>%
          left_join(exog_df_ready %>% pivot_longer(-time), by = c("time","name")) %>%
          mutate(values_forecasted_exogenously = value) %>%
          select(-"value", -"values_historical") -> exog_data_to_replace

        if(nrow(exog_data_to_replace) > 0){
          exog_data_nowcasting %>%
            pivot_longer(-time) %>%
            left_join(exog_data_to_replace, by = c("time","name")) %>%
            mutate(value = case_when(is.na(value) & !is.na(values_forecasted_exogenously) ~ values_forecasted_exogenously, TRUE ~ value)) %>%
            select(-"values_forecasted_exogenously") %>%
            pivot_wider(id_cols = "time",names_from = "name", values_from = "value") -> exog_data_nowcasting
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

        pred_setup_list <- forecast_setup_estimated_relationships(
          model = model,
          i = ord,
          exog_df_ready = exog_data_nowcasting,
          n.ahead = length(cur_target_dates),
          current_spec = current_spec,
          nowcasted_data = model$full_data
        )

        final_i_data <- pred_setup_list$final_i_data
        pred_df <- pred_setup_list$pred_df
        isat_obj <- pred_setup_list$isat_obj
        current_pred_raw <- pred_setup_list$current_pred_raw

        if(!is.null(pred_setup_list$pred_df.all)){
          pred_df.all <- pred_setup_list$pred_df.all
        }

        isat_obj$call$ar <- isat_obj$aux$args$ar
        isat_obj$call$mc <- isat_obj$aux$args$mc

        pred_obj <- gets::predict.isat(isat_obj,
                                       newmxreg = as.matrix(utils::tail(pred_df %>% dplyr::select(dplyr::any_of(isat_obj$aux$mXnames)), length(cur_target_dates))),
                                       n.ahead = length(cur_target_dates), plot = FALSE,
                                       ci.levels = NULL, n.sim = 1)


        vars_not_full_analysis %>%
          dplyr::filter(.data$order == ord) %>% dplyr::pull(model.args) %>% dplyr::first() %>% .$use_logs -> log_use

        # add the data to the full data and the processed input data
        dplyr::tibble(time = as.Date(cur_target_dates), values = as.numeric(pred_obj)) %>%
          dplyr::mutate(values = dplyr::case_when(log_use == "both" | log_use == "y" ~ exp(values), TRUE ~ values),
                        na_item = dep_var,.after = "time") -> data_to_add

        model$full_data %>%
          dplyr::filter(!(na_item == dep_var & time %in% cur_target_dates)) %>% # delete the old data (should always be NA)
          dplyr::bind_rows(.,data_to_add) %>%
          dplyr::arrange(dplyr::desc(time), na_item) -> model$full_data

        model$processed_input_data %>%
          dplyr::filter(!(na_item == dep_var & time %in% cur_target_dates)) %>% # delete the old data (should always be NA)
          dplyr::bind_rows(.,data_to_add) %>%
          dplyr::arrange(dplyr::desc(time), na_item) -> model$processed_input_data

      }

      # Identities
      if(vars_not_full_analysis %>% dplyr::filter(.data$order == ord) %>% dplyr::pull(type) == "d"){

        identity_data <- identify_module_data(data = model$full_data,
                                              classification = classify_variables(model$module_order),
                                              module = vars_not_full_analysis %>% dplyr::filter(order == ord))

        # we now look where we have historical or nowcasted data but not estimated (.hat) data
        # we would not want to use those for estimation but for nowcasting this makes sense
        # so we identify where in identity_data there is data NA for the dates that we are nowcasting
        # we then replace those with the historical data
        model$full_data %>%
          dplyr::rename(values_full = values) %>%
          dplyr::filter(!grepl("\\.hat$",na_item)) %>%
          #dplyr::mutate(na_item = paste0(na_item,".hat")) %>%
          dplyr::left_join(identity_data %>%
                             dplyr::filter(time %in% cur_target_dates) %>%
                             dplyr::filter(is.na(values)), by = c("time", "na_item")) %>%
          dplyr::mutate(values = values_full,
                        values_full = NULL) -> data_to_substitue


        # here we check whether any of the exogenous values need to be replaced
        # this can happen when co-variates were not available up to the final time
        # this would mean that those values are not available for nowcasting
        data_to_substitue %>%
          filter(is.na(values)) %>%
          left_join(exog_df_ready %>% pivot_longer(-time, names_to = "na_item"), by = c("time","na_item")) %>%
          mutate(values_forecasted_exogenously = value) %>%
          select(-"value", -"values") -> exog_data_to_replace

        if(nrow(exog_data_to_replace) > 0){
          data_to_substitue %>%
            left_join(exog_data_to_replace, by = c("time","na_item")) %>%
            mutate(values = case_when(is.na(values) & !is.na(values_forecasted_exogenously) ~ values_forecasted_exogenously, TRUE ~ values)) %>%
            select(-"values_forecasted_exogenously") -> data_to_substitue

        }

        identity_data %>%
          dplyr::filter(time %in% cur_target_dates) %>%
          dplyr::filter(!is.na(values)) %>%
          dplyr::bind_rows(data_to_substitue) -> identity_data_subst

        identity_data %>%
          distinct(na_item) %>%
          rowwise() %>%
          mutate(hat = grepl("\\.hat$",na_item),
                 orig_name = na_item,
                 na_item = gsub("\\.hat","",na_item)) -> identity_naming

        # now get the naming with .hat right
        identity_data_subst %>%
          dplyr::left_join(identity_naming, by = "na_item") %>%
          dplyr::rowwise() %>%
          dplyr::mutate(na_item = dplyr::case_when(hat ~ paste0(na_item,".hat"), TRUE ~ na_item)) %>%
          dplyr::select(-hat,-orig_name) %>%
          dplyr::distinct() -> identity_data_subst_ready

        identity_pred <- identity_module(data = identity_data_subst_ready,
                                         module = vars_not_full_analysis %>% dplyr::filter(order == ord),
                                         classification = classify_variables(model$module_order))

        identity_pred %>%
          dplyr::select("time",dplyr::starts_with(dep_var)) %>%
          dplyr::select("time", dplyr::ends_with(".hat")) %>%
          dplyr::rename_with(.cols = dplyr::everything(), .fn = ~gsub(".level|.hat","",.)) %>%
          tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") -> data_to_add


        model$full_data %>%
          dplyr::filter(!(na_item == dep_var & time %in% cur_target_dates)) %>% # delete the old data (should always be NA)
          dplyr::bind_rows(.,data_to_add) %>%
          dplyr::arrange(dplyr::desc(time), na_item) -> model$full_data

        model$processed_input_data %>%
          dplyr::filter(!(na_item == dep_var & time %in% cur_target_dates)) %>% # delete the old data (should always be NA)
          dplyr::bind_rows(.,data_to_add) %>%
          dplyr::arrange(dplyr::desc(time), na_item) -> model$processed_input_data
      }
    }


    out <- list()
    out$nowcast_model <- model
    out$orig_model <- orig_model
    return(out)

  }



}
