
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

      # if (!mvar_name %in% x_names_vec_nolag) {
      #   if (paste0("ln.",mvar_name) %in% x_names_vec_nolag) {
      #
      #     log_possible <- all(mvar_tibble[, mvar_euname, drop = TRUE] > 0)
      #     # TODO record that log_possible chose asinh
      #     if(log_possible){
      #       mvar_tibble %>%
      #         dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), log, .names = "ln.{.col}")) %>%
      #         dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble
      #       mvar_all.estimates.tibble %>%
      #         dplyr::mutate(dplyr::across(dplyr::all_of(paste0(mvar_euname, ".all")), ~purrr::map(.,log), .names = "ln.{.col}")) %>%
      #         dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname,".all"))) -> mvar_all.estimates.tibble
      #     } else {
      #       mvar_tibble %>%
      #         dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), asinh, .names = "ln.{.col}")) %>%
      #         dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble
      #       mvar_all.estimates.tibble %>%
      #         dplyr::mutate(dplyr::across(dplyr::all_of(paste0(mvar_euname, ".all")), ~purrr::map(.,asinh), .names = "ln.{.col}")) %>%
      #         dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname,".all"))) -> mvar_all.estimates.tibble
      #     }
      #   } else {
      #     stop("Error occurred in adding missing/lower estimated variables (likely identities) to a subsequent/higher model. This is likely being caused by either log specification or lag specifiction. Check code.")
      #   }
      # }

      # this first adds the correct time dimension to mvar_tibble
      exog_df_ready %>%
        dplyr::select("time") %>%
        dplyr::slice((dplyr::n() - (nrow(mvar_tibble)-1)) : dplyr::n()) %>%
        dplyr::bind_cols(mvar_tibble) -> mvar_tibble_time

      #mvar_tibble <- mvar_tibble_time

      # if the variable is already there (e.g. through nowcasting) then combine them
      #if(all(names(mvar_tibble) %in% names(current_pred_raw))){

      # # now we rename it to then be able to join it
      # dplyr::rename_with(.cols = dplyr::all_of(names(mvar_tibble)), .fn = ~paste0("new.",.)) %>%
      #
      # # then we join it
      # dplyr::full_join(current_pred_raw %>%
      #                    dplyr::select("time", dplyr::all_of(names(mvar_tibble))), by = "time") %>%
      #
      # dplyr::arrange(.data$time) %>%
      #
      # # and combine the two columns so that each value is filled
      # dplyr::transmute(!!dplyr::sym(names(mvar_tibble)) := dplyr::case_when(
      #   is.na(.[[names(mvar_tibble)]]) ~ .[[paste0("new.",names(mvar_tibble))]],
      #   TRUE ~ .[[names(mvar_tibble)]]
      # )) -> mvar_tibble

      # now we do the same for the all estimates
      # mvar_tibble_time %>%
      #   dplyr::bind_cols(mvar_all.estimates.tibble) %>%
      #   dplyr::rename_with(.cols = dplyr::all_of(names(mvar_all.estimates.tibble)), .fn = ~paste0("new.",.)) %>%
      #   dplyr::full_join(current_pred_raw_all %>%
      #                      dplyr::select("time", dplyr::all_of(names(mvar_tibble))), by = "time") %>%
      #
      #   dplyr::arrange(.data$time) %>%
      #
      #   dplyr::transmute(!!dplyr::sym(names(mvar_all.estimates.tibble)) := dplyr::case_when(
      #     is.na(.[[names(mvar_tibble)]]) ~ .[[paste0("new.",names(mvar_all.estimates.tibble))]],
      #     TRUE ~ purrr::map(.[[names(mvar_tibble)]], ~ .x)  # Wrap each individual value in a list
      #   )) -> mvar_all.estimates.tibble

      #   current_pred_raw %>%
      #     dplyr::select(-dplyr::all_of(names(mvar_tibble))) -> current_pred_raw
      #
      #   current_pred_raw_all %>%
      #     dplyr::select(-dplyr::all_of(names(mvar_tibble))) -> current_pred_raw_all
      #
      # }
      #
      # current_pred_raw <- dplyr::bind_cols(current_pred_raw,mvar_tibble)
      # current_pred_raw_all <- dplyr::bind_cols(current_pred_raw_all,mvar_all.estimates.tibble)
    }
  }

  # central estimate
  cvar_pred <- predict(varm, dumvar = as.matrix(mvar_tibble))

  dplyr::tibble(names = names(cvar_pred$fcst),
                models = cvar_pred$fcst) %>%
    dplyr::mutate(fcst_values = purrr::map(models, ~ .x[,"fcst"])) %>%
    dplyr::select(-"models") %>%
    tidyr::unnest(fcst_values)  -> cvar_forecasts

  cvar_forecasts <- dplyr::tibble(
    names = names(cvar_pred$fcst),
    models = cvar_pred$fcst
  ) %>%
    dplyr::mutate(
      # Add time index and extract all columns as tibbles
      fcst_df = purrr::map(models, ~ {
        dplyr::as_tibble(.x) %>%
          dplyr::mutate(time = seq_len(nrow(.x)))
      })
    ) %>%
    dplyr::select(-models) %>%
    tidyr::unnest(fcst_df)

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
    cvar_pred.all <- predict(varm, dumvar = as.matrix(mvar_all.estimates.single.time[,2]))

    # store results
    if(j == 1){
      cvar_forecasts.all <- dplyr::tibble(
        na_item = names(cvar_pred.all$fcst),
        models = cvar_pred.all$fcst
      ) %>%
        dplyr::mutate(
          # Add time index and extract all columns as tibbles
          fcst_df = purrr::map(models, ~ {
            dplyr::as_tibble(.x) %>%
              dplyr::mutate(time = seq_len(nrow(.x)))
          })
        ) %>%
        dplyr::select(-models) %>%
        tidyr::unnest(fcst_df) %>%
        dplyr::mutate(iteration = j)
    } else {
      cvar_forecasts.j <- dplyr::tibble(
        na_item = names(cvar_pred.all$fcst),
        models = cvar_pred.all$fcst
      ) %>%
        dplyr::mutate(
          # Add time index and extract all columns as tibbles
          fcst_df = purrr::map(models, ~ {
            dplyr::as_tibble(.x) %>%
              dplyr::mutate(time = seq_len(nrow(.x)))
          })
        ) %>%
        dplyr::select(-models) %>%
        tidyr::unnest(fcst_df) %>%
        dplyr::mutate(iteration = j)

      cvar_forecasts.all <- dplyr::bind_rows(cvar_forecasts.all, cvar_forecasts.j)
    }
  }

  out <- list()
  out$central <- cvar_forecasts
  out$all <- cvar_forecasts.all

  return(out)
}
