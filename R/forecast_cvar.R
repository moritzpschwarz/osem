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
  } else {

    mvar_tibble <- dplyr::tibble(.rows = nrow(exog_df_ready)) # empty tibble
    mvar_all.estimates.tibble <- dplyr::tibble(.rows = nrow(exog_df_ready)) # empty tibble
  }

  # add exogenous and original data together
  mvar_tibble <- exog_df_ready %>%
    dplyr::bind_rows(model$processed_input_data %>%
                       tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values"),.) %>%
    dplyr::mutate(
      dplyr::across(-c("time", dplyr::starts_with("q_")), .fns = ~ if (any(. <= 0, na.rm = TRUE)) {
        asinh(.)
      } else {
        log(.)
      }, .names = "ln.{.col}"),
      dplyr::across(-"time", list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
    ) %>%

    tidyr::pivot_longer(-"time") %>%
    tidyr::drop_na() %>%
    dplyr::filter(.data$time %in% exog_df_ready$time) %>%

    tidyr::pivot_wider(id_cols = "time", names_from = "name", values_from = "value") %>%


    {if(ncol(mvar_tibble) > 0){
      dplyr::full_join(., mvar_tibble %>%
                         dplyr::mutate(time = exog_df_ready$time), by = "time")
    } else {.}} %>%

    # only select variables needed in varm object
    dplyr::select(dplyr::any_of(colnames(varm$datamat))) %>%
    # remove dependent variables
    dplyr::select(-dplyr::any_of(c(strsplit(current_spec$dependent,",")[[1]],
                                   paste0("ln.",strsplit(current_spec$dependent,",")[[1]]),
                                   paste0("D.",strsplit(current_spec$dependent,",")[[1]]),
                                   paste0("D.ln.",strsplit(current_spec$dependent,",")[[1]]))))

  # central estimate
  cvar_pred <- stats::predict(varm, dumvar = as.matrix(mvar_tibble))

  cvar_forecasts <- dplyr::tibble(
    na_item = names(cvar_pred$fcst),
    model   = cvar_pred$fcst
  ) %>%
    dplyr::mutate(
      fcst_df = purrr::map(.data$model, ~ dplyr::as_tibble(.x) %>%
                             dplyr::mutate(time = dplyr::row_number()) %>%
                             dplyr::select("time", "fcst"))
    ) %>%
    dplyr::select(-"model") %>%
    tidyr::unnest("fcst_df")

  # sample residuals
  index_draws <- sample(1:nrow(varm$resid), size = uncertainty_sample * n.ahead, replace = TRUE)

  res_draws_matrix <- varm$resid[index_draws, ] %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      draw = rep(seq_len(uncertainty_sample), each = n.ahead),
      time = rep(seq_len(n.ahead), times = uncertainty_sample)
    ) %>%
    tidyr::pivot_longer(
      -c("draw", "time"),
      names_to = "resid_name",
      values_to = "resid_value"
    ) %>%
    dplyr::mutate(resid_name = stringr::str_remove(.data$resid_name, "^resids of\\s+")) %>%
    dplyr::arrange(.data$resid_name, .data$draw, .data$time) %>%
    dplyr::mutate(resid_value = cumsum(.data$resid_value), .by = c("draw","resid_name")) %>%
    dplyr::mutate(draw = paste0("run_", .data$draw)) %>%
    dplyr::group_by(.data$time, .data$resid_name) %>%
    tidyr::nest(data = c(.data$draw, .data$resid_value)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      id_cols = "time",
      names_from = "resid_name",
      values_from = "data",
      names_glue = "{resid_name}.all"
    )

  if(ncol(mvar_all.estimates.tibble) == 0){
    cvar_forecasts.all <- dplyr::tibble(
      na_item = names(cvar_pred$fcst),
      models = cvar_pred$fcst
    ) %>%
      dplyr::mutate(
        fcst_df = purrr::map(.data$models, ~ {
          dplyr::as_tibble(.x) %>%
            dplyr::mutate(time = dplyr::row_number())
        })
      ) %>%
      dplyr::select(-"models") %>%
      tidyr::unnest("fcst_df") %>%

      # add the drawn samples from the CVAR model residuals
      dplyr::full_join(res_draws_matrix %>%
                         dplyr::rename_with(~ sub("\\.all$", "", .x)) %>%
                         tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "resid_draws"),
                       by = c("na_item","time")) %>%

      # Add time index and extract all columns as tibbles
      dplyr::mutate(fcst = purrr::map2(.data$fcst, .data$resid_draws, .f = function(fcst,resid){
        if (is.null(resid) || (length(resid) == 1 && all(is.na(resid)))) {
          dplyr::tibble(draw = NA_character_, resid_value = fcst)
        } else {
          resid %>% dplyr::mutate(resid_value = .data$resid_value + fcst)
        }
      })) %>%
      dplyr::select("na_item", "fcst", "time") %>%
      tidyr::unnest("fcst") %>%
      dplyr::rename(fcst = "resid_value", iteration = "draw") %>%
      dplyr::mutate(iteration = as.integer(gsub("^run_", "", .data$iteration)))

  } else {
    # all estimates - cycle through mvar_all.estimates
    # preallocate list for results (faster than bind_rows inside the loop)
    cvar_forecasts_list <- vector("list", uncertainty_sample)

    for (j in seq_len(uncertainty_sample)) {

      # pick j-th draw from the stored uncertainties (only in *.all columns)
      mvar_all.estimates.single <- mvar_all.estimates.tibble %>%
        dplyr::mutate(
          dplyr::across(dplyr::ends_with(".all"), ~ purrr::map(.x, \(v) v[[j]]))
        )

      # combine with time index
      exog_df_ready %>%
        dplyr::select("time") %>%
        dplyr::slice_tail(n = nrow(mvar_all.estimates.single)) %>%
        dplyr::bind_cols(mvar_all.estimates.single) %>%
        tidyr::unnest(dplyr::ends_with(".all")) %>%
        dplyr::rename_with(.cols = dplyr::ends_with(".all"), ~gsub("\\.all$","",.)) -> mvar_all.estimates.single.time

      # predict
      dumvar_mat <- mvar_all.estimates.single.time %>%
        dplyr::select(-"time") %>%
        as.matrix()

      cvar_pred.all <- stats::predict(varm, dumvar = dumvar_mat)

      # store results
      cvar_forecasts_list[[j]] <- dplyr::tibble(
        na_item = names(cvar_pred.all$fcst),
        models = cvar_pred.all$fcst
      ) %>%
        dplyr::mutate(
          # Add time index and extract all columns as tibbles
          fcst_df = purrr::map(.data$models, ~ {
            dplyr::as_tibble(.x) %>%
              dplyr::mutate(time = dplyr::row_number())
          })
        ) %>%
        dplyr::select(-"models") %>%
        tidyr::unnest("fcst_df") %>%
        dplyr::mutate(iteration = j)
    }

    # bind once at the end
    cvar_forecasts.all <- dplyr::bind_rows(cvar_forecasts_list)

    # now add current uncertainty
    residual_draws_long <- res_draws_matrix %>%
      dplyr::rename_with(~ sub("\\.all$", "", .x), dplyr::ends_with(".all")) %>%
      tidyr::pivot_longer(-"time", names_to  = "na_item", values_to = "resid_draws") %>%
      tidyr::unnest("resid_draws") %>%                      # exposes draw + resid_value
      dplyr::mutate(iteration = as.integer(gsub("^run_", "", .data$draw))) %>%
      dplyr::select(.data$na_item, .data$time, .data$iteration, resid_cum = .data$resid_value)

    cvar_forecasts.all <- cvar_forecasts.all %>%
      dplyr::left_join(residual_draws_long, by = c("na_item", "time", "iteration")) %>%
      dplyr::mutate(fcst = .data$fcst + .data$resid_cum) %>%
      dplyr::select(-c("resid_cum", "lower", "upper", "CI"))
  }

  out <- list()
  out$central <- cvar_forecasts
  out$all <- cvar_forecasts.all

  return(out)
}
