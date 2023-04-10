#' Forecast Aggregate model
#'
#' @param model An aggregate model of class 'aggmod'
#' @param exog_predictions A data.frame or tibble with values for the exogenous values. The number of rows of this data must be equal to n.ahead.
#' @param n.ahead Periods to forecast ahead
#' @param ci.levels Numeric vector. Vector with confidence intervals to be calculated. Default: c(0.5,0.66,0.95)
#' @param ar.fill.max Integer. When no exogenous values have been provided, these must be inferred. If option 'exog_fill_method = "AR"' then an autoregressive model is used to further forecast the exogenous values. This options determines the number of AR terms that should be used. Default is 4.
#' @param exog_fill_method Character, either 'AR' or 'last'. When no exogenous values have been provided, these must be inferred. When option 'exog_fill_method = "AR"' then an autoregressive model is used to further forecast the exogenous values. With 'last', simply the last available value is used.
#' @param plot.forecast Logical. Should the result be plotted? Default is TRUE.
#'
#' @return An object of class aggmod.forecast
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
#' fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
#' fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
#' filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa,
#' "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)
#' \dontrun{
#' a <- run_model(specification = spec, dictionary = NULL,
#' inputdata_directory = NULL, filter_list = filter_list,
#' download = TRUE, save_to_disk = NULL, present = FALSE)
#' forecast(a)
#' }


forecast_model <- function(model,
                           exog_predictions = NULL,
                           n.ahead = 10,
                           ci.levels = c(0.5,0.66,0.95),
                           exog_fill_method = "AR",
                           ar.fill.max = 4,
                           plot.forecast = TRUE,
                           uncertainty_sample = 100,
                           seed = 1234){

  #if(class(model) != "aggmod"){stop("Forecasting only possible with an aggmod object. Execute 'run_model' to get such an object.")}
  if(!isa(model, "aggmod")){stop("Forecasting only possible with an aggmod object. Execute 'run_model' to get such an object.")}
  if(!is.null(exog_fill_method) & !exog_fill_method %in% c("AR","last")){stop("The method to fill exogenous values 'exog_fill_method' can only be either NULL (when data is provided), or 'AR' or 'last'.")}
  if(!is.null(ar.fill.max) & (!is.integer(as.integer(ar.fill.max)) | ar.fill.max < 1)){stop("The option 'ar.fill.max' can either be NULL or must be an integer that is larger than 0.")}

  # 1. Determine Exogenous Variables and wrangle future values ---------------

  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = model$module_order_eurostatvars)

  classification %>%
    dplyr::filter(.data$class == "x") %>%
    dplyr::pull(.data$var) -> exog_vars

  exog_df_ready <- forecast_exogenous_values(model = model,
                                             exog_vars = exog_vars,
                                             exog_predictions = exog_predictions,
                                             exog_fill_method = exog_fill_method,
                                             ar.fill.max = ar.fill.max,
                                             n.ahead = n.ahead)

  # 2. Forecasting step by step according to model order ------------------------------------------------
  prediction_list <- dplyr::tibble(
    index = model$module_order_eurostatvars$index,
    order = model$module_order_eurostatvars$order,
    dep_var = model$module_order_eurostatvars$dependent,
    predict.isat_object = list(NA_complex_),
    data = list(NA_complex_),
    central.estimate = list(NA_complex_) #,
    #all.estimates = list(NA_complex_)
  )

  # cycling through each module
  for(i in seq(model$module_order_eurostatvars$order)){
    # i = 1

    current_spec <- model$module_order_eurostatvars %>%
      dplyr::filter(.data$order == i) %>%

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

    if(model$module_order_eurostatvars$type[model$module_order_eurostatvars$order == i] != "d"){

      pred_setup_list <- forecast_setup_estimated_relationships(model = model,
                                                          i = i,
                                                          exog_df_ready = exog_df_ready,
                                                          n.ahead = n.ahead,
                                                          current_spec = current_spec)

      final_i_data <- pred_setup_list$final_i_data
      pred_df <- pred_setup_list$pred_df
      isat_obj <- pred_setup_list$isat_obj
      chk_any_listcols <- pred_setup_list$chk_any_listcols
      current_pred_raw <- pred_setup_list$current_pred_raw

      if(!is.null(pred_setup_list$pred_df.all)){
        pred_df.all <- pred_setup_list$pred_df.all
      }

      # Predict
      isat_obj$call$ar <- isat_obj$aux$args$ar
      isat_obj$call$mc <- isat_obj$aux$args$mc

      pred_obj <- gets::predict.isat(isat_obj, newmxreg = as.matrix(utils::tail(pred_df, n.ahead)),
                                     n.ahead = n.ahead, plot = plot.forecast,
                                     ci.levels = ci.levels)

      # make samples from the model residuals and add them to the mean prediction
      set.seed(seed)
      res_draws <- sample(as.numeric(isat_obj$residuals), size = uncertainty_sample*n.ahead, replace = TRUE)
      pred_draw_matrix <- as.vector(pred_obj$yhat) + matrix(res_draws,nrow = n.ahead)

      # if there are any list columns then that means that a preceding variable has uncertainty
      # then the pred_draw_matrix is replaced with the uncertainty estimates
      if(chk_any_listcols){
        preds_runs <- dplyr::tibble()
        for(run in seq_along(1:uncertainty_sample)){

          pred_df.all %>%
            dplyr::mutate(across(-c(dplyr::any_of("trend"),
                                    dplyr::starts_with("q_"),
                                    dplyr::starts_with("iis"),
                                    dplyr::starts_with("sis")),
                                 ~purrr::map(., function(x){
                                   if(!is.null(ncol(x))){
                                     dplyr::pull(x, run)
                                   } else {
                                     x
                                   }})
            )) %>%
            tidyr::unnest(-c(dplyr::any_of("trend"), dplyr::starts_with("q_"),dplyr::starts_with("iis"), dplyr::starts_with("sis"))) %>%
            utils::tail(., n.ahead) %>%
            as.matrix -> pred_df_run

          pred_obj_run <- gets::predict.isat(isat_obj, newmxreg = pred_df_run,
                                             n.ahead = n.ahead, plot = FALSE,
                                             ci.levels = ci.levels, n.sim = 1)

          dplyr::tibble(run = run,
                        time = 1:n.ahead,
                        pred = as.numeric(pred_obj_run$yhat)) %>%
            dplyr::bind_rows(preds_runs, .) -> preds_runs
        }
        # here pred only takes into account the uncertainty in the x-variables
        # pred_draws combines the model residual uncertainty for y and the uncertainty of the x-variables
        preds_runs %>%
          dplyr::mutate(pred_draws = pred + res_draws) -> pred_runs_final

        pred_runs_final %>%
          dplyr::select(c("run","time","pred_draws")) %>%
          tidyr::pivot_wider(id_cols = "time", names_from = "run", values_from = "pred_draws") %>%
          dplyr::select(-"time") %>%
          as.matrix() -> pred_runs_final_matrix

        dimnames(pred_runs_final_matrix) <- NULL

        # now replace the pred_draw_matrix - this one only survives without being overwritten if there is no preceding uncertainty
        pred_draw_matrix <- pred_runs_final_matrix

      }

      outvarname <- paste0(if (model$module_collection %>%
                               dplyr::filter(.data$order == i) %>%
                               .$model.args %>%
                               .[[1]] %>%
                               .$use_logs %in% c("both","y")) {"ln."} else {""},
                           current_spec %>% dplyr::pull("dependent") %>% unique)

      dplyr::tibble(time = current_pred_raw %>% dplyr::pull(.data$time),
                    value = as.numeric(pred_obj[,1])) %>%
        setNames(c("time",outvarname)) -> central_estimate

      # dplyr::tibble(time = current_pred_raw %>% dplyr::pull(time)) %>%
      #   dplyr::bind_cols(pred_obj[,-1]) %>%
      #   setNames(c("time",paste0(outvarname,".", gsub("^y","",names(pred_obj[,-1]))))) -> all_estimates

      colnames(pred_draw_matrix) <- paste0("run_",1:uncertainty_sample)
      pred_draw_matrix <- dplyr::as_tibble(pred_draw_matrix) %>%
        dplyr::bind_cols(dplyr::tibble(time = current_pred_raw$time), .)


      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::as_tibble(pred_obj)))
      prediction_list[prediction_list$order == i, "data"] <- final_i_data
      prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(central_estimate = list(central_estimate))
      prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(all_estimates = list(pred_draw_matrix))


    } else {
      # Loop goes to this part if the type of the module is not "d"
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

      dplyr::tibble(time = current_pred_raw %>% dplyr::pull(.data$time),
                    value = as.numeric(identity_pred_final[,1])) %>%
        setNames(c("time",outvarname)) -> central_estimate

      # just change the type of the object holding all estimates
      identity_pred_final.all <- as.matrix(identity_pred_final.all)
      colnames(identity_pred_final.all) <- paste0("run_",1:uncertainty_sample)
      identity_pred_final.all <- dplyr::as_tibble(identity_pred_final.all) %>%
        dplyr::bind_cols(dplyr::tibble(time = current_pred_raw$time), .)


      names(identity_pred_final) <- unique(current_spec$dependent)
      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::tibble(yhat = identity_pred_final[,1])))
      prediction_list[prediction_list$order == i, "data"] <- dplyr::tibble(data = list(dplyr::bind_cols(identity_pred_final, identity_pred)))
      prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(data = list(central_estimate))
      prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(data = list(identity_pred_final.all))
    }
  }

  out <- list()
  out$forecast <- prediction_list
  out$orig_model <- model
  out$dictionary <- model$dictionary
  out$exog_data <- exog_df_ready

  class(out) <- "aggmod.forecast"

  if(plot.forecast){
    plot.aggmod.forecast(out)
  }

  return(out)

}
