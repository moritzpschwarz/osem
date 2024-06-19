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
forecast_setup_estimated_relationships <- function(model, i, exog_df_ready, n.ahead, current_spec, prediction_list, uncertainty_sample, nowcasted_data, full_exog_predicted_data = NULL) {

  # set up -----------
  # get isat obj
  model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model) %>% .[[1]] -> isat_obj

  # get data obj (that is the data that was used in the estimation)
  model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$dataset) %>% .[[1]] -> data_obj

  # determine ARDL or ECM
  is_ardl <- is.null(model$args$ardl_or_ecm) | identical(model$args$ardl_or_ecm,"ARDL")

  # determine log y
  ylog <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$use_logs %in% c("both","y")

  # determine log x
  xlog <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$use_logs %in% c("both","x")

  # determine x vars
  x_vars_basename <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$x_vars_basename

  y_vars_basename <- model$module_collection %>%
    dplyr::filter(.data$order == i) %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]] %>%
    .$dep_var_basename

  # check quarterly dummies to drop
  q_pred_todrop <- c("q_1","q_2","q_3","q_4")[!c("q_1","q_2","q_3","q_4") %in% colnames(isat_obj$aux$mX)]

  # check if mconst is used
  if ("mconst" %in% colnames(isat_obj$aux$mX)) {
    mconst <- TRUE
  } else {
    mconst <- FALSE
  }

  # identify any ar terms in the estimated data
  pred_ar_needed <- colnames(isat_obj$aux$mX)[grepl("ar[0-9]+",colnames(isat_obj$aux$mX))]
  # identify any dl terms in the estimated data
  pred_dl_needed <- colnames(isat_obj$aux$mX)[grepl("^L[0-9]+",colnames(isat_obj$aux$mX))]

  # this condition checks whether there are any ar terms that need to be created
  if (!is.null(pred_ar_needed) & !identical(character(0),pred_ar_needed)) {

    # if we need AR terms, the following loop creates the names of those variables (incl. considering whether they are logged)
    ar_vec <- 0:max(as.numeric(gsub("ar","",pred_ar_needed)))
    y_names_vec <- c()
    for (ar in ar_vec) {
      # ar = 0
      y_names_vec <- c(y_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),y_vars_basename))
    }
  } else {
    # if we do not need any AR terms then we simply use the standard name (and add ln. if necessary)
    y_names_vec <- paste0(ifelse(ylog,"ln.",""),y_vars_basename)
    ar_vec <- 0
  }

  # TODO: check whether the AR vector is the correct one for the x variables
  if (!identical(character(0),x_vars_basename)) {
    x_names_vec_nolag <- paste0(ifelse(xlog,"ln.",""),x_vars_basename)
    x_names_vec <- c(x_names_vec_nolag, pred_dl_needed)
  } else {
    x_names_vec <- NULL
    x_names_vec_nolag <- NULL
  }

  # get iis dummies
  if (!is.null(gets::isatdates(isat_obj)$iis)) {
    iis_pred <- matrix(0,
                       nrow = nrow(exog_df_ready),
                       ncol = nrow(gets::isatdates(isat_obj)$iis),
                       dimnames  = list(NULL,
                                        gets::isatdates(isat_obj)$iis$breaks)) %>%
      dplyr::as_tibble()
  }

  # get sis dummies
  if (!is.null(gets::isatdates(isat_obj)$sis)) {
    sis_pred <- matrix(1,
                       nrow = nrow(exog_df_ready),
                       ncol = nrow(gets::isatdates(isat_obj)$sis),
                       dimnames  = list(NULL,
                                        gets::isatdates(isat_obj)$sis$breaks)) %>%
      dplyr::as_tibble()
  }

  if ("trend" %in% names(coef(isat_obj))) {
    trend_pred <- dplyr::tibble(trend = (max(isat_obj$aux$mX[,"trend"]) + 1):(max(isat_obj$aux$mX[,"trend"]) + n.ahead))
  }


  # adding together all variables apart from x-variables (so IIS, SIS, trends, etc.)  -------------------------------

  exog_df_ready %>%

    # select the relevant variables
    dplyr::select("time", dplyr::any_of(c("q_1","q_2","q_3","q_4")), dplyr::any_of(names(data_obj))) %>%

    # drop not used quarterly dummies
    dplyr::select(-dplyr::any_of(q_pred_todrop)) %>%

    {if ("trend" %in% names(coef(isat_obj))) {
      dplyr::bind_cols(.,trend_pred)
    } else { . }} %>%

    {if (!is.null(gets::isatdates(isat_obj)$iis)) {
      dplyr::bind_cols(.,iis_pred)
    } else { . }} %>%

    {if (!is.null(gets::isatdates(isat_obj)$sis)) {
      dplyr::bind_cols(.,sis_pred)
    } else { . }} %>%

    {if (xlog) {
      dplyr::mutate(.,
                    dplyr::across(.cols = dplyr::any_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
                    #dplyr::across(dplyr::starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}"
      )
    } else {.}} -> current_pred_raw

  current_pred_raw_all <- current_pred_raw

  # Deal with current_spec not being fully exogenous --------
  if (!all(current_spec$independent %in% names(exog_df_ready)) && !all(is.na(current_spec$independent))) {

    missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

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

      prediction_list %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull("all.estimates") %>%
        .[[1]] %>%
        dplyr::select(-"time") -> mvar_all.estimates


      mvar_euname <- model$module_collection %>%
        dplyr::filter(.data$index == mvar_model_index) %>%
        dplyr::pull("dependent")

      mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

      # name all the individual estimates
      colnames(mvar_all.estimates) <- paste0(mvar_name,".all.",seq(uncertainty_sample))

      # get all the individual estimates into a column of a tibble
      mvar_all.estimates.tibble <- dplyr::as_tibble(mvar_all.estimates) %>%
        dplyr::mutate(index = 1:dplyr::n()) %>%
        tidyr::nest(data = -"index") %>%
        dplyr::select(-"index") %>%
        setNames(paste0(mvar_name,".all"))

      # add the mean yhat estimates and the all estimates together
      mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
        setNames(mvar_name) #%>%
      # dplyr::bind_cols(mvar_all.estimates.tibble)

      # Old version not including the all.estimates
      # mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
      #   setNames(mvar_name)


      if (!mvar_name %in% x_names_vec_nolag) {
        if (paste0("ln.",mvar_name) %in% x_names_vec_nolag) {
          mvar_tibble %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), log, .names = "ln.{.col}")) %>%
            dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble

          mvar_all.estimates.tibble %>%
            dplyr::mutate(dplyr::across(dplyr::all_of(paste0(mvar_euname, ".all")), ~purrr::map(.,log), .names = "ln.{.col}")) %>%
            dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname,".all"))) -> mvar_all.estimates.tibble
        } else {
          stop("Error occurred in adding missing/lower estimated variables (likely identities) to a subsequent/higher model. This is likely being caused by either log specification or lag specifiction. Check code.")
        }
      }

      current_pred_raw <- dplyr::bind_cols(current_pred_raw,mvar_tibble)
      current_pred_raw_all <- dplyr::bind_cols(current_pred_raw_all,mvar_all.estimates.tibble)
    }
  }

  # checking the data for nowcasted data --------

  data_obj %>%
    dplyr::select("time", dplyr::all_of(x_names_vec_nolag)) -> historical_estimation_data

  # in this section we check whether any of the missing values are present in nowcasted data
  # we first check if there is even any historical data used (would not be true for e.g. AR models)
  if(historical_estimation_data %>% dplyr::select(-"time") %>% ncol() > 0){

    # then we check whether there are any lines in the historical data that are missing (often the case)
    historical_estimation_data %>%
      tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values") %>%
      dplyr::filter(is.na(.data$values)) -> missing_values_dataobj

    # then we check whether any of the missing values in the historical data are present in nowcasted or in the exog_df_ready data
    if (nrow(missing_values_dataobj) > 0 & !is.null(nowcasted_data)) {

      missing_values_dataobj %>%
        dplyr::mutate(basename = gsub("ln.","",.data$na_item)) %>%
        dplyr::left_join(nowcasted_data %>%
                           dplyr::rename(basename = "na_item",
                                         values_nowcast = "values"), by = c("time", "basename")) %>%

        dplyr::left_join(exog_df_ready %>%
                           tidyr::pivot_longer(-"time",names_to = "basename",
                                               values_to = "values_exog"), by = c("time", "basename")) %>%

        {if(!is.null(full_exog_predicted_data)){
          dplyr::left_join(.,full_exog_predicted_data %>%
                             tidyr::pivot_longer(-"time",names_to = "basename",
                                                 values_to = "values_exog_full"), by = c("time", "basename"))} else {.}} %>%


        # where there are nowcast values but not original ones, take now the nowcast ones
        # when doing this, we check first if we need to log them
        dplyr::mutate(values = dplyr::case_when(!is.na(.data$values_nowcast) & is.na(.data$values) ~ .data$values_nowcast, TRUE ~ .data$values)) %>%

        # now do the same with exog values; where there are exog values but not original or nowcast ones, take now the exog ones
        dplyr::mutate(values = dplyr::case_when(!is.na(.data$values_exog) & is.na(.data$values) ~ .data$values_exog, TRUE ~ .data$values)) %>%

        {if(!is.null(full_exog_predicted_data)){
          # we do the same for the case that this function is called within the nowcast() function.
          # we need this particular line in the cases where a co-variate for nowcasting is missing in its lag.
          # an example: nowcasting a = b + c for date t. For time t also b and c are missing, so have to be taken from the exogenously forecasted values.
          # then, if everything is contemporaneous, a can be nowcasted
          # however, if we e.g. need the first lag of c to nowcast a and that lag is missing, then we need to go back to the full dataset of exogenously forecasted values
          # we then take that value as well.
          dplyr::mutate(.,values = dplyr::case_when(!is.na(.data$values_exog_full) & is.na(.data$values) ~ .data$values_exog_full, TRUE ~ .data$values))
        } else {.}} %>%

        # now we check first if we need to log them
        dplyr::mutate(values = dplyr::case_when(!is.na(.data$values) & grepl("^ln.",.data$na_item) ~ log(.data$values), TRUE ~ .data$values)) %>%

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

    ########### TODO CHHHHEEEEEEECK. Don't think this makes sense. This happens if e.g. a value for one variable is released later
    # The drop_na below was used because for GCapitalForm the value for July 2022 was missing - while it was there for FinConsExpHH
    # Now the question is whether the drop_na messes up the timing
    #tidyr::drop_na() %>% # UNCOMMENT THIS WHEN NOT HAVING A FULL DATASET

    dplyr::bind_rows(current_pred_raw %>%
                       #dplyr::select(time, dplyr::all_of(x_names_vec_nolag), dplyr::any_of("trend"))) -> intermed
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
    tidyr::drop_na() %>%
    dplyr::select(-"time") %>%
    dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) %>%
    return() -> pred_df

  #print(pred_df)

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
    #data_obj %>%
    # dplyr::select(time, dplyr::all_of(x_names_vec_nolag)) %>%
    historical_estimation_data_w_nowcast %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(x_names_vec_nolag), ~as.list(.))) %>%

      ########### TODO CHHHHEEEEEEECK. Don't think this makes sense. This happens if e.g. a value for one variable is released later
      # The drop_na below was used because for GCapitalForm the value for July 2022 was missing - while it was there for FinConsExpHH
      # Now the question is whether the drop_na messes up the timing
      #tidyr::drop_na() %>% # UNCOMMENT THIS WHEN NOT HAVING A FULL DATASET

      dplyr::bind_rows(current_pred_raw_all %>%
                         #dplyr::select(time, dplyr::all_of(x_names_vec_nolag), dplyr::any_of("trend"))) -> intermed
                         dplyr::rename_with(dplyr::everything(), .fn = ~gsub(".all","",.)) %>%
                         dplyr::mutate(dplyr::across(-"time", .fn = ~as.list(.))) %>%
                         dplyr::select("time", dplyr::all_of(paste0(x_names_vec_nolag)))) -> intermed.all


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
      tidyr::drop_na() %>%
      dplyr::select(-"time") %>%
      dplyr::select(dplyr::any_of(row.names(isat_obj$mean.results))) %>%
      return() -> pred_df.all
  }


  # Final output data -------------------------------------------------------



  final_i_data <- dplyr::tibble(
    data = list(intermed %>%
                  dplyr::left_join(current_pred_raw %>% dplyr::select("time", dplyr::starts_with("q_"),
                                                                      dplyr::starts_with("iis"),
                                                                      dplyr::starts_with("sis")),
                                   by = "time") %>%
                  tidyr::drop_na()))

  out <- list()
  out$pred_df <- pred_df
  out$isat_obj <- isat_obj
  out$final_i_data <- final_i_data
  out$chk_any_listcols <- chk_any_listcols
  out$current_pred_raw <- current_pred_raw
  out$current_pred_raw_all <- if(exists("current_pred_raw_all")){current_pred_raw_all}
  out$pred_df.all <- if(exists("pred_df.all")){pred_df.all}
  return(out)

}
