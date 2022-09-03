#' Forecast Aggregate model
#'
#' @param model An aggregate model of class 'aggmod'
#' @param exog_predictions A data.frame or tibble with values for the exogenous values. The number of rows of this data must be equal to n.ahead.
#' @param n.ahead Periods to forecast ahead
#' @param plot.forecast Logical. Should the result be plotted? Default is TRUE.
#'
#' @return
#' @export
#'
#' @examples
forecast_model <- function(model, exog_predictions = NULL, n.ahead = 10, plot.forecast = TRUE){

  if(class(model) != "aggmod"){stop("Forecasting only possible with an aggmod object. Execute 'run_model' to get such an object.")}


  # 1. Determine Exogenous Variables and wrangle future values ---------------

  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = model$module_order_eurostatvars)

  classification %>%
    filter(class == "x") %>%
    pull(var) -> exog_vars


  if(is.null(exog_predictions)){
    message("No exogenous values provided. Model will use the last available value.")
    exog_df <- model$full_data %>%
      filter(na_item %in% exog_vars) %>%
      group_by(na_item) %>%
      filter(time == max(time)) %>%
      mutate(na_item = janitor::make_clean_names(na_item))

    if(!all(exog_df$time == exog_df$time[1])){
      warning("Latest Exogenous Data is not available for all variables. For those where most recent data is not available, the period before that is used.")
    }
  }


  exog_df %>%
    group_by(na_item) %>%
    rowwise() %>%
    mutate(time = list(seq(time, length = n.ahead+1, by = "3 months")[1:n.ahead+1])) %>%
    unnest(time) %>%
    ungroup %>%
    mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
    pivot_wider(names_from = "na_item",values_from = "values", id_cols = c(time,q)) %>%

    fastDummies::dummy_cols(
      select_columns = "q", remove_first_dummy = FALSE,
      remove_selected_columns = TRUE) -> exog_df_ready


  # 2. Forecasting step by step according to model order ------------------------------------------------
  prediction_list <- tibble(
    index = model$module_order_eurostatvars$index,
    order = model$module_order_eurostatvars$order,
    predict.isat_object = list(NA_complex_),
    data = list(NA_complex_)
  )

  for(i in seq(model$module_order_eurostatvars$order)){
    # i = 1
    current_spec <- model$module_order_eurostatvars %>%
      filter(order == i) %>%
      mutate(independent = gsub(" ", "", independent),
             independent_eu = gsub(" ", "", independent_eu),) %>%
      rowwise() %>%
      mutate(#ind_vars = list(strsplits(independent,c("\\-", "\\+"))),
        ind_vars_eu = list(strsplits(independent_eu,c("\\-", "\\+")))) %>%
      # following line added to deal with AR models wehn ind_vars is a list of NULL
      bind_rows(tibble(#ind_vars = list(""),
        ind_vars_eu = list(""))) %>%
      #unnest(ind_vars, keep_empty = TRUE) %>%
      unnest(ind_vars_eu, keep_empty = TRUE) %>%
      drop_na(index) %>%
      select(index, dependent_eu, independent_eu,
             #ind_vars,
             ind_vars_eu) %>%
      mutate(independent_eu = tolower(independent_eu),
             #ind_vars = tolower(ind_vars),
             ind_vars_eu = tolower(ind_vars_eu))

    if(model$module_order_eurostatvars$type[model$module_order_eurostatvars$order == i] != "d"){
      # set up
      # get isat obj
      model$module_collection %>%
        filter(order == i) %>%
        pull(model) %>% .[[1]] -> isat_obj

      # get data obj
      model$module_collection %>%
        filter(order == i) %>%
        pull(dataset) %>% .[[1]] -> data_obj

      # isat_obj$aux$mX %>% nrow
      # data_obj %>% nrow
      # colnames(isat_obj$aux$mX)
      # names(data_obj)

      # determine ARDL or ECM
      is_ardl <- is.null(model$args$ardl_or_ecm) | identical(model$args$ardl_or_ecm,"ARDL")

      # determine log y
      ylog <- model$module_collection %>%
        filter(order == i) %>%
        pull(model.args) %>%
        .[[1]] %>%
        .$use_logs %in% c("both","y")

      # determine log x
      xlog <- model$module_collection %>%
        filter(order == i) %>%
        pull(model.args) %>%
        .[[1]] %>%
        .$use_logs %in% c("both","x")

      # determine x vars
      x_vars_basename <- model$module_collection %>%
        filter(order == i) %>%
        pull(model.args) %>%
        .[[1]] %>%
        .$x_vars_basename

      y_vars_basename <- model$module_collection %>%
        filter(order == i) %>%
        pull(model.args) %>%
        .[[1]] %>%
        .$dep_var_basename

      # check quarterly dummies
      q_pred_todrop <- c("q_1","q_2","q_3","q_4")[!c("q_1","q_2","q_3","q_4") %in% colnames(isat_obj$aux$mX)]

      # check if mconst is used
      if("mconst" %in% colnames(isat_obj$aux$mX)){
        mconst <- TRUE
      } else {
        mconst <- FALSE
      }

      # get ar
      pred_ar_needed <- colnames(isat_obj$aux$mX)[grepl("ar[0-9]+",colnames(isat_obj$aux$mX))]

      if(!is.null(pred_ar_needed)){
        ar_vec <- 0:max(as.numeric(gsub("ar","",pred_ar_needed)))
        y_names_vec <- c()
        for(ar in ar_vec){
          # ar = 0
          y_names_vec <- c(y_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),y_vars_basename))
        }
      } else {
        y_names_vec <- paste0(ifelse(ylog,"ln.",""),y_vars_basename)
      }

      if(!identical(character(0),x_vars_basename)){
        x_names_vec <- c()
        for(ar in ar_vec){
          # ar = 0
          x_names_vec <- c(x_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),x_vars_basename))
        }

        x_names_vec_nolag <- paste0(ifelse(ylog,"ln.",""),x_vars_basename)
      } else {
        x_names_vec <- NULL
        x_names_vec_nolag <- NULL
      }

      # get iis dummies
      if(!is.null(gets::isatdates(isat_obj)$iis)){
        iis_pred <- matrix(0,
                           nrow = nrow(exog_df_ready),
                           ncol = nrow(gets::isatdates(isat_obj)$iis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_obj)$iis$breaks)) %>%
          as_tibble()
      }

      # get sis dummies
      if(!is.null(gets::isatdates(isat_obj)$sis)){
        sis_pred <- matrix(1,
                           nrow = nrow(exog_df_ready),
                           ncol = nrow(gets::isatdates(isat_obj)$sis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_obj)$sis$breaks)) %>%
          as_tibble()
      }


      exog_df_ready %>%

        # select the relevant variables
        select(time, any_of(c("q_1","q_2","q_3","q_4")), any_of(names(data_obj))) %>%

        # drop not used quarterly dummies
        select(-any_of(q_pred_todrop)) %>%

        # {if(!identical(pred_ar_needed, character(0))){
        #   bind_cols(.,pred_ar)
        # } else {.}} %>%
        #
        # {if(mconst){
        #   bind_cols(.,tibble(mconst = 1))
        # } else { . }} %>%
        #
        {if(!is.null(gets::isatdates(isat_obj)$iis)){
          bind_cols(.,iis_pred)
        } else { . }} %>%

        {if(!is.null(gets::isatdates(isat_obj)$sis)){
          bind_cols(.,sis_pred)
        } else { . }} %>%

        {if(xlog){
          mutate(.,
                 across(.cols = any_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
                 #across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}"
          )
        } else {.}} -> current_pred_raw

      # Deal with current_spec not being fully exogenous
      if(!all(current_spec$ind_vars_eu %in% names(exog_df_ready)) && !is.na(current_spec$ind_vars_eu)){

        missing_vars <- current_spec$ind_vars_eu[!current_spec$ind_vars_eu %in% names(exog_df_ready)]

        for(mvar in missing_vars){
          # mvar = "p5g"

          model$module_order_eurostatvars %>%
            filter(tolower(dependent_eu) == mvar) %>%
            pull(index) -> mvar_model_index

          prediction_list %>%
            filter(index == mvar_model_index) %>%
            pull(predict.isat_object) %>%
            .[[1]] -> mvar_model_obj

          mvar_logs <- model$module_collection %>%
            filter(index == mvar_model_index) %>%
            with(model.args) %>%
            .[[1]] %>%
            .$use_logs

          mvar_euname <- model$module_collection %>%
            filter(index == mvar_model_index) %>%
            pull(dependent_eu)

          mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), tolower(mvar_euname))

          mvar_tibble <- tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
            setNames(mvar_name)

          current_pred_raw <- bind_cols(current_pred_raw,mvar_tibble)

        }
      }


      data_obj %>%
        select(time, all_of(x_names_vec_nolag)) %>%
        bind_rows(current_pred_raw %>%
                    select(time, all_of(x_names_vec_nolag))) -> intermed

      to_be_added <- tibble(.rows = nrow(intermed))
      for (j in 1:max(ar_vec)) {
        intermed %>%
          mutate(across(c(#starts_with("D."),
            starts_with("ln.")), ~ dplyr::lag(., n = j))) %>%
          select(c(#starts_with("D."),
            starts_with("ln."))) %>%
          rename_with(.fn = ~ paste0("L", j, ".", .)) %>%
          bind_cols(to_be_added, .) -> to_be_added
      }

      bind_cols(intermed, to_be_added) %>%
        left_join(current_pred_raw %>%
                    select(time, starts_with("q_"), starts_with("iis"), starts_with("sis")), by = "time") %>%
        drop_na %>%
        select(-time) -> pred_df

      # Predict
      isat_obj$call$ar <- isat_obj$aux$args$ar
      pred_obj <- predict(isat_obj, newmxreg = as.matrix(pred_df),
                          n.ahead = n.ahead, plot = plot.forecast,
                          ci.levels = c(0.66,0.95,0.99))

      prediction_list[prediction_list$order == i, "predict.isat_object"] <- tibble(predict.isat_object = list(as_tibble(pred_obj)))
      prediction_list[prediction_list$order == i, "data"] <- tibble(data = list(pred_df))
    } else {
      identity_pred <- tibble()
      identity_pred <- exog_df_ready %>%
        select(all_of(current_spec$ind_vars_eu[current_spec$ind_vars_eu %in% names(exog_df_ready)]))

      identity_logs <- c(rep(FALSE,length(current_spec$ind_vars_eu[current_spec$ind_vars_eu %in% names(exog_df_ready)])))

      missing_vars <- current_spec$ind_vars_eu[!current_spec$ind_vars_eu %in% names(exog_df_ready)]


      for(mvar in missing_vars){
        # mvar = "yf"

        model$module_order_eurostatvars %>%
          filter(tolower(dependent_eu) == mvar) %>%
          pull(index) -> mvar_model_index

        prediction_list %>%
          filter(index == mvar_model_index) %>%
          pull(predict.isat_object) %>%
          .[[1]] -> mvar_model_obj

        mvar_logs <- model$module_collection %>%
          filter(index == mvar_model_index) %>%
          with(model.args) %>%
          .[[1]] %>%
          .$use_logs

        identity_logs <- c(identity_logs, ifelse(mvar_logs %in% c("both","x"), TRUE, FALSE))

        mvar_euname <- model$module_collection %>%
          filter(index == mvar_model_index) %>%
          pull(dependent_eu)

        mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), tolower(mvar_euname))

        mvar_tibble <- tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
          setNames(mvar_name)

        if(ncol(identity_pred)==0){
          identity_pred <- mvar_tibble
        } else {
          identity_pred <- bind_cols(identity_pred,mvar_tibble)
        }


      }

      # log all the exogenous columns, if any of the estimated columns is logged
      if(any(identity_logs) && !all(identity_logs)){
        identity_pred %>%
          mutate(across(all_of(names(identity_pred)[!identity_logs]),
                        .fns = list(ln = log), .names = "{.fn}.{col}")) %>%
          select(-all_of(all_of(names(identity_pred)[!identity_logs]))) -> identity_pred
      }

      # sum the identities
      cols_to_cycle <-strsplits(unique(current_spec$independent_eu), c("\\+", "\\-"))
      operators <- str_extract_all(string = unique(current_spec$independent_eu), pattern = "\\+|\\-")[[1]]

      if(length(cols_to_cycle) != (length(operators)+1)){warning("Identity might be falsely calculated. Check operators.")}

      identity_pred_final <- identity_pred[,1]
      for(col_cycle in 2:ncol(identity_pred)){
        if(operators[col_cycle-1] == "+"){
          identity_pred_final <- identity_pred_final + identity_pred[,col_cycle]
        } else if(operators[col_cycle-1] == "-"){
          identity_pred_final <- identity_pred_final - identity_pred[,col_cycle]
        } else {stop("Error in calculating Identity.")}

      }
      names(identity_pred_final) <- unique(current_spec$dependent_eu)
      prediction_list[prediction_list$order == i, "predict.isat_object"] <- tibble(predict.isat_object = list(tibble(yhat = identity_pred_final[,1])))
      prediction_list[prediction_list$order == i, "data"] <- tibble(data = list(bind_cols(identity_pred_final, identity_pred)))
    }
  }



  # if(plot){
  #   plot(rnorm(10))
  # }



  return(prediction_list)

}
