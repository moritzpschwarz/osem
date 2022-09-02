#' Forecast Aggregate model
#'
#' @param model An aggregate model of class 'aggmod'
#' @param exog_predictions A data.frame or tibble with values for the exogenous values. The number of rows of this data must be equal to n.ahead.
#' @param n.ahead Periods to forecast ahead
#' @param plot Logical. Should the result be plotted? Default is TRUE.
#'
#' @return
#' @export
#'
#' @examples
forecast_model <- function(model, exog_predictions = NULL, n.ahead = 10, plot = TRUE){

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

  for(i in seq(model$module_order_eurostatvars$order)){
    # i = 1

    current_spec <- model$module_order_eurostatvars %>%
      filter(order == i) %>%
      mutate(independent = gsub(" ", "", independent)) %>%
      rowwise() %>%
      mutate(ind_vars = list(strsplits(independent,c("\\-", "\\+")))) %>%
      # following line added to deal with AR models wehn ind_vars is a list of NULL
      bind_rows(tibble(ind_vars = list(""))) %>%
      unnest(ind_vars, keep_empty = TRUE) %>%
      drop_na(index) %>%
      select(index, dependent_eu, independent_eu)

    # Build the prediction df

    # Case 1: current_spec is fully exogenous
    if(all(current_spec$independent_eu %in% names(exog_df_ready))){

      # set up
      # get isat obj
      model$module_collection %>%
        filter(order == i) %>%
        pull(model) %>% .[[1]] -> isat_obj

      # get data obj
      model$module_collection %>%
        filter(order == i) %>%
        pull(dataset) %>% .[[1]] -> data_obj

      isat_obj$aux$mX %>% nrow
      data_obj %>% nrow
      colnames(isat_obj$aux$mX)
      names(data_obj)

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
      q_pred <- c("q_1","q_2","q_3","q_4")[!c("q_1","q_2","q_3","q_4") %in% colnames(isat_obj$aux$mX)]

      # check if mconst is used
      if("mconst" %in% colnames(isat_obj$aux$mX)){
        mconst <- TRUE
      } else {
        mconst <- FALSE
      }

      # get ar
      pred_ar_needed <- colnames(isat_obj$aux$mX)[grepl("ar[0-9]+",colnames(isat_obj$aux$mX))]

      # if(!identical(pred_ar_needed, character(0))){
      #   pred_ar <- matrix(NA,
      #          nrow = nrow(exog_df_ready),
      #          ncol = length(pred_ar_needed),
      #          dimnames  = list(NULL,
      #                           pred_ar_needed)) %>%
      #     as_tibble() %>%
      #     mutate(across(everything(), as.numeric))
      # } else {
      #   pred_ar <- NULL
      # }

      ar_vec <- 0:as.numeric(gsub("ar","",pred_ar_needed))
      y_names_vec <- c()
      for(ar in ar_vec){
        # ar = 0
        y_names_vec <- c(y_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),y_vars_basename))
      }

      x_names_vec <- c()
      for(ar in ar_vec){
        # ar = 0
        x_names_vec <- c(x_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),x_vars_basename))
      }

      x_names_vec_nolag <- paste0(ifelse(ylog,"ln.",""),x_vars_basename)


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
        select(-any_of(q_pred)) %>%

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
                 across(.cols = all_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
                 #across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}"
          )
        } else {.}} -> current_pred_raw


      data_obj %>%
        select(time, all_of(x_names_vec_nolag)) %>%
        bind_rows(current_pred_raw %>%
                    select(time, all_of(x_names_vec_nolag))) -> intermed

      to_be_added <- tibble(.rows = nrow(intermed))
      for (i in 1:max(ar_vec)) {
        intermed %>%
          mutate(across(c(#starts_with("D."),
            starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
          select(c(#starts_with("D."),
            starts_with("ln."))) %>%
          rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
          bind_cols(to_be_added, .) -> to_be_added
      }

      bind_cols(intermed, to_be_added) %>%
        left_join(current_pred_raw %>%
                    select(time, starts_with("q_"), starts_with("iis"), starts_with("sis")), by = "time") %>%
        drop_na %>%
        select(-time) -> pred_df

      predict(isat_obj, newmxreg = pred_df, n.ahead = n_ahead, plot = plot)



      #
      #
      # data_obj %>%
      #   bind_rows(current_pred_raw) %>%
      #   mutate(
      #     across(.cols = all_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
      #     across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
      #   ) -> intermed
      #
      # to_be_added <- tibble(.rows = nrow(intermed))
      # for (i in 1:max(ar_vec)) {
      #   intermed %>%
      #     mutate(across(c(starts_with("D."), starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
      #     select(c(starts_with("D."), starts_with("ln."))) %>%
      #     rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
      #     bind_cols(to_be_added, .) -> to_be_added
      # }
      #
      #
      #
      # # make sure the x vars are in the right transformation
      # current_pred_raw %>%
      #   select(all_of(x_vars_basename)) %>%
      #   mutate(
      #     across(.fns = list(ln = log), .names = "{.fn}.{.col}"),
      #     across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
      #   ) -> intermed
      #
      # to_be_added <- tibble(.rows = nrow(intermed))
      # for (i in 1:max(ar_vec)) {
      #   intermed %>%
      #     mutate(across(c(starts_with("D."), starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
      #     select(c(starts_with("D."), starts_with("ln."))) %>%
      #     rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
      #     bind_cols(to_be_added, .) -> to_be_added
      # }
      #
      #
      #
      #
      #
      # for(pred_time in 1:n_ahead){
      #
      #   if(pred_time == 1){
      #     pred_ar_needed
      #   } else {
      #
      #
      #   }
      #
      #
      # }
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      #
      # current_pred_raw %>%
      #   select(all_of(x_vars_basename)) %>%
      #   mutate(
      #     across(.fns = list(ln = log), .names = "{.fn}.{.col}"),
      #     across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
      #   ) -> intermed
      #
      # to_be_added <- tibble(.rows = nrow(intermed))
      # for (i in 1:max(ar_vec)) {
      #   intermed %>%
      #     mutate(across(c(starts_with("D."), starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
      #     select(c(starts_with("D."), starts_with("ln."))) %>%
      #     rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
      #     bind_cols(to_be_added, .) -> to_be_added
      # }
      #
      #
      # current_pred_raw %>%
      #   select(all_of(y_vars_basename)) %>%
      #   mutate(
      #     across(.fns = list(ln = log), .names = "{.fn}.{.col}"),
      #     across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
      #   ) -> intermed
      #
      # to_be_added <- tibble(.rows = nrow(intermed))
      # for (i in 1:max(ar_vec)) {
      #   intermed %>%
      #     mutate(across(c(starts_with("D."), starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
      #     select(c(starts_with("D."), starts_with("ln."))) %>%
      #     rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
      #     bind_cols(to_be_added, .) -> to_be_added
      # }
      #
      #
      #
      # intermed %>%
      #   bind_cols(to_be_added) %>%
      #   mutate(type = "pred")
      #
      #
      # data_obj %>%
      #   select(time, all_of(y_names_vec), all_of(x_names_vec)) %>%
      #   bind_rows()
      #
      #
      # data_obj %>%
      #   mutate(type = "hist") %>%
      #   bind_rows(.,) %>% View
      #
      #
      # current_pred_raw %>%
      #   mutate(type = "pred") %>%
      #   bind_rows(isat_obj$aux$mX %>%
      #               as_tibble %>%
      #               select(any_of(names(current_pred_raw))) %>%
      #               mutate(type = "hist"),.) %>% View
      #
      #
      # model$module_collection$dataset[[i]]
      #
      # predict(isat_obj, newmxreg = as.matrix(pred_df), n.ahead = 4)
      #
      # # transform variables in the right form
      #

      #
      #
      #       exog_df_ready %>%
      #         select(-time) %>%
      #         #select(all_of())
      #         as.matrix
      #
      #       predict(isat_obj, newmxreg = , n.ahead = 4)



    }

    # Case 2: current_spec is not fully exogenous


  }




}
