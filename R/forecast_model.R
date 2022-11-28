#' Forecast Aggregate model
#'
#' @param model An aggregate model of class 'aggmod'
#' @param exog_predictions A data.frame or tibble with values for the exogenous values. The number of rows of this data must be equal to n.ahead.
#' @param n.ahead Periods to forecast ahead
#' @param plot.forecast Logical. Should the result be plotted? Default is TRUE.
#'
#' @return An object of class aggmod.forecast
#' @export
#'
#' @examples
#' spec <- tibble(
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
#' filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)
#' \dontrun{
#' a <- run_model(specification = spec, dictionary = NULL, inputdata_directory = NULL, filter_list = filter_list, download = TRUE, save_to_disk = NULL, present = FALSE)
#' forecast(a)
#' }


forecast_model <- function(model,
                           exog_predictions = NULL,
                           n.ahead = 10,
                           ci.levels = c(0.5,0.66,0.95),
                           exog_fill_method = "AR",
                           ar.fill.max = 4,
                           plot.forecast = TRUE){

  if(class(model) != "aggmod"){stop("Forecasting only possible with an aggmod object. Execute 'run_model' to get such an object.")}
  if(!is.null(exog_fill_method) & !exog_fill_method %in% c("AR","last")){stop("The method to fill exogenous values 'exog_fill_method' can only be either NULL (when data is provided), or 'AR' or 'last'.")}
  if(!is.null(ar.fill.max) & (!is.integer(as.integer(ar.fill.max)) | ar.fill.max < 1)){stop("The option 'ar.fill.max' can either be NULL or must be an integer that is larger than 0.")}

  # 1. Determine Exogenous Variables and wrangle future values ---------------

  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = model$module_order_eurostatvars)

  classification %>%
    filter(class == "x") %>%
    pull(var) -> exog_vars

  if (is.null(exog_predictions) & exog_fill_method == "last") {
    message("No exogenous values provided. Model will use the last available value.\nAlternative is exog_fill_method = 'AR'.")
    exog_df <- model$full_data %>%
      filter(na_item %in% exog_vars) %>%
      group_by(na_item) %>%
      filter(time == max(time)) #%>%
    #mutate(na_item = janitor::make_clean_names(na_item))

    if(!all(exog_df$time == exog_df$time[1])){
      warning("Latest Exogenous Data is not available for all variables. For those where most recent data is not available, the period before that is used.")
    }

    exog_df %>%
      group_by(na_item) %>%
      rowwise() %>%
      mutate(time = list(seq(time, length = n.ahead + 1, by = "3 months")[1:n.ahead + 1])) %>%
      unnest(time) %>%
      ungroup %>%
      mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
      pivot_wider(names_from = "na_item",values_from = "values", id_cols = c(time,q)) %>%

      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) -> exog_df_ready
  }

  if (is.null(exog_predictions) & exog_fill_method == "AR") {
    message(paste0("No exogenous values provided. Model will forecast the exogenous values with an AR", ar.fill.max," process (incl. Q dummies, IIS and SIS w 't.pval = 0.001').\nAlternative is exog_fill_method = 'last'."))

    exog_df_intermed <- model$full_data %>%
      filter(na_item %in% exog_vars) %>%
      group_by(na_item) %>%
      pivot_wider(id_cols = time, names_from = na_item, values_from = values)

    exog_df_forecast <- model$full_data %>%
      filter(time == max(time)) %>%
      distinct(time) %>%
      rowwise() %>%
      mutate(time = list(seq(time, length = n.ahead + 1, by = "3 months")[1:n.ahead + 1])) %>%
      unnest(time) %>%
      ungroup

    for(col_to_forecast in seq_along(exog_df_intermed)){
      # col_to_forecast <- 2

      # skip the time column
      if(col_to_forecast == 1){next}
      exog_df_intermed %>%
        mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE,remove_selected_columns = TRUE) %>%
        arrange(time) -> to_ar_predict

      to_ar_predict %>%
        pull(col_to_forecast) -> y_ar_predict

      to_ar_predict %>%
        select(q_2, q_3, q_4) -> x_ar_predict

      isat_ar_predict <- isat(y = y_ar_predict, mxreg = x_ar_predict,
                              mc = TRUE, ar = 1:4, plot = FALSE, t.pval = 0.001,
                              print.searchinfo = FALSE, sis = TRUE, iis = TRUE)


      # get iis dummies
      if(!is.null(gets::isatdates(isat_ar_predict)$iis)){
        iis_pred <- matrix(0,
                           nrow = n.ahead,
                           ncol = nrow(gets::isatdates(isat_ar_predict)$iis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_ar_predict)$iis$breaks)) %>%
          as_tibble()
      }

      # get sis dummies
      if(!is.null(gets::isatdates(isat_ar_predict)$sis)){
        sis_pred <- matrix(1,
                           nrow = n.ahead,
                           ncol = nrow(gets::isatdates(isat_ar_predict)$sis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_ar_predict)$sis$breaks)) %>%
          as_tibble()
      }

      model$full_data %>%
        filter(time == max(time)) %>%
        distinct(time) %>%
        rowwise() %>%
        mutate(time = list(seq(time, length = n.ahead + 1, by = "3 months")[1:n.ahead + 1])) %>%
        unnest(time) %>%
        ungroup %>%

        mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%

        {if (exists("iis_pred")) {bind_cols(.,iis_pred)} else {.}} %>%
        {if (exists("sis_pred")) {bind_cols(.,sis_pred)} else {.}} %>%

        select(-time) -> x_ar_predict_pred_df

      if (exists("iis_pred")) {rm(iis_pred)}
      if (exists("sis_pred")) {rm(sis_pred)}

      predict(object = isat_ar_predict, n.ahead = n.ahead, newmxreg = x_ar_predict_pred_df) %>%
        as.vector -> pred_values

      tibble(data = pred_values) %>%
        setNames(names(exog_df_intermed)[col_to_forecast]) %>%
        bind_cols(exog_df_forecast,.) -> exog_df_forecast

    }

    exog_df_forecast %>%
      mutate(q = lubridate::quarter(time, with_year = FALSE)) %>%

      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) -> exog_df_ready
  }


  # 2. Forecasting step by step according to model order ------------------------------------------------
  prediction_list <- tibble(
    index = model$module_order_eurostatvars$index,
    order = model$module_order_eurostatvars$order,
    predict.isat_object = list(NA_complex_),
    data = list(NA_complex_),
    central.estimate = list(NA_complex_) #,
    #all.estimates = list(NA_complex_)
  )

  # cycling through each module
  for(i in seq(model$module_order_eurostatvars$order)){
    # i = 1

    current_spec <- model$module_order_eurostatvars %>%
      filter(order == i) %>%

      # save original form of independent col
      mutate(independent_orig = independent) %>%

      # make sure each independent variable has a separate row
      mutate(independent = gsub(" ", "", independent)) %>%
      #independent_eu = gsub(" ", "", independent_eu),) %>%
      rowwise() %>%
      mutate(#ind_vars_eu = list(strsplits(independent_eu,c("\\-", "\\+"))),
        independent = list(strsplits(independent,c("\\-", "\\+")))) %>%

      # following line added to deal with AR models when ind_vars is a list of NULL
      #bind_rows(tibble(ind_vars_eu = list(""))) %>%
      bind_rows(tibble(independent = list(""))) %>%

      #nnest(ind_vars_eu, keep_empty = TRUE) %>%
      unnest(independent, keep_empty = TRUE) %>%
      drop_na(index) %>%
      select(index,
             #dependent_eu, independent_eu,ind_vars_eu,
             dependent,
             independent,
             independent_orig) #%>%
    #mutate(independent_eu = tolower(independent_eu),ind_vars_eu = tolower(ind_vars_eu))

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

      # check quarterly dummies to drop
      q_pred_todrop <- c("q_1","q_2","q_3","q_4")[!c("q_1","q_2","q_3","q_4") %in% colnames(isat_obj$aux$mX)]

      # check if mconst is used
      if ("mconst" %in% colnames(isat_obj$aux$mX)) {
        mconst <- TRUE
      } else {
        mconst <- FALSE
      }

      # get ar
      pred_ar_needed <- colnames(isat_obj$aux$mX)[grepl("ar[0-9]+",colnames(isat_obj$aux$mX))]

      if (!is.null(pred_ar_needed) & !identical(character(0),pred_ar_needed)) {
        ar_vec <- 0:max(as.numeric(gsub("ar","",pred_ar_needed)))
        y_names_vec <- c()
        for (ar in ar_vec) {
          # ar = 0
          y_names_vec <- c(y_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),y_vars_basename))
        }
      } else {
        y_names_vec <- paste0(ifelse(ylog,"ln.",""),y_vars_basename)
      }

      if (!identical(character(0),x_vars_basename)) {
        x_names_vec <- c()
        for (ar in ar_vec) {
          # ar = 0
          x_names_vec <- c(x_names_vec,paste0(paste0(ifelse(ar == 0,"",paste0("L",ar,"."))),ifelse(ylog,"ln.",""),x_vars_basename))
        }

        x_names_vec_nolag <- paste0(ifelse(ylog,"ln.",""),x_vars_basename)
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
          as_tibble()
      }

      # get sis dummies
      if (!is.null(gets::isatdates(isat_obj)$sis)) {
        sis_pred <- matrix(1,
                           nrow = nrow(exog_df_ready),
                           ncol = nrow(gets::isatdates(isat_obj)$sis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_obj)$sis$breaks)) %>%
          as_tibble()
      }

      if ("trend" %in% names(coef(isat_obj))) {
        trend_pred <- tibble(trend = (max(isat_obj$aux$mX[,"trend"]) + 1):(max(isat_obj$aux$mX[,"trend"]) + n.ahead))
      }


      exog_df_ready %>%

        # select the relevant variables
        select(time, any_of(c("q_1","q_2","q_3","q_4")), any_of(names(data_obj))) %>%

        # drop not used quarterly dummies
        select(-any_of(q_pred_todrop)) %>%

        {if ("trend" %in% names(coef(isat_obj))) {
          bind_cols(.,trend_pred)
        } else { . }} %>%

        {if (!is.null(gets::isatdates(isat_obj)$iis)) {
          bind_cols(.,iis_pred)
        } else { . }} %>%

        {if (!is.null(gets::isatdates(isat_obj)$sis)) {
          bind_cols(.,sis_pred)
        } else { . }} %>%

        {if (xlog) {
          mutate(.,
                 across(.cols = any_of(x_vars_basename), .fns = list(ln = log), .names = "{.fn}.{.col}"),
                 #across(starts_with("ln."), list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}"
          )
        } else {.}} -> current_pred_raw

      # Deal with current_spec not being fully exogenous
      if (!all(current_spec$independent %in% names(exog_df_ready)) && !all(is.na(current_spec$independent))) {

        missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

        for (mvar in missing_vars) {
          # mvar = "p5g"
          model$module_order_eurostatvars %>%
            filter(dependent == mvar) %>%
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
            pull(dependent)

          mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

          # TODO: Here we can implement the forecast plume
          # currently we are using 'yhat' below - but the mvar_model_obj has all values of the ci.levels
          mvar_tibble <- tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
            setNames(mvar_name)

          if (!mvar_name %in% x_names_vec_nolag) {
            if (paste0("ln.",mvar_name) %in% x_names_vec_nolag) {
              mvar_tibble %>%
                mutate(across(all_of(mvar_euname), log, .names = "ln.{.col}")) %>%
                select(all_of(paste0("ln.",mvar_euname))) -> mvar_tibble
            } else {
              stop("Error occurred in adding missing/lower estimated variables (likely identities) to a subsequent/higher model. This is likely being caused by either log specification or lag specifiction. Check code.")
            }
          }

          current_pred_raw <- bind_cols(current_pred_raw,mvar_tibble)

        }
      }

      data_obj %>%
        select(time, all_of(x_names_vec_nolag)) %>%

        ########### TODO CHHHHEEEEEEECK. Don't think this makes sense. This happens if e.g. a value for one variable is released later
        # The drop_na below was used because for GCapitalForm the value for July 2022 was missing - while it was there for FinConsExpHH
        # Now the question is whether the drop_na messes up the timing
        #drop_na %>% # UNCOMMENT THIS WHEN NOT HAVING A FULL DATASET

        bind_rows(current_pred_raw %>%
                    #select(time, all_of(x_names_vec_nolag), any_of("trend"))) -> intermed
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
                    select(time, any_of("trend"), starts_with("q_"), starts_with("iis"), starts_with("sis")), by = "time") %>%
        drop_na %>%
        select(-time) %>%
        select(any_of(row.names(isat_obj$mean.results))) %>%
        return() -> pred_df

      #print(pred_df)

      # Predict
      isat_obj$call$ar <- isat_obj$aux$args$ar
      isat_obj$call$mc <- isat_obj$aux$args$mc

      # backup <- pred_df$L1.ln.p31_s14_s15
      # pred_df$L1.ln.p31_s14_s15 <- 0
      # pred_df$test <- 10
      # pred_df$L1.ln.p31_s14_s15 <- backup

      pred_obj <- predict(isat_obj, newmxreg = as.matrix(tail(pred_df, n.ahead)),
                          n.ahead = n.ahead, plot = plot.forecast,
                          ci.levels = ci.levels)

      outvarname <- paste0(if (model$module_collection %>%
                               filter(order == i) %>%
                               with(model.args) %>%
                               .[[1]] %>%
                               .$use_logs %in% c("both","y")) {"ln."} else {""},
                           current_spec %>% pull(dependent))

      tibble(time = current_pred_raw %>% pull(time),
             value = as.numeric(pred_obj[,1])) %>%
        setNames(c("time",outvarname)) -> central_estimate

      # tibble(time = current_pred_raw %>% pull(time)) %>%
      #   bind_cols(pred_obj[,-1]) %>%
      #   setNames(c("time",paste0(outvarname,".", gsub("^y","",names(pred_obj[,-1]))))) -> all_estimates



      prediction_list[prediction_list$order == i, "predict.isat_object"] <- tibble(predict.isat_object = list(as_tibble(pred_obj)))
      prediction_list[prediction_list$order == i, "data"] <- tibble(data = list(bind_cols(intermed, to_be_added) %>%
                                                                                  left_join(current_pred_raw %>%
                                                                                              select(time, starts_with("q_"), starts_with("iis"), starts_with("sis")), by = "time") %>%
                                                                                  drop_na))
      prediction_list[prediction_list$order == i, "central.estimate"] <- tibble(central_estimate = list(central_estimate))
      #prediction_list[prediction_list$order == i, "all.estimates"] <- tibble(all_estimates = list(all_estimates))

    } else {
      # Loop goes to this part if the type of the module is not "d"
      identity_pred <- tibble()
      identity_pred <- exog_df_ready %>%
        select(all_of(current_spec$independent[current_spec$independent %in% names(exog_df_ready)]))

      identity_logs <- c(rep(FALSE,length(current_spec$independent[current_spec$independent %in% names(exog_df_ready)])))

      # check which x variables are needed in this module, but are not fully exogenous
      missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

      for(mvar in missing_vars){
        # mvar = "yf"

        model$module_order_eurostatvars %>%
          filter(dependent == mvar) %>%
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

        identity_logs <- c(identity_logs, ifelse(mvar_logs %in% c("both","x") || is.null(mvar_logs), TRUE, FALSE))

        mvar_euname <- model$module_collection %>%
          filter(index == mvar_model_index) %>%
          pull(dependent)

        mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

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
      cols_to_cycle <- gsub(" ","",strsplits(unique(current_spec$independent_orig), c("\\+", "\\-")))
      operators <- str_extract_all(string = unique(current_spec$independent_orig), pattern = "\\+|\\-")[[1]]

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

      outvarname <- paste0(#if(any(identity_logs) && !all(identity_logs)){"ln."} else {""},
        current_spec %>% pull(dependent) %>% unique) #%>% tolower)

      tibble(time = current_pred_raw %>% pull(time),
             value = as.numeric(identity_pred_final[,1])) %>%
        setNames(c("time",outvarname)) -> central_estimate


      names(identity_pred_final) <- unique(current_spec$dependent)
      prediction_list[prediction_list$order == i, "predict.isat_object"] <- tibble(predict.isat_object = list(tibble(yhat = identity_pred_final[,1])))
      prediction_list[prediction_list$order == i, "data"] <- tibble(data = list(bind_cols(identity_pred_final, identity_pred)))
      prediction_list[prediction_list$order == i, "central.estimate"] <- tibble(data = list(central_estimate))
    }
  }

  out <- list()
  out$forecast <- prediction_list
  out$orig_model <- model
  out$dictionary <- model$dictionary

  class(out) <- "aggmod.forecast"

  if(plot.forecast){
    plot.aggmod.forecast(out)
  }

  return(out)

}
