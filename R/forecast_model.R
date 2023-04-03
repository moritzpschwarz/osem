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
                           plot.forecast = TRUE){

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

  if (is.null(exog_predictions) & exog_fill_method == "last") {
    message("No exogenous values provided. Model will use the last available value.\nAlternative is exog_fill_method = 'AR'.")
    exog_df <- model$full_data %>%
      dplyr::filter(.data$na_item %in% exog_vars) %>%
      dplyr::group_by(.data$na_item) %>%
      dplyr::filter(.data$time == max(.data$time)) #%>%
    #dplyr::mutate(na_item = janitor::make_clean_names(na_item))

    if(!all(exog_df$time == exog_df$time[1])){
      warning("Latest Exogenous Data is not available for all variables. For those where most recent data is not available, the period before that is used.")
    }

    exog_df %>%
      dplyr::group_by(.data$na_item) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = "3 months")[1:n.ahead + 1])) %>%
      tidyr::unnest("time") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
      tidyr::pivot_wider(names_from = "na_item",values_from = "values", id_cols = c("time","q")) %>%

      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) -> exog_df_ready
  }

  if (is.null(exog_predictions) & exog_fill_method == "AR") {
    message(paste0("No exogenous values provided. Model will forecast the exogenous values with an AR", ar.fill.max," process (incl. Q dummies, IIS and SIS w 't.pval = 0.001').\nAlternative is exog_fill_method = 'last'."))

    exog_df_intermed <- model$full_data %>%
      dplyr::filter(.data$na_item %in% exog_vars) %>%
      dplyr::group_by(.data$na_item) %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values")

    exog_df_forecast <- model$full_data %>%
      dplyr::filter(.data$time == max(.data$time)) %>%
      dplyr::distinct(dplyr::across("time")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = "3 months")[1:n.ahead + 1])) %>%
      tidyr::unnest("time") %>%
      dplyr::ungroup()

    for(col_to_forecast in seq_along(exog_df_intermed)){
      # col_to_forecast <- 2

      # skip the time column
      if(col_to_forecast == 1){next}
      exog_df_intermed %>%
        dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE,remove_selected_columns = TRUE) %>%
        dplyr::arrange(.data$time) -> to_ar_predict

      to_ar_predict %>%
        dplyr::pull(col_to_forecast) -> y_ar_predict

      to_ar_predict %>%
        dplyr::select("q_2", "q_3", "q_4") -> x_ar_predict

      isat_ar_predict <- gets::isat(y = y_ar_predict, mxreg = x_ar_predict,
                                    mc = TRUE, ar = 1:4, plot = FALSE, t.pval = 0.001,
                                    print.searchinfo = FALSE, sis = TRUE, iis = TRUE)


      # get iis dummies
      if(!is.null(gets::isatdates(isat_ar_predict)$iis)){
        iis_pred <- matrix(0,
                           nrow = n.ahead,
                           ncol = nrow(gets::isatdates(isat_ar_predict)$iis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_ar_predict)$iis$breaks)) %>%
          dplyr::as_tibble()
      }

      # get sis dummies
      if(!is.null(gets::isatdates(isat_ar_predict)$sis)){
        sis_pred <- matrix(1,
                           nrow = n.ahead,
                           ncol = nrow(gets::isatdates(isat_ar_predict)$sis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_ar_predict)$sis$breaks)) %>%
          dplyr::as_tibble()
      }

      model$full_data %>%
        dplyr::filter(.data$time == max(.data$time)) %>%
        dplyr::distinct(dplyr::across("time")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = "3 months")[1:n.ahead + 1])) %>%
        tidyr::unnest("time") %>%
        dplyr::ungroup() %>%

        dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%

        {if (exists("iis_pred")) {dplyr::bind_cols(.,iis_pred)} else {.}} %>%
        {if (exists("sis_pred")) {dplyr::bind_cols(.,sis_pred)} else {.}} %>%

        dplyr::select(-"time") -> x_ar_predict_pred_df

      if (exists("iis_pred")) {rm(iis_pred)}
      if (exists("sis_pred")) {rm(sis_pred)}

      gets::predict.isat(object = isat_ar_predict, n.ahead = n.ahead, newmxreg = x_ar_predict_pred_df) %>%
        as.vector -> pred_values

      dplyr::tibble(data = pred_values) %>%
        setNames(names(exog_df_intermed)[col_to_forecast]) %>%
        dplyr::bind_cols(exog_df_forecast,.) -> exog_df_forecast

    }

    exog_df_forecast %>%
      dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%

      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) -> exog_df_ready
  }


  # 2. Forecasting step by step according to model order ------------------------------------------------
  prediction_list <- dplyr::tibble(
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
      dplyr::filter(.data$order == i) %>%

      # save original form of independent col
      dplyr::mutate(independent_orig = .data$independent) %>%

      # make sure each independent variable has a separate row
      dplyr::mutate(independent = gsub(" ", "", .data$independent)) %>%
      #independent_eu = gsub(" ", "", independent_eu),) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(#ind_vars_eu = list(strsplits(independent_eu,c("\\-", "\\+"))),
        independent = list(strsplits(.data$independent,c("\\-", "\\+")))) %>%

      # following line added to deal with AR models when ind_vars is a list of NULL
      #dplyr::bind_rows(dplyr::tibble(ind_vars_eu = list(""))) %>%
      dplyr::bind_rows(dplyr::tibble(independent = list(""))) %>%

      #nnest(ind_vars_eu, keep_empty = TRUE) %>%
      tidyr::unnest("independent", keep_empty = TRUE) %>%
      tidyr::drop_na("index") %>%
      dplyr::select("index",
                    #dependent_eu, independent_eu,ind_vars_eu,
                    "dependent",
                    "independent",
                    "independent_orig") #%>%
    #dplyr::mutate(independent_eu = tolower(independent_eu),ind_vars_eu = tolower(ind_vars_eu))

    if(model$module_order_eurostatvars$type[model$module_order_eurostatvars$order == i] != "d"){
      # set up
      # get isat obj
      model$module_collection %>%
        dplyr::filter(.data$order == i) %>%
        dplyr::pull(.data$model) %>% .[[1]] -> isat_obj

      # get data obj
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

      # Deal with current_spec not being fully exogenous
      if (!all(current_spec$independent %in% names(exog_df_ready)) && !all(is.na(current_spec$independent))) {

        missing_vars <- current_spec$independent[!current_spec$independent %in% names(exog_df_ready)]

        for (mvar in missing_vars) {
          # mvar = "p5g"
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

          mvar_euname <- model$module_collection %>%
            dplyr::filter(.data$index == mvar_model_index) %>%
            dplyr::pull("dependent")

          mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

          # TODO: Here we can implement the forecast plume
          # currently we are using 'yhat' below - but the mvar_model_obj has all values of the ci.levels
          mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
            setNames(mvar_name)

          if (!mvar_name %in% x_names_vec_nolag) {
            if (paste0("ln.",mvar_name) %in% x_names_vec_nolag) {
              mvar_tibble %>%
                dplyr::mutate(dplyr::across(dplyr::all_of(mvar_euname), log, .names = "ln.{.col}")) %>%
                dplyr::select(dplyr::all_of(paste0("ln.",mvar_euname))) -> mvar_tibble
            } else {
              stop("Error occurred in adding missing/lower estimated variables (likely identities) to a subsequent/higher model. This is likely being caused by either log specification or lag specifiction. Check code.")
            }
          }

          current_pred_raw <- dplyr::bind_cols(current_pred_raw,mvar_tibble)

        }
      }

      data_obj %>%
        dplyr::select("time", dplyr::all_of(x_names_vec_nolag)) %>%

        ########### TODO CHHHHEEEEEEECK. Don't think this makes sense. This happens if e.g. a value for one variable is released later
        # The drop_na below was used because for GCapitalForm the value for July 2022 was missing - while it was there for FinConsExpHH
        # Now the question is whether the drop_na messes up the timing
        tidyr::drop_na() %>% # UNCOMMENT THIS WHEN NOT HAVING A FULL DATASET

        dplyr::bind_rows(current_pred_raw %>%
                           #dplyr::select(time, dplyr::all_of(x_names_vec_nolag), dplyr::any_of("trend"))) -> intermed
                           dplyr::select("time", dplyr::all_of(x_names_vec_nolag))) -> intermed

      # add the lagged x-variables
      if(ncol(intermed) > 1){
        to_be_added <- dplyr::tibble(.rows = nrow(intermed))
        for (j in ar_vec) {
          if(j == 0){next}
          intermed %>%
            dplyr::mutate(dplyr::across(-time, ~dplyr::lag(., n = j))) %>%
            dplyr::starts_with("ln.")), ~ dplyr::lag(., n = j))) %>%
            dplyr::select(-time) -> inter_intermed

          inter_intermed %>%
            setNames(paste0("L", j, ".", names(inter_intermed))) %>%
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

      # Predict
      isat_obj$call$ar <- isat_obj$aux$args$ar
      isat_obj$call$mc <- isat_obj$aux$args$mc

      # backup <- pred_df$L1.ln.p31_s14_s15
      # pred_df$L1.ln.p31_s14_s15 <- 0
      # pred_df$test <- 10
      # pred_df$L1.ln.p31_s14_s15 <- backup

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
                                             ci.levels = ci.levels)

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
      # } else {
      #   pred_runs_final <- NULL
      # }


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



      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::as_tibble(pred_obj)))
      prediction_list[prediction_list$order == i, "data"] <- dplyr::tibble(
        data = list(intermed %>%
                      dplyr::left_join(current_pred_raw %>% dplyr::select("time", dplyr::starts_with("q_"),
                                                                          dplyr::starts_with("iis"),
                                                                          dplyr::starts_with("sis")),
                                       by = "time") %>%
                      tidyr::drop_na()))
      prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(central_estimate = list(central_estimate))
      #prediction_list[prediction_list$order == i, "all.estimates"] <- dplyr::tibble(all_estimates = list(all_estimates))


    } else {
      # Loop goes to this part if the type of the module is not "d"
      identity_pred <- dplyr::tibble()
      identity_pred <- exog_df_ready %>%
        dplyr::select(dplyr::all_of(current_spec$independent[current_spec$independent %in% names(exog_df_ready)]))

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

        identity_logs <- c(identity_logs, ifelse(mvar_logs %in% c("both","x") || is.null(mvar_logs), TRUE, FALSE))

        mvar_euname <- model$module_collection %>%
          dplyr::filter(.data$index == mvar_model_index) %>%
          dplyr::pull("dependent")

        mvar_name <- paste0(ifelse(mvar_logs %in% c("both","x"), "ln.",""), mvar_euname)

        mvar_tibble <- dplyr::tibble(data = as.numeric(mvar_model_obj$yhat)) %>%
          setNames(mvar_name)

        if(ncol(identity_pred)==0){
          identity_pred <- mvar_tibble
        } else {
          identity_pred <- dplyr::bind_cols(identity_pred,mvar_tibble)
        }


      }

      # log all the exogenous columns, if any of the estimated columns is logged
      if(any(identity_logs) && !all(identity_logs)){
        identity_pred %>%
          dplyr::mutate(dplyr::across(dplyr::all_of(names(identity_pred)[!identity_logs]),
                                      .fns = list(ln = log), .names = "{.fn}.{col}")) %>%
          dplyr::select(-dplyr::all_of(dplyr::all_of(names(identity_pred)[!identity_logs]))) -> identity_pred
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

      outvarname <- paste0(#if(any(identity_logs) && !all(identity_logs)){"ln."} else {""},
        current_spec %>% dplyr::pull("dependent") %>% unique) #%>% tolower)

      dplyr::tibble(time = current_pred_raw %>% dplyr::pull(.data$time),
                    value = as.numeric(identity_pred_final[,1])) %>%
        setNames(c("time",outvarname)) -> central_estimate


      names(identity_pred_final) <- unique(current_spec$dependent)
      prediction_list[prediction_list$order == i, "predict.isat_object"] <- dplyr::tibble(predict.isat_object = list(dplyr::tibble(yhat = identity_pred_final[,1])))
      prediction_list[prediction_list$order == i, "data"] <- dplyr::tibble(data = list(dplyr::bind_cols(identity_pred_final, identity_pred)))
      prediction_list[prediction_list$order == i, "central.estimate"] <- dplyr::tibble(data = list(central_estimate))
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
