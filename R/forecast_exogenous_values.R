#' Internal function to forecast exogenous variables
#'
#' @param exog_vars The set of exogenous variables to be forecasted.
#' @inheritParams forecast_model
#'
#' @return A dataset containing the set of forecasted exogenous values.

forecast_exogenous_values <- function(model, exog_vars, exog_predictions, exog_fill_method, ar.fill.max, n.ahead, quiet){

  frequency <- model$full_data %>%
    dplyr::arrange(.data$na_item, .data$time) %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::distinct(.data$time) %>%
    dplyr::mutate(diff_num = c(NA,diff(.data$time)),
                  diff = dplyr::case_when(.data$diff_num == 1 ~ "day",
                                          .data$diff_num %in% c(28:31) ~ "month",
                                          .data$diff_num %in% c(90:92) ~ "3 months",
                                          .data$diff_num %in% c(365:366) ~ "year")) %>%
    dplyr::ungroup() %>%
    tidyr::drop_na("diff") %>%
    dplyr::distinct(.data$diff) %>%
    dplyr::pull(.data$diff)

  if(length(frequency) > 1 | frequency == "month" | frequency == "day"){stop("Mixed frequency forecasts or forecasts with daily or monthly data are not yet implemented.")}

  if (is.null(exog_predictions) & exog_fill_method == "last") {
    if(!quiet){
      message("No exogenous values provided. Model will use the last available value.\nAlternative is exog_fill_method = 'AR'.")
    }
    exog_df <- model$full_data %>%
      dplyr::filter(.data$na_item %in% exog_vars) %>%
      tidyr::drop_na("values") %>%
      dplyr::mutate(last_time = max(.data$time),.by = "na_item") %>%
      dplyr::filter(.data$time == .data$last_time) %>%
      dplyr::select(-"last_time")

    if(!all(exog_df$time == exog_df$time[1]) & !quiet){
      message("Latest Exogenous Data is not available for all variables. For those where most recent data is not available, the period before that is used.")
    }

    max.ahead <- max(seq(max(exog_df$time), length = n.ahead + 1, by = frequency))

    exog_df %>%
      dplyr::group_by(.data$na_item) %>%
      dplyr::rowwise() %>%
      #dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = frequency)[1:n.ahead + 1])) %>%
      dplyr::mutate(time = list(seq(from = .data$time, to = max.ahead, by = frequency)[-1])) %>%
      tidyr::unnest("time") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
      tidyr::pivot_wider(names_from = "na_item",values_from = "values", id_cols = c("time","q")) -> exog_df_q

    if(!all(c(1,2,3,4) %in% unique(exog_df_q$q))){
      dplyr::tibble(q = c(1,2,3,4)[!c(1,2,3,4) %in% unique(exog_df_q$q)]) %>%
        dplyr::bind_rows(exog_df_q,.) -> exog_df_q
    }

    exog_df_q %>%

      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE)  %>%
      tidyr::drop_na("time") -> exog_df_ready


  }

  if (is.null(exog_predictions) & exog_fill_method == "AR") {
    if(!quiet){
      message(paste0("No exogenous values provided. Model will forecast the exogenous values with an AR", ar.fill.max," process (incl. Q dummies, IIS and SIS w 't.pval = 0.001').\nAlternative is exog_fill_method = 'last'."))
    }

    exog_df_intermed <- model$full_data %>%
      dplyr::filter(.data$na_item %in% exog_vars) %>%
      dplyr::group_by(.data$na_item) %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values")

    #max.ahead <- max(seq(max(exog_df_intermed$time), length = n.ahead + 1, by = frequency))

    exog_df_forecast <- model$full_data %>%
      tidyr::drop_na("values") %>%
      dplyr::filter(.data$time == max(.data$time)) %>%
      dplyr::distinct(dplyr::across("time")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = frequency)[1:n.ahead + 1])) %>%
      tidyr::unnest("time") %>%
      dplyr::ungroup()

    for(col_to_forecast in seq_along(exog_df_intermed)){
      # col_to_forecast <- 2

      # skip the time column
      if(col_to_forecast == 1){next}

      # let's first figure out what time frame we need to predict:
      model$full_data %>%
        dplyr::filter(.data$na_item == names(exog_df_intermed)[col_to_forecast]) %>%
        tidyr::drop_na() %>%
        dplyr::filter(.data$time == max(.data$time)) %>%
        dplyr::pull("time") -> col_to_forecast_max_time

      model$full_data %>%
        tidyr::drop_na() %>%
        dplyr::summarise(time = max(.data$time)) %>%
        dplyr::pull("time") -> overall_max_time

      diff_time_to_max <- length(seq.Date(from = col_to_forecast_max_time, to = overall_max_time, by = frequency)[-1])
      time_to_forecast <- seq.Date(col_to_forecast_max_time, length.out = n.ahead + (1 + diff_time_to_max),
                                   by = frequency)[-1]


      # now let's extract the data
      exog_df_intermed %>%
        dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE,remove_selected_columns = TRUE) %>%
        dplyr::arrange(.data$time) -> to_ar_predict

      to_ar_predict %>%
        dplyr::pull(col_to_forecast) -> y_ar_predict

      to_ar_predict %>%
        dplyr::select(dplyr::any_of(c("q_2", "q_3", "q_4"))) -> x_ar_predict

      isat_ar_predict <- tryCatch(gets::isat(y = y_ar_predict,
                                             mxreg = if (ncol(x_ar_predict) == 0) {NULL} else{as.matrix(x_ar_predict)},

                                             mc = TRUE, ar = 1:4, plot = FALSE, t.pval = 0.001,
                                             print.searchinfo = FALSE, sis = TRUE, iis = TRUE),
                                  error = function(abcd){
                                    message(paste0("Exogneous forecasted values for ", names(exog_df_intermed)[col_to_forecast]," will only use SIS, not IIS as too many indicators retained.\n"))
                                    gets::isat(y = y_ar_predict,
                                               mxreg = if (ncol(x_ar_predict) == 0) {NULL} else{as.matrix(x_ar_predict)},
                                               mc = TRUE, ar = 1:4, plot = FALSE, t.pval = 0.001,
                                               print.searchinfo = FALSE, sis = TRUE, iis = FALSE)
                                  })

      # get iis dummies
      if(!is.null(gets::isatdates(isat_ar_predict)$iis)){
        iis_pred <- matrix(0,
                           nrow = n.ahead + diff_time_to_max,
                           ncol = nrow(gets::isatdates(isat_ar_predict)$iis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_ar_predict)$iis$breaks)) %>%
          dplyr::as_tibble()
      }

      # get sis dummies
      if(!is.null(gets::isatdates(isat_ar_predict)$sis)){
        sis_pred <- matrix(1,
                           nrow = n.ahead + diff_time_to_max,
                           ncol = nrow(gets::isatdates(isat_ar_predict)$sis),
                           dimnames  = list(NULL,
                                            gets::isatdates(isat_ar_predict)$sis$breaks)) %>%
          dplyr::as_tibble()
      }


      dplyr::tibble(time = time_to_forecast) %>%
        dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%

        {if (exists("iis_pred")) {dplyr::bind_cols(.,iis_pred)} else {.}} %>%
        {if (exists("sis_pred")) {dplyr::bind_cols(.,sis_pred)} else {.}} %>%

        dplyr::select(-"time") -> x_ar_predict_pred_df

      if(!all(c("q_2", "q_3", "q_4") %in% colnames(x_ar_predict_pred_df))){

        dplyr::tibble(time = time_to_forecast) %>%
          dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
          fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = FALSE, remove_selected_columns = TRUE) %>%# here is the difference

          {if (exists("iis_pred")) {dplyr::bind_cols(.,iis_pred)} else {.}} %>%
          {if (exists("sis_pred")) {dplyr::bind_cols(.,sis_pred)} else {.}} %>%

          dplyr::select(-"time") -> new_cols_to_add


        if(!all(c("q_2", "q_3", "q_4") %in% colnames(new_cols_to_add))){
          new_col_name <- c("q_2", "q_3", "q_4")[!c("q_2", "q_3", "q_4") %in% colnames(new_cols_to_add)]
          matrix(0, nrow = nrow(new_cols_to_add), ncol = length(new_col_name), dimnames = list(NULL,new_col_name)) %>%
            dplyr::bind_cols(new_cols_to_add) -> new_cols_to_add
        }

        new_cols_to_add[,!colnames(new_cols_to_add) %in% colnames(x_ar_predict_pred_df), drop = FALSE] %>%
          dplyr::bind_cols(., x_ar_predict_pred_df) -> x_ar_predict_pred_df
      }

      if (exists("iis_pred")) {rm(iis_pred)}
      if (exists("sis_pred")) {rm(sis_pred)}


      gets::predict.isat(object = isat_ar_predict, n.ahead = n.ahead + diff_time_to_max,
                         newmxreg = x_ar_predict_pred_df %>% dplyr::select(dplyr::any_of(isat_ar_predict$aux$mXnames)) %>% as.matrix) %>%
        as.vector -> pred_values

      dplyr::tibble(time = time_to_forecast,
                    data = pred_values) %>%
        setNames(c("time",names(exog_df_intermed)[col_to_forecast])) %>%
        dplyr::full_join(exog_df_forecast, by = "time") -> exog_df_forecast
    }

    exog_df_forecast %>%
      dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) -> exog_df_forecast_q

    if(!all(c(1,2,3,4) %in% unique(exog_df_forecast_q$q))){
      dplyr::tibble(q = c(1,2,3,4)[!c(1,2,3,4) %in% unique(exog_df_forecast_q$q)]) %>%
        dplyr::bind_rows(exog_df_forecast_q,.) -> exog_df_forecast_q
    }

    exog_df_forecast_q %>%
      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) %>%
      tidyr::drop_na("time") %>%
      dplyr::arrange(.data$time) -> exog_df_ready
  }

  if (is.null(exog_predictions) & exog_fill_method == "auto") {
    if(!quiet){
      message(paste0("No exogenous values provided. Model will forecast the exogenous values with the auto.arima()."))
    }

    exog_df_intermed <- model$full_data %>%
      dplyr::filter(.data$na_item %in% exog_vars) %>%
      dplyr::group_by(.data$na_item) %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values")

    exog_df_forecast <- model$full_data %>%
      tidyr::drop_na("values") %>%
      dplyr::filter(.data$time == max(.data$time)) %>%
      dplyr::distinct(dplyr::across("time")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = frequency)[1:n.ahead + 1])) %>%
      tidyr::unnest("time") %>%
      dplyr::ungroup()

    for(col_to_forecast in seq_along(exog_df_intermed)){
      # col_to_forecast <- 2

      # skip the time column
      if(col_to_forecast == 1){next}

      # let's first figure out what time frame we need to predict:
      model$full_data %>%
        dplyr::filter(.data$na_item == names(exog_df_intermed)[col_to_forecast]) %>%
        tidyr::drop_na() %>%
        dplyr::filter(.data$time == max(.data$time)) %>%
        dplyr::pull("time") -> col_to_forecast_max_time

      model$full_data %>%
        tidyr::drop_na() %>%
        dplyr::summarise(time = max(.data$time)) %>%
        dplyr::pull("time") -> overall_max_time

      diff_time_to_max <- length(seq.Date(from = col_to_forecast_max_time, to = overall_max_time, by = frequency)[-1])
      time_to_forecast <- seq.Date(col_to_forecast_max_time, length.out = n.ahead + (1 + diff_time_to_max),
                                   by = frequency)[-1]

      # now let's extract the data
      exog_df_intermed %>%
        dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE,remove_selected_columns = TRUE) %>%
        dplyr::arrange(.data$time) -> to_ar_predict

      to_ar_predict %>%
        dplyr::select("time", dplyr::all_of(col_to_forecast)) %>%
        tidyr::drop_na(2) %>%
        dplyr::pull(2) -> y_ar_predict

      arima_model <- forecast::auto.arima(y_ar_predict)
      pred_values <- forecast::forecast(arima_model, h = length(time_to_forecast))

      dplyr::tibble(time = time_to_forecast,
                    data = pred_values$mean) %>%
        setNames(c("time",names(exog_df_intermed)[col_to_forecast])) %>%
        dplyr::full_join(exog_df_forecast, by = "time") -> exog_df_forecast
    }

    exog_df_forecast %>%
      dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) -> exog_df_forecast_q

    if(!all(c(1,2,3,4) %in% unique(exog_df_forecast_q$q))){
      dplyr::tibble(q = c(1,2,3,4)[!c(1,2,3,4) %in% unique(exog_df_forecast_q$q)]) %>%
        dplyr::bind_rows(exog_df_forecast_q,.) -> exog_df_forecast_q
    }

    exog_df_forecast_q %>%
      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) %>%
      tidyr::drop_na("time") %>%
      dplyr::arrange(.data$time) -> exog_df_ready
  }


  if (is.null(exog_predictions) & exog_fill_method == "ets") {
    if(!quiet){
      message(paste0("No exogenous values provided. Model will forecast the exogenous values with ets()."))
    }

    exog_df_intermed <- model$full_data %>%
      dplyr::filter(.data$na_item %in% exog_vars) %>%
      dplyr::group_by(.data$na_item) %>%
      tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values")

    exog_df_forecast <- model$full_data %>%
      tidyr::drop_na("values") %>%
      dplyr::filter(.data$time == max(.data$time)) %>%
      dplyr::distinct(dplyr::across("time")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(time = list(seq(.data$time, length = n.ahead + 1, by = frequency)[1:n.ahead + 1])) %>%
      tidyr::unnest("time") %>%
      dplyr::ungroup()

    for(col_to_forecast in seq_along(exog_df_intermed)){
      # col_to_forecast <- 2

      # skip the time column
      if(col_to_forecast == 1){next}

      # let's first figure out what time frame we need to predict:
      model$full_data %>%
        dplyr::filter(.data$na_item == names(exog_df_intermed)[col_to_forecast]) %>%
        tidyr::drop_na() %>%
        dplyr::filter(.data$time == max(.data$time)) %>%
        dplyr::pull("time") -> col_to_forecast_max_time

      model$full_data %>%
        tidyr::drop_na() %>%
        dplyr::summarise(time = max(.data$time)) %>%
        dplyr::pull("time") -> overall_max_time

      diff_time_to_max <- length(seq.Date(from = col_to_forecast_max_time, to = overall_max_time, by = frequency)[-1])
      time_to_forecast <- seq.Date(col_to_forecast_max_time, length.out = n.ahead + (1 + diff_time_to_max),
                                   by = frequency)[-1]

      # now let's extract the data
      exog_df_intermed %>%
        dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
        fastDummies::dummy_cols(select_columns = "q", remove_first_dummy = TRUE,remove_selected_columns = TRUE) %>%
        dplyr::arrange(.data$time) -> to_ar_predict

      to_ar_predict %>%
        dplyr::select("time", dplyr::all_of(col_to_forecast)) %>%
        tidyr::drop_na(2) %>%
        dplyr::pull(2) -> y_ar_predict

      ets_model <- forecast::ets(y_ar_predict, model = "ZZZ")
      pred_values <- forecast::forecast(ets_model, h = length(time_to_forecast))

      dplyr::tibble(time = time_to_forecast,
                    data = pred_values$mean) %>%
        setNames(c("time",names(exog_df_intermed)[col_to_forecast])) %>%
        dplyr::full_join(exog_df_forecast, by = "time") -> exog_df_forecast
    }

    exog_df_forecast %>%
      dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) -> exog_df_forecast_q

    if(!all(c(1,2,3,4) %in% unique(exog_df_forecast_q$q))){
      dplyr::tibble(q = c(1,2,3,4)[!c(1,2,3,4) %in% unique(exog_df_forecast_q$q)]) %>%
        dplyr::bind_rows(exog_df_forecast_q,.) -> exog_df_forecast_q
    }

    exog_df_forecast_q %>%
      fastDummies::dummy_cols(
        select_columns = "q", remove_first_dummy = FALSE,
        remove_selected_columns = TRUE) %>%
      tidyr::drop_na("time") %>%
      dplyr::arrange(.data$time) -> exog_df_ready
  }



  if(!is.null(exog_predictions)){
    exog_df_ready <- exog_predictions
  }

  out <- list()
  out$exog_df_ready <- exog_df_ready
  out$frequency <- frequency

  return(out)

}
