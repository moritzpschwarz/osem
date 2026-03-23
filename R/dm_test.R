#' Diebold-Mariano test for osem forecast comparison
#'
#' This function performs the Diebold-Mariano test to compare the forecast accuracy of the OSEM model against specified comparison methods (e.g., VAR, BVAR) for insample forecasts.
#' The function allows for flexible specification of insample forecasting methods and handles the preparation of forecast errors for the DM test, including adjustments for overlapping forecasts.
#' The results include DM statistics and p-values for each variable and forecast horizon.
#'
#' @param model A model of class 'osem' (to be returned by \code{\link{run_model}}).
#' @param insample_model A model of class 'osem.forecast.insample' (to be returned by \code{\link{forecast_insample}}) to be used for the insample forecast comparisons. If provided, other options for insample modelling (sample share, methods, etc.) are ignored.
#' @param insample_methods A character vector of methods to use for the insample forecast comparisons. Default is c("ets", "auto"). Note that the method specified in insample_main_comparison must be included in this vector.
#' @param insample_main_comparison A character string specifying the method to be used as the main comparison for the insample forecasts. Default is "ets". Must be one of the methods specified in insample_methods.
#' @param insample_sample_share A numeric value between 0 and 1 specifying the share of the sample to be used for insample forecasting. Default is 0.5.
#' @param comparison_methods A character vector specifying the methods to compare against the insample main comparison method. Default is c("VAR", "BVAR"). Must be a subset of c("VAR", "BVAR", "RW", "ar", "ets", "auto") which correspond to Vector Autoregressions (VAR), Bayesian VAR (BVAR), Random Walks (RW) and Autoregressive models.
#' @param parallel.cores An integer specifying the number of cores to use for parallel processing when running insample forecasts. If NULL, the function will not use parallel processing. Default is NULL.
#' @param dm.horizons An integer specifying the forecast horizons to be used for the Diebold-Mariano test. Must be a single positive integer. Default is 8.
#' @param dm.variance A character string specifying the variance estimator to be used in the Diebold-Mariano test. Default is "bartlett". Must be one of the variance estimators supported by forecast::dm.test().
#' @param dm.alternative A character string specifying the alternative hypothesis for the Diebold-Mariano test. Default is "two.sided". Must be one of "two.sided", "less", or "greater".
#' @param dm.power A numeric value specifying the power to which forecast errors are raised in the Diebold-Mariano test. Default is 2 (squared errors). Must be a single positive number.
#' @param lags An integer specifying the number of lags to be used in the comparison methods. Default is 4.
#' @param grepl_variables A character vector of variable names to be included in the forecast comparisons. If NULL, all variables will be included. Default is NULL.
#' @param comparison_data A data frame containing pre-prepared forecast errors and related information for the DM test. If provided, the function will use this data instead of running the insample forecasts and preparing the data internally. Default is NULL.
#' @param quiet A logical value indicating whether to suppress messages during the execution of the function. Default is FALSE.
#'
#' @returns A list containing the results of the Diebold-Mariano test for forecast comparisons, including a tibble with DM statistics and p-values for each variable and forecast horizon, as well as the prepared data used for the comparisons.
#' If the insample_model was not provided, the list will also include the insample forecast model used for the comparisons.
#' @export
#'
dm_test <- function(model,
                    insample_model = NULL,
                    insample_methods = c("ets", "auto"),
                    insample_main_comparison = "ets",
                    insample_sample_share = 0.5,
                    comparison_methods = c("VAR","BVAR"),
                    parallel.cores = NULL,
                    dm.horizons = 8,
                    dm.variance = "bartlett",
                    dm.alternative = "two.sided",
                    dm.power = 2,
                    lags = 4,
                    grepl_variables = NULL,
                    comparison_data = NULL,
                    quiet = FALSE
) {

  if(!quiet & !is.null(insample_model)){
    message("Using provided insample_model for forecast comparisons, other options for insample modelling (sample share, methods, etc.) are ignored.")
  }
  if(!is.null(insample_model) & !(insample_main_comparison %in% unique(insample_model$central$method))){
    stop("The method specified in insample_main_comparison must be included in the methods used in the provided insample_model.")
  }
  # check inputs for false inputs
  if(!insample_main_comparison %in% insample_methods) {
    stop("insample_main_comparison must be one of the insample_methods")
  }
  if(!all(comparison_methods %in% c("VAR", "BVAR", "RW", "ar", "ets", "auto"))) {
    stop("comparison_methods must be a subset of c('VAR', 'BVAR', 'ar','ets', 'auto')")
  }
  if(!is.numeric(dm.horizons) || any(dm.horizons < 1) || length(dm.horizons) != 1) {
    stop("dm.horizons must be a single  positive integers")
  }
  if(!is.numeric(dm.power) || length(dm.power) != 1 || dm.power <= 0) {
    stop("dm.power must be a single positive number, either 1 (absolute error) or 2 (squared error).")
  }
  if(!dm.alternative %in% c("two.sided", "less", "greater")) {
    stop("dm.alternative must be one of 'two.sided', 'less', or 'greater'")
  }
  # check lags
  if(!is.numeric(lags) || length(lags) != 1 || lags < 0) {
    stop("lags must be a single non-negative integer")
  }

  # Define relevant helper functions ----------------------------------------


  get_errors <- function(dat, variable, h, m1, m2) {

    wide <- dat %>%
      dplyr::filter(.data$na_item == variable,
                    .data$Horizon == h,
                    .data$forecast_type %in% c(m1, m2)) %>%
      dplyr::select("time", "forecast_type", "diff") %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = "forecast_type", values_from = "diff") %>%
      dplyr::arrange(.data$time) %>%
      tidyr::drop_na()

    e1 <- wide[[m1]]
    e2 <- wide[[m2]]

    list(e1 = e1, e2 = e2, wide = wide)
  }

  run_dm_forecast <- function(dat, variable, h, m1, m2, power, alternative) {

    tmp <- get_errors(dat, variable, h, m1, m2)

    if(is.null(tmp$e1)){return(NULL)}
    if(is.null(tmp$e2)){return(NULL)}
    if(length(tmp$e1) < h){return(NULL)}

    # forecast::dm.test expects forecast errors (not losses)
    # h = forecast horizon to handle overlap
    try(
      dm.out <- forecast::dm.test(tmp$e1, tmp$e2,
                                  alternative = dm.alternative, h = h,
                                  power = dm.power, varestimator = dm.variance),
      silent = TRUE
    )
    if(!exists("dm.out")){return(NULL)}

    # Return a tidy tibble
    dplyr::tibble(
      variable = variable,
      Horizon  = h,
      model_1  = m1,
      model_2  = m2,
      statistic = unname(dm.out$statistic),
      p_value   = dm.out$p.value,
      alternative = alternative,
      power = power,
      n = length(tmp$e1)
    )
  }

  q_index <- function(d) {
    as.integer(format(d, "%Y")) * 4L + ((as.integer(format(d, "%m")) - 1L) %/% 3L + 1L)
  }

  q_start <- function(qi) {
    y <- qi %/% 4L
    q <- qi %% 4L
    # handle modulo = 0 case (Q4)
    y <- ifelse(q == 0L, y - 1L, y)
    q <- ifelse(q == 0L, 4L, q)
    as.Date(sprintf("%d-%02d-01", y, (q - 1L) * 3L + 1L))
  }


  if(is.null(comparison_data)){
    if(is.null(insample_model)){
      if(!quiet){
        message("Running insample forecasts")
      }
      insample_mod <- forecast_insample(model,
                                        sample_share = insample_sample_share,
                                        quiet = quiet,
                                        plot = FALSE,
                                        parallel.cores = parallel.cores,
                                        exog_fill_method = insample_methods)
    } else {
      insample_mod <- insample_model
    }


    naive_dat <- dplyr::tibble()

    for(j in comparison_methods){

      if(!quiet){
        message(paste0("Running ",j," forecast comparison"))
      }
      for(i in 1:length(insample_mod$all_models)){

        fc_comparison_base <- insample_mod$all_models[i][[1]]

        naive_dat <- naive_dat %>%
          dplyr::bind_rows(forecast_comparison2(fc_comparison_base,
                                                n.ahead = dm.horizons,
                                                forecast_type = j, lags = lags,
                                                grepl_variables = grepl_variables))
      }
    }

    full_data <- model$processed_input_data

    insample_values <- insample_mod$central %>%
      dplyr::mutate(forecast_type = paste0("OSEM ",.data$method)) %>%
      dplyr::mutate(
        Horizon = q_index(.data$time) - q_index(.data$start),
        na_item = .data$dep_var
      ) %>%
      dplyr::rename(Origin_Date = "start",
                    value = "values") %>%
      dplyr::select(-"method", -"end") %>%

      {if(!is.null(grepl_variables)){
        dplyr::filter(., grepl(paste(grepl_variables, collapse = "|"), .data$na_item))
      } else {
        .
      }}

    dat <- naive_dat %>%
      {if(!"dep_var" %in% names(naive_dat)){dplyr::mutate(., dep_var = NA_character_)} else {.}} %>%
      tidyr::pivot_longer(-c("Origin_Date", "Horizon", "dep_var", "forecast_type"), names_to = "na_item") %>%
      dplyr::mutate(time = q_start(q_index(.data$Origin_Date) + .data$Horizon)) %>%
      tidyr::drop_na("value") %>%

      dplyr::bind_rows(insample_values) %>%

      dplyr::inner_join(full_data, by = c("na_item", "time")) %>%
      dplyr::rename(hist_value = "values") %>%

      dplyr::mutate(diff = .data$value - .data$hist_value,
                    sq_error = .data$diff^2,
                    dep_var = ifelse(is.na(.data$dep_var), .data$na_item, .data$dep_var)) %>%

      dplyr::full_join(model$module_order %>%
                         dplyr::select("order", dep_var = "dependent"), by = "dep_var") %>%
      dplyr::filter(order == min(.data$order), .by = c("Origin_Date", "Horizon", "na_item", "forecast_type"))
  } else {
    dat <- comparison_data
  }

  res_dm_forecast <- dplyr::tibble()

  vars_to_cycle <- dat %>%
    dplyr::distinct(.data$na_item) %>%
    dplyr::pull("na_item") %>%
    {if(!is.null(grepl_variables)){
      .[grepl(paste(grepl_variables, collapse = "|"), .)]
    } else {
      .
    }}

  for(var in vars_to_cycle){
    for(compare in comparison_methods){

      res_dm_forecast_int <- dplyr::tibble()
      for(h in 1:dm.horizons){
        intermed <- run_dm_forecast(
          dat, var, h,
          m1 = paste0("OSEM ",insample_main_comparison),
          m2 = compare,
          power = dm.power, alternative = dm.alternative)

        res_dm_forecast_int <- res_dm_forecast_int %>%
          dplyr::bind_rows(intermed)
      }
      #res_dm_forecast_int <- dplyr::bind_rows(
      # lapply(1:dm.horizons, function(h) {
      #   run_dm_forecast(dat, var, h,
      #                   m1 = paste0("OSEM ",insample_main_comparison), m2 = compare,
      #                   power = dm.power, alternative = dm.alternative)
      # }))
      res_dm_forecast <- dplyr::bind_rows(res_dm_forecast,
                                          res_dm_forecast_int %>%
                                            dplyr::mutate(model_1 = paste0("OSEM ", insample_main_comparison),
                                                          model_2 = compare,
                                                          var = var))
    }
  }

  out <- list()
  out$dm_forecast = res_dm_forecast
  out$dat <- dat
  if(is.null(insample_model)){
    out$insample_model <- insample_mod
  }
  return(out)
}




# library(dplyr)
# library(tidyr)
# library(forecast)
#
#
#
# df <- read.csv(test_path("testdata", "ragged_edge", "ragged_edge_emissions_data.csv"))
#
# spec <- dplyr::tibble(
#   type = c(
#     "n",
#     "n",
#     "n",
#     "n"
#   ),
#   dependent = c(
#     "Import",
#     "FinConsExpHH",
#     "GCapitalForm",
#     "EmiCO2Combustion"
#   ),
#   independent = c(
#     "FinConsExpHH + GCapitalForm",
#     "",
#     "GValueAdd",
#     "FinConsExpHH + GCapitalForm + GValueAdd"
#   )
# )
#
# mod <- run_model(
#   specification = spec,
#   dictionary = dict,
#   input = df,
#   primary_source = "local",
#   present = FALSE,
#   quiet = TRUE, saturation = "IIS"
# )
#
# set.seed(1298)
#
#
#
# prepare_dm <- function(data, variable, h) {
#
#   data %>%
#     filter(
#       na_item == variable,
#       Horizon == h
#     ) %>%
#     select(time, forecast_type, sq_error) %>%
#     pivot_wider(
#       names_from = forecast_type,
#       values_from = sq_error
#     ) %>%
#     arrange(time) %>%
#     drop_na()
# }
# prepare_dm(dat, "FinConsExpHH", 1)
# library(sandwich)
# library(lmtest)
# run_dm <- function(df, h) {
#
#   # Loss differential: VAR - BVAR
#   d <- df$VAR - df$BVAR
#
#   T <- length(d)
#
#   # Mean regression
#   reg <- lm(d ~ 1)
#
#   # HAC variance with lag h-1
#   S <- NeweyWest(reg, lag = h - 1, prewhite = FALSE)
#
#   dm_stat <- coef(reg)[1] / sqrt(S[1,1])
#
#   # Harvey small sample correction
#   adj <- sqrt((T + 1 - 2*h + h*(h-1)/T) / T)
#   dm_stat_adj <- dm_stat * adj
#
#   p_value <- 2 * (1 - pnorm(abs(dm_stat_adj)))
#
#   tibble(
#     T = T,
#     DM = dm_stat_adj,
#     p_value = p_value
#   )
# }
# results <- lapply(1:8, function(h) {
#
#   df_h <- prepare_dm(dat, "GCapitalForm", h)
#
#   out <- run_dm(df_h, h)
#
#   out$Horizon <- h
#
#   out
# }) %>% bind_rows()
#
#

