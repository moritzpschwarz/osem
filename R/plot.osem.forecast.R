#' Plot OSEM Forecast
#'
#' Plot a forecast object of the OSEM Model
#'
#' @param x An object of class osem.forecast, which is the output from the \link{forecast_model} function.
#' @param exclude.exogenous Logical. Should exogenous values be plotted? Default is FALSE.
#' @param order.as.run Logical. Should the plots be arranged in the way that the model was run? Default FALSE.
#' @param interactive Logical. Should the resulting plot be launched in an interactive way (the plotly package is required for this).
#' @param first_date Character. First date value to be shown. Must be a character value that can be turned into a date using as.Date() or NULL.
#' @param grepl_variables Regular Expression Character. Can be used to select variables to be plotted. Experimental feature so use with care.
#' @param title Character. Title of the plot. Default is "OSEM Model Forecast".
#' @param linewidth Numeric. Linewidth argument for the plot. Default is 1.
#' @param return.data Logical. Do not return a plot but rather just the final dataset that has been created for the plot.
#' @param ... Additional arguments passed to the plotting function.
#'
#' @export
#'
#' @return A ggplot2 object or a plotly object if interactive is TRUE. If return.data is TRUE, a tibble with the data used for plotting.
#'
#' @examples
#' spec <- dplyr::tibble(
#' type = c(
#'   "d",
#'   "d",
#'   "n"
#' ),
#' dependent = c(
#'   "StatDiscrep",
#'   "TOTS",
#'   "Import"
#' ),
#' independent = c(
#'   "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#'   "GValueAdd + Import",
#'   "FinConsExpHH + GCapitalForm"
#' ),
#' lag = c(
#'   "",
#'   "",
#'   ""
#' ),
#' cvar = c(
#'   "",
#'   "",
#'   ""
#' )
#' )
#'\donttest{
#' a <- run_model(specification = spec, dictionary = NULL,
#' inputdata_directory = NULL, primary_source = "download",
#' save_to_disk = NULL, present = FALSE)
#' plot(forecast_model(a))
#'}
#'

plot.osem.forecast <- function(x, title = "OSEM Model Forecast", exclude.exogenous = TRUE, order.as.run = FALSE, interactive = FALSE, first_date = NULL, grepl_variables = NULL, return.data = FALSE, linewidth = 1, ...){

  if(!isa(x, "osem.forecast")){
    stop("Input object not of type 'osem.forecast'. Run 'forecast_model' again and use the output of that function.")
  }

  if(!is.null(first_date)){if(!is.character(first_date) | !lubridate::is.Date(as.Date(first_date))){stop("When supplying 'first_date', the it must be a character and must be (able to be converted to) a Date.")}}

  x$orig_model$full_data %>%
    dplyr::mutate(var = .data$na_item,
                  na_item = gsub("\\.hat","",.data$na_item),
                  fit = as.character(stringr::str_detect(.data$var, ".hat"))) -> plot_df


  if(order.as.run & !exclude.exogenous){
    cat("As 'order.as.run' is TRUE, exogenous values will not be shown. So 'exclude.exogenous' will be ignored.\n")
    exclude.exogenous <- TRUE
  }
  if(exclude.exogenous){
    # determine which variables to exclude, when we are not including exogenous variables
    plot_df %>%
      dplyr::distinct(dplyr::across(c("na_item", "fit"))) %>%
      dplyr::mutate(fit = as.logical(.data$fit)) %>%
      dplyr::filter(.data$fit) %>%
      dplyr::pull("na_item") -> when_excluding_exog

    plot_df %>%
      dplyr::filter(.data$na_item %in% when_excluding_exog) -> plot_df
  }

  # get log information
  if(!is.null(x$orig_model$opts_df[["log_opts"]])){
    x$orig_model$opts_df %>%
      dplyr::mutate(log_opts_dependent = purrr::map2(.data$log_opts, .data$dependent, function(opts,dep){
        opts[,dep, drop = TRUE]
      })) %>%
      tidyr::unnest("log_opts_dependent", keep_empty = TRUE) %>%
      tidyr::replace_na(list(log_opts_dependent = "none")) %>%
      dplyr::select(c("dep_var" = "dependent","log_opt" = "log_opts_dependent")) -> log_opts_processed
  } else {
    log_opts_processed <- dplyr::tibble(dep_var = x$orig_model$opts_df$dependent, log_opt = "none")
  }

  # CENTRAL FORECASTS --------
  # get the central forecasts
  x$forecast %>%
    dplyr::select("dep_var", "central.estimate") %>%
    tidyr::unnest("central.estimate") %>%
    tidyr::pivot_longer(-c("time","dep_var")) %>%
    tidyr::drop_na() %>%
    dplyr::full_join(log_opts_processed, by = "dep_var") %>%
    dplyr::mutate(value = dplyr::case_when(.data$log_opt == "log" ~ exp(.data$value),
                                           .data$log_opt == "asinh" ~ sinh(.data$value),
                                           .data$log_opt == "none" ~ .data$value)) %>%
    dplyr::select(-c("name", "log_opt")) %>%
    dplyr::rename(values = "value",
                  na_item = "dep_var") %>%
    dplyr::mutate(fit = "forecast")-> forecasts_processed

  # ALL FORECASTS --------
  # Dealing with uncertainty (all forecasts)
  # unnest the set of all estimates from the original object
  x$forecast %>%
    dplyr::select("dep_var", "all.estimates") %>%
    dplyr::full_join(log_opts_processed, by = "dep_var") %>%
    tidyr::unnest("all.estimates") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("run_"), ~dplyr::case_when(.data$log_opt == "log" ~ exp(.),
                                                                              .data$log_opt == "asinh" ~ sinh(.),
                                                                              .data$log_opt == "none" ~ .))) %>%
    dplyr::select(-"log_opt") -> all_forecasts_unnested

  if(nrow(all_forecasts_unnested) > 0){
    all_forecasts_unnested %>%
      tidyr::pivot_longer(-c("time", "dep_var")) %>%
      dplyr::rename(na_item = "dep_var", values = "value") %>%

      dplyr::mutate(fit = "Forecast Uncertainty")  %>%

      tidyr::drop_na("time") %>%
      dplyr::group_by(.data$na_item, .data$time, .data$fit) %>%
      dplyr::summarise(
        p95 = stats::quantile(.data$values, probs = 0.95),
        p05 = stats::quantile(.data$values, probs = 0.05),
        p975 = stats::quantile(.data$values, probs = 0.975),
        p025 = stats::quantile(.data$values, probs = 0.025),
        p75 = stats::quantile(.data$values, probs = 0.75),
        p25 = stats::quantile(.data$values, probs = 0.25)) %>%
      dplyr::ungroup() -> all_forecasts_processed_q
  }

  # NOWCASTS --------
  if(!is.null(x$nowcast_data)) {
    x$nowcast_data %>%
      dplyr::mutate(fit = "nowcast") -> nowcast_processed
  } else {nowcast_processed <- NULL}


  # EXOGENOUS FORECASTS --------
  x$exog_data_nowcast %>%
    dplyr::select(-dplyr::any_of(c("q_1","q_2","q_3","q_4"))) %>%

    {if(ncol(.) > 1){ # checks if any exogenous variables (alternative would be entire model just a collection of AR models)
      tidyr::pivot_longer(.,-"time", values_to = "values", names_to = "na_item") %>%
        dplyr::arrange(.data$time) %>%
        dplyr::mutate(fit = "Forecast/Assumption of\nExogenous Variables") %>%
        {if(!is.null(grepl_variables)){dplyr::filter(., grepl(grepl_variables,.data$na_item))} else {.}}
    } else {
      NULL
    }} -> exog_forecasts

  # LAST VALUES -------------------------------------------------------------
  # here we get the last fitted value
  plot_df %>%
    dplyr::filter(.data$fit == "TRUE", !is.na(.data$values)) %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::filter(.data$time == max(.data$time, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"var") %>%
    dplyr::mutate(fit = "forecast") -> last_fitted_value

  # and the last historical value
  plot_df %>%
    dplyr::filter(.data$fit == "FALSE") %>%
    tidyr::drop_na("values") %>%
    dplyr::group_by(.data$na_item) %>%
    dplyr::filter(.data$time == max(.data$time, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"var") %>%
    dplyr::mutate(fit = "forecast") -> last_hist_value

  # and the last nowcasted value
  if(!is.null(x$nowcast_data)) {
    nowcast_processed %>%
      dplyr::filter(.data$fit == "nowcast") %>%
      dplyr::group_by(.data$na_item) %>%
      dplyr::filter(.data$time == max(.data$time, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(fit = "forecast") -> last_nowcast_value
  }


  # CONSTRUCTING JOINT LINES --------
  if(!is.null(nowcast_processed)){
    nowcast_processed %>%
      dplyr::distinct(.data$na_item) %>%
      dplyr::pull("na_item") -> nowcast_present
  } else {nowcast_present <- NULL}

  ## Exogenous Forecasts ---
  if(!is.null(exog_forecasts)){
    exog_forecasts %>%
      dplyr::bind_rows(last_hist_value %>%
                         dplyr::filter(.data$na_item %in% unique(exog_forecasts$na_item))) %>%
      dplyr::mutate(fit = "Forecast/Assumption of\nExogenous Variables") -> exog_forecasts
  }

  ## Central Forecasts ----

  forecasts_processed <- forecasts_processed %>%
    dplyr::bind_rows(last_hist_value %>% dplyr::filter(!.data$na_item %in% nowcast_present)) %>%
    {if(!is.null(nowcast_processed)){dplyr::bind_rows(.,last_nowcast_value %>% dplyr::filter(.data$na_item %in% nowcast_present))}else{.}}

  ## All Forecasts ---
  if(nrow(all_forecasts_unnested) > 0){
    all_forecasts_processed_q %>%
      dplyr::bind_rows(last_hist_value %>%
                         dplyr::filter(!.data$na_item %in% nowcast_present) %>%
                         dplyr::reframe(p95 = .data$values,
                                        p05 = .data$values,
                                        p975 = .data$values,
                                        p025 = .data$values,
                                        p75 =  .data$values,
                                        p25 =  .data$values, .by = "time", .data$na_item) %>%
                         dplyr::mutate(fit = "Forecast Uncertainty")) %>%
      {if(!is.null(nowcast_processed)){dplyr::bind_rows(.,last_nowcast_value %>%
                                                          dplyr::filter(.data$na_item %in% nowcast_present) %>%
                                                          dplyr::reframe(p95 = .data$values,
                                                                         p05 = .data$values,
                                                                         p975 = .data$values,
                                                                         p025 = .data$values,
                                                                         p75 =  .data$values,
                                                                         p25 =  .data$values, .by = "time", .data$na_item) %>%
                                                          dplyr::mutate(fit = "Forecast Uncertainty"))} else {.}} -> all_forecasts_processed_q

    if(!is.null(grepl_variables)){all_forecasts_processed_q <- dplyr::filter(all_forecasts_processed_q, grepl(grepl_variables,.data$na_item))}
  }

  all_forecasts_processed_q <- all_forecasts_processed_q %>%
    {if(order.as.run){
      dplyr::mutate(.,na_item = factor(.data$na_item, levels = x$orig_model$module_order$dependent)) %>%
        tidyr::drop_na("na_item") %>%
        dplyr::arrange(.data$time, .data$na_item)} else {.}}


  ## Nowcasts ----
  nowcast_processed %>%
    dplyr::bind_rows(last_hist_value %>%
                       dplyr::filter(.data$na_item %in% nowcast_present)) %>%
    dplyr::mutate(fit = "nowcast") -> nowcast_processed


  plotting_df_ready <- forecasts_processed %>%
    dplyr::bind_rows(plot_df) %>%
    dplyr::bind_rows(nowcast_processed) %>%

    {if(order.as.run){
      dplyr::mutate(.,na_item = factor(.data$na_item, levels = x$orig_model$module_order$dependent)) %>%
        tidyr::drop_na("na_item") %>%
        dplyr::arrange(.data$time, .data$na_item)} else {.}} %>%

    {if(!is.null(first_date)){dplyr::filter(., .data$time >= as.Date(first_date))} else {.}} %>%

    {if(!is.null(grepl_variables)){dplyr::filter(., grepl(grepl_variables,.data$na_item))} else {.}} %>%

    dplyr::mutate(fit = dplyr::case_when(fit == "TRUE" ~ "Insample Fit",
                                         fit == "FALSE" ~ "Observation",
                                         fit == "nowcast" ~ "Nowcast",
                                         fit == "forecast" ~ "Forecast",
                                         TRUE ~ fit)) %>%
    dplyr::mutate(fit = factor(.data$fit, levels = c("Observation","Insample Fit", "Nowcast", "Forecast")))

  ggplot_options <- list(
    if (!exclude.exogenous & !is.null(exog_forecasts)) {ggplot2::geom_line(data = exog_forecasts, ggplot2::aes(x = .data$time, y = .data$values, color = .data$fit))} else {NULL}
  )

  #"#440154FF" "#3B528BFF" "#21908CFF" "#5DC863FF" "#FDE725FF"
  # viridis::viridis(4)
  colors <- c(
    "Forecast/Assumption of\nExogenous Variables" = "#31688EFF",
    "Forecast" = "#3B528BFF",
    "Insample Fit" = "#FDE725FF",
    "Observation" = "#440154FF",
    "Nowcast" = "#35B779FF"
  )

  if(nrow(all_forecasts_unnested) > 0){
    uncertainty_layers <- list(
      ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = .data$p05, x = .data$time, ymax = .data$p95, fill = .data$fit), linewidth = 0.1, alpha = 0.3, inherit.aes = FALSE, na.rm = TRUE),
      ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = .data$fit), linewidth = 0.1, alpha = 0.3, inherit.aes = FALSE, na.rm = TRUE),
      ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = .data$fit), linewidth = 0.1, alpha = 0.5, inherit.aes = FALSE, na.rm = TRUE))
  } else {
    uncertainty_layers <- list()
  }

  plotting_df_ready %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$values, color = .data$fit)) +

    uncertainty_layers +

    ggplot2::geom_line(linewidth = linewidth, na.rm = TRUE) +

    ggplot_options +

    ggplot2::facet_wrap(~.data$na_item, scales = "free") +

    ggplot2::labs(x = NULL, y = NULL, title = title) +

    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    #ggplot2::scale_color_viridis_d() +
    ggplot2::scale_color_manual(values = colors) +

    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> p

  if(return.data){
    # prepare a nice dataset to be spit out

    plotting_df_ready %>%
      {if(!exclude.exogenous){dplyr::bind_rows(.,exog_forecasts %>%
                                                 dplyr::mutate(fit = "Exogenous Forecast"))} else {.}} %>%
      dplyr::select(-"var") %>%

      {if(nrow(all_forecasts_unnested) > 0){
        dplyr::full_join(.,all_forecasts_processed_q %>%
                           dplyr::select(-"fit"),
                         by = dplyr::join_by("time", "na_item"),
                         relationship = "many-to-many")} else {.}} %>%

      dplyr::mutate(fit = dplyr::case_when(fit == "forecast" ~ "Endogenous Forecast",
                                           fit == "TRUE" ~ "Insample Fit",
                                           fit == "FALSE" ~ "Observation",
                                           TRUE ~ fit)) %>%
      dplyr::rename(type = "fit") %>%
      dplyr::arrange(dplyr::desc(.data$time), .data$na_item, .data$type) %>%
      dplyr::relocate(dplyr::any_of(c("time", "na_item", "values", "type", "p95", "p05", "p975",
                        "p025", "p75", "p25"))) %>% # here any_of because with only identity the percentiles don't exist

      return()
  } else {
    if(interactive){
      plotly::ggplotly(p)
    } else {
      suppressWarnings(suppressMessages(print(p)))
    }
  }






}
