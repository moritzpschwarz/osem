#' Plot a Forecast Object of the Aggregate Model
#'
#' @param x An object of class aggmod.forecast, which is the output from the forecast_model function.
#' @param exclude.exogenous Logical. Should exogenous values be plotted? Default is FALSE.
#' @param order.as.run Logical. Should the plots be arranged in the way that the model was run? Default FALSE.
#' @param interactive Logical. Should the plots be interactive using plotly::ggplotly(). Default is FALSE.
#' @param ... Further arguments (currently not in use).
#'
#' @export
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
#' )
#' )
#'
#' fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
#' fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
#' filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa,
#' "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)
#'\donttest{
#' a <- run_model(specification = spec, dictionary = NULL,
#' inputdata_directory = NULL, filter_list = filter_list, download = TRUE,
#' save_to_disk = NULL, present = FALSE)
#' plot(forecast_model(a))
#'}
plot.aggmod.forecast <- function(x, exclude.exogenous = TRUE, order.as.run = FALSE, interactive = FALSE, ...){

  #if(class(x) != "aggmod.forecast"){
  if(!isa(x, "aggmod.forecast")){

    stop("Input object not of type aggmod.forecast. Run 'forecast_model' again and use the output of that function.")
  }

  x$orig_model$full_data %>%
    dplyr::mutate(var = na_item,
                  na_item = gsub("\\.hat","",na_item),
                  fit = as.character(stringr::str_detect(var, ".hat"))) -> plot_df

  plot_df %>%
    dplyr::distinct(na_item, fit) %>%
    dplyr::mutate(fit = as.logical(fit)) %>%
    dplyr::filter(fit) %>%
    dplyr::pull(na_item) -> when_excluding_exog


  if(exclude.exogenous){
    plot_df %>%
      dplyr::filter(na_item %in% when_excluding_exog) -> plot_df
  }


  if(order.as.run & !exclude.exogenous){cat("As 'order.as.run' is TRUE, exogenous values will not be shown.\n")}

  # dplyr::left_join(x$dictionary %>%
  #             dplyr::rename(na_item = model_varnma) %>%
  #             dplyr::select(na_item, model_varname), by = "na_item")

  # plot_df %>%
  #   ggplot2::ggplot(ggplot2::aes(x = time, y = values, color = fit, group = var, fit = var)) +
  #   ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
  #
  #   ggplot2::facet_wrap(~na_item, scales = "free") +
  #
  #   ggplot2::labs(x = NULL, y = NULL) +
  #
  #   ggplot2::scale_y_continuous(labels = scales::comma) +
  #   ggplot2::scale_color_viridis_d() +
  #
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(legend.position = "none",
  #         panel.grid.major.x = ggplot2::element_blank(),
  #         panel.grid.minor.x = ggplot2::element_blank(),
  #         panel.grid.minor.y = ggplot2::element_blank()) -> initial_plot


  x$forecast %>%
    dplyr::select(central.estimate) %>%
    tidyr::unnest(central.estimate) %>%
    tidyr::pivot_longer(-c(time)) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(id_cols = c(time), names_from = name, values_from = value) -> central_forecasts

  # x$forecast %>%
  #   mutate(dep_var = map(central.estimate, function(x){
  #     names(x)[2]
  #   })) %>%
  #   unnest(dep_var) %>%
  #   relocate(dep_var,)
  #
  #
  # for(e in 1:length(x$forecast$central.estimate)){
  #   # e = 1
  #   ti <- x$forecast$central.estimate[[e]]$time
  #   name <- names(x$forecast$central.estimate[[e]])[2]
  #
  #   cbind(dplyr::tibble(time = ti,
  #                       name = name),
  #         x$forecast$all.estimates[[e]])
  #
  #
  #
  # }

  # Central Forecast -----------
  x$forecast %>%
    dplyr::select(dep_var, all.estimates) %>%
    tidyr::unnest(all.estimates) %>%
    tidyr::pivot_longer(-c(time, dep_var)) -> all_forecasts
  #tidyr::pivot_wider(id_cols = c(time), names_from = name, values_from = value)

  central_forecasts %>%
    names %>%
    stringr::str_detect(., "^ln.") -> to_exponentiate

  plot_df %>%
    dplyr::filter(fit == "TRUE") %>%
    dplyr::group_by(na_item) %>%
    dplyr::filter(time == max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-var) %>%
    dplyr::mutate(fit = "Forecast") -> last_hist_value

  # Dealing with uncertainty (all forecasts) --------

  dplyr::tibble(dep_var = names(central_forecasts),
                expo = to_exponentiate,
                all = c("time",unique(all_forecasts$dep_var))) -> to_exponentiate_tibble

  central_forecasts %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
    dplyr::rename_with(.fn = ~gsub("ln.","",.)) %>%

    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") %>%
    dplyr::full_join(x$orig_model$module_order_eurostatvars %>%
                       dplyr::select(dependent) %>%
                       dplyr::rename(na_item = dependent), by = "na_item") %>%

    dplyr::mutate(fit = "Forecast") -> forecasts_processed

  all_forecasts %>%
    dplyr::mutate(value = dplyr::case_when(dep_var %in% to_exponentiate_tibble$all[to_exponentiate_tibble$expo == TRUE] ~ exp(value),
                                           TRUE ~ value)) %>%
    dplyr::rename(na_item = dep_var, values = value) %>%
    dplyr::full_join(x$orig_model$module_order_eurostatvars %>%
                       dplyr::select(dependent) %>%
                       dplyr::rename(na_item = dependent), by = "na_item") %>%
    dplyr::mutate(fit = "Forecast Uncertainty") -> all_forecasts_processed

  all_forecasts_processed %>%
    dplyr::group_by(na_item, time, fit) %>%
    dplyr::summarise(max = max(values),
                     min = min(values),
                     p975 = quantile(values, probs = 0.975),
                     p025 = quantile(values, probs = 0.025),
                     p75 = quantile(values, probs = 0.75),
                     p25 = quantile(values, probs = 0.25)) -> all_forecasts_processed_q



  last_hist_value %>%
    dplyr::group_by(time,na_item) %>%
    dplyr::summarise(max = values,
                     min = values,
                     p975 = values,
                     p025 = values,
                     p75 =  values,
                     p25 =  values, .groups = "drop") %>%
    dplyr::mutate(fit = "Forecast Uncertainty") %>%
    dplyr::bind_rows(.,all_forecasts_processed_q) -> all_forecasts_processed_q


  # Exogenous forecasts --------

  x$exog_data %>%
    dplyr::select(-dplyr::any_of(c("q_1","q_2","q_3","q_4"))) %>%
    tidyr::pivot_longer(-"time", values_to = "values", names_to = "na_item") %>%
    dplyr::bind_rows(last_hist_value) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(fit = "Forecast/Assumption of\nExogenous Variables") -> exog_forecasts



  plot_df %>%
    dplyr::filter(na_item %in%  exog_forecasts$na_item) %>%
    dplyr::group_by(na_item) %>%
    dplyr::filter(time == max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-var) %>%
    dplyr::mutate(fit = "Forecast/Assumption of\nExogenous Variables") %>%

    dplyr::bind_rows(exog_forecasts) -> exog_forecasts


  # dplyr::bind_rows(forecasts_processed, last_hist_value) %>%
  #   dplyr::bind_rows(plot_df) %>%
  #
  #   {if(order.as.run){
  #     dplyr::mutate(.,na_item = factor(na_item, levels = x$orig_model$module_order_eurostatvars$dependent)) %>%
  #       tidyr::drop_na(na_item) %>%
  #       dplyr::arrange(time, na_item)} else {.}} %>%
  #
  #   ggplot2::ggplot(ggplot2::aes(x = time, y = values, color = fit)) +
  #   ggplot2::geom_line(linewidth = 1) +
  #
  #   ggplot2::facet_wrap(~na_item, scales = "free") +
  #
  #   ggplot2::labs(x = NULL, y = NULL) +
  #
  #   ggplot2::scale_y_continuous(labels = scales::comma) +
  #   ggplot2::scale_color_viridis_d() +
  #
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(legend.position = "none",
  #                  panel.grid.major.x = ggplot2::element_blank(),
  #                  panel.grid.minor.x = ggplot2::element_blank(),
  #                  panel.grid.minor.y = ggplot2::element_blank())



  ggplot_options <- list(
    if (!exclude.exogenous) {ggplot2::geom_line(data = exog_forecasts, ggplot2::aes(x = time, y = values, color = fit))} else {NULL}
  )


  dplyr::bind_rows(forecasts_processed, last_hist_value) %>%
    dplyr::bind_rows(plot_df) %>%

    ggplot2::ggplot(ggplot2::aes(x = time, y = values, color = fit)) +

    #ggplot2::geom_line(data = all_forecasts_processed, ggplot2::aes(group = paste0(name,na_item), y = values, x = time), linewidth = 0.1, alpha = 0.8) +
    ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = min, x = time, ymax = max, fill = fit), linewidth = 0.1, alpha = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = p025, x = time, ymax = p975, fill = fit), linewidth = 0.1, alpha = 0.3, inherit.aes = FALSE) +
    ggplot2::geom_ribbon(data = all_forecasts_processed_q, ggplot2::aes(ymin = p25, x = time, ymax = p75, fill = fit), linewidth = 0.1, alpha = 0.5, inherit.aes = FALSE) +

    ggplot2::geom_line(linewidth = 1) +
    ggplot_options +

    ggplot2::facet_wrap(~na_item, scales = "free") +

    ggplot2::labs(x = NULL, y = NULL) +

    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_viridis_d() +

    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> p

  if(interactive){
    plotly::ggplotly(p)
  } else {
    return(p)
  }




}
