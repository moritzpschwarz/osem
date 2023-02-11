#' Plot a Forecast Object of the Aggregate Model
#'
#' @param x An object of class aggmod.forecast, which is the output from the forecast_model function.
#' @param exclude.exogenous Logical. Should exogenous values be plotted? Default is FALSE.
#' @param order.as.run Logical. Should the plots be arranged in the way that the model was run? Default FALSE.
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
plot.aggmod.forecast <- function(x, exclude.exogenous = TRUE, order.as.run = FALSE, ...){

  #if(class(x) != "aggmod.forecast"){
  if(!isa(x, "aggmod.forecast")){

    stop("Input object not of type aggmod.forecast. Run 'forecast_model' again and use the output of that function.")
  }

  x$orig_model$full_data %>%
    dplyr::mutate(var = na_item,
           na_item = gsub("\\.hat","",na_item),
           fit = as.character(str_detect(var, ".hat"))) -> plot_df

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
  #             rename(na_item = model_varnma) %>%
  #             dplyr::select(na_item, model_varname), by = "na_item")

  # plot_df %>%
  #   ggplot(aes(x = time, y = values, color = fit, group = var, fit = var)) +
  #   geom_line(size = 1, na.rm = TRUE) +
  #
  #   facet_wrap(~na_item, scales = "free") +
  #
  #   labs(x = NULL, y = NULL) +
  #
  #   scale_y_continuous(labels = scales::comma) +
  #   scale_colour_viridis_d() +
  #
  #   theme_minimal() +
  #   theme(legend.position = "none",
  #         panel.grid.major.x = element_blank(),
  #         panel.grid.minor.x = element_blank(),
  #         panel.grid.minor.y = element_blank()) -> initial_plot


  x$forecast %>%
    dplyr::select(central.estimate) %>%
    unnest(central.estimate) %>%
    tidyr::pivot_longer(-c(time)) %>%
    drop_na %>%
    tidyr::pivot_wider(id_cols = c(time), names_from = name, values_from = value) -> central_forecasts

  central_forecasts %>%
    names %>%
    str_detect(., "^ln.") -> to_exponentiate

  central_forecasts %>%
    dplyr::mutate(across(.cols = dplyr::all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
    rename_with(.fn = ~gsub("ln.","",.)) %>%

    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") %>%
    dplyr::full_join(x$orig_model$module_order_eurostatvars %>%
                dplyr::select(dependent) %>%
                rename(na_item = dependent), by = "na_item") %>%

    dplyr::mutate(fit = "forecast") -> forecasts_processed

  plot_df %>%
    dplyr::filter(fit == "TRUE") %>%
    dplyr::group_by(na_item) %>%
    dplyr::filter(time == max(time)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-var) %>%
    dplyr::mutate(fit = "forecast") -> last_hist_value

  dplyr::bind_rows(forecasts_processed, last_hist_value) %>%
    dplyr::bind_rows(plot_df) %>%

    {if(order.as.run){
      dplyr::mutate(.,na_item = factor(na_item, levels = x$orig_model$module_order_eurostatvars$dependent)) %>%
        tidyr::drop_na(na_item) %>%
        arrange(time, na_item)} else {.}} %>%

    ggplot(aes(x = time, y = values, color = fit)) +
    geom_line(size = 1) +

    facet_wrap(~na_item, scales = "free") +

    labs(x = NULL, y = NULL) +

    scale_y_continuous(labels = scales::comma) +
    scale_colour_viridis_d() +

    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())










}
