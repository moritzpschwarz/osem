#' Plot an Insample-Forecast of the OSEM Model
#'
#' @param x An object of class osem.forecast.insample, which is the output from the \link{forecast_insample} function.
#' @inheritParams plot.osem.forecast
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
#'\donttest{
#' a <- run_model(specification = spec, dictionary = NULL,
#' inputdata_directory = NULL, primary_source = "download",
#' save_to_disk = NULL, present = FALSE)
#' plot(forecast_model(a))
#'}
#'

plot.osem.forecast.insample <- function(x, title = "OSEM Insample Hindcasts",
                                        #exclude.exogenous = TRUE, order.as.run = FALSE, interactive = FALSE, first_date = NULL, grepl_variables = NULL, return.data = FALSE,
                                        ...){

  if(!isa(x, "osem.forecast.insample")){
    stop("Input object not of type 'osem.forecast.insample'. Run 'forecast_insample' again and use the output of that function.")
  }

  # With those times, we can now find the share to forecast
  share_to_show <- 1 - (ifelse((1 - x$args$sample_share) * 2 <= 1, (1 - x$args$sample_share) * 2, 1))
  time_to_show <- x$args$all_times[ceiling(length(x$args$all_times) * share_to_show):length(x$args$all_times)]

  extract_dep_vars <- unique(x$args$dep_vars)

  if(!is.null(x$uncertainty)){
    uncertainty_layers <- list(ggplot2::geom_ribbon(data = x$uncertainty, ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE),
                               ggplot2::geom_ribbon(data = x$uncertainty, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE),
                               ggplot2::geom_ribbon(data = x$uncertainty, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE))
  } else {
    uncertainty_layers <- NULL
  }



  ggplot2::ggplot() +
    ggplot2::geom_line(data = x$hist_data %>%
                         dplyr::filter(.data$dep_var %in% extract_dep_vars,
                                       .data$time %in% time_to_show),
                       #.data$time > as.Date("2010-01-01")),
                       ggplot2::aes(x = .data$time, y = .data$values), linewidth = 1) +

    ggplot2::facet_wrap(~dep_var, scales = "free") +

    uncertainty_layers +

    ggplot2::geom_line(data = x$central, ggplot2::aes(y = .data$values, x = .data$time, color = as.factor(.data$start)), inherit.aes = FALSE) +
    ggplot2::facet_wrap(~.data$dep_var, scales = "free") +
    #ggplot2::scale_color_brewer(palette = "PRGn") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_cartesian(expand = TRUE) +

    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> plt


  suppressMessages(suppressWarnings(print(plt)))
  #plotly::ggplotly(plt)

}
