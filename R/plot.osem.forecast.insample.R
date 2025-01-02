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

  share_to_show_hist <- 1 - (ifelse((1 - (x$args$sample_share - 0.1)) * 2 <= 1, (1 - (x$args$sample_share -  0.1)) * 2, 1))
  time_to_show_hist <- x$args$all_times[ceiling(length(x$args$all_times) * share_to_show_hist):length(x$args$all_times)]

  extract_dep_vars <- unique(x$args$dep_vars)

  historical_data <- x$args$model$processed_input_data %>%
    dplyr::filter(.data$na_item %in% extract_dep_vars,
                  .data$time %in% time_to_show_hist) %>%
    dplyr::rename(dep_var = "na_item")

  centrals <- x$central
  if(!is.null(x$uncertainty)){uncertainties <- x$uncertainty}

  for(i in unique(x$central$start)){
    # i = unique(x$central$start)[1]
    hist_intermed <- historical_data %>%
      dplyr::arrange(.data$dep_var, .data$time) %>%
      dplyr::mutate(n = 1:dplyr::n(), .by = "dep_var")

    rank_of_i <- hist_intermed %>%
      dplyr::filter(.data$time == i) %>%
      dplyr::distinct(.data$n) %>%
      dplyr::pull(.data$n)

    hist_intermed %>%
      dplyr::filter(.data$n == rank_of_i) %>%
      dplyr::select(-"n") %>%
      dplyr::mutate(start = as.Date(i)) %>%
      dplyr::cross_join(dplyr::tibble(method = unique(centrals$method))) %>%
      dplyr::bind_rows(centrals,.) -> centrals

    if(!is.null(x$uncertainty)){
      hist_intermed %>%
        dplyr::filter(.data$n == rank_of_i) %>%
        dplyr::select(-"n") %>%
        dplyr::mutate(min = .data$values,
                      max = .data$values,
                      p975 = .data$values,
                      p025 = .data$values,
                      p75 = .data$values,
                      p25 = .data$values) %>%
        dplyr::mutate(start = as.Date(i)) %>%
        dplyr::cross_join(dplyr::tibble(method = unique(centrals$method))) %>%
        dplyr::bind_rows(uncertainties,.) -> uncertainties
    }
  }

  if(!is.null(x$uncertainty)){
    uncertainty_layers <- list(ggplot2::geom_ribbon(data = uncertainties, ggplot2::aes(ymin = .data$min, x = .data$time, ymax = .data$max, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE),
                               ggplot2::geom_ribbon(data = uncertainties, ggplot2::aes(ymin = .data$p025, x = .data$time, ymax = .data$p975, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE),
                               ggplot2::geom_ribbon(data = uncertainties, ggplot2::aes(ymin = .data$p25, x = .data$time, ymax = .data$p75, fill = as.factor(.data$start)), linewidth = 0.1, alpha = 0.1, inherit.aes = FALSE))
  } else {
    uncertainty_layers <- NULL
  }

  legend_options <- list(
    ggplot2::theme(legend.position = dplyr::if_else(length(unique(centrals$method))>1, "bottom","none"))
  )

  centrals <- centrals %>% dplyr::rename(Method = "method")
  if(exists("uncertainties")){uncertainties <- uncertainties %>% dplyr::rename(Method = "method")}

  ggplot2::ggplot() +
    ggplot2::geom_line(data = historical_data,
                       ggplot2::aes(x = .data$time, y = .data$values), linewidth = 1) +

    ggplot2::facet_wrap(~.data$dep_var, scales = "free") +

    uncertainty_layers +

    ggplot2::geom_line(data = centrals, ggplot2::aes(y = .data$values,
                                                     x = .data$time,
                                                     linetype = .data$Method,
                                                     color = as.factor(.data$start)),
                       inherit.aes = FALSE) +
    ggplot2::facet_wrap(~.data$dep_var, scales = "free") +
    #ggplot2::scale_color_brewer(palette = "PRGn") +
    ggplot2::scale_colour_viridis_d() +
    ggplot2::coord_cartesian(expand = TRUE) +

    ggplot2::labs(x = NULL, y = NULL, title = title) +
    ggplot2::theme_minimal() +

    legend_options +

    ggplot2::guides(color = "none", fill = "none") + # remove legend for color and fill

    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> plt

  suppressMessages(suppressWarnings(print(plt)))
  #plotly::ggplotly(plt)

}
