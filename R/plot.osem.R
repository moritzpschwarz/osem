#' Plot the estimated OSEM Model
#'
#' @param x An object of class osem, which is the output from the \link{run_model} function.
#' @param exclude.exogenous Logical. Should exogenous values be plotted? Default is FALSE.
#' @param order.as.run Logical. Should the plots be arranged in the way that the model was run? Default FALSE.
#' @param interactive Logical. Should the resulting plot be launched in an interactive way (the plotly package is required for this).
#' @param first_date Character. First date value to be shown. Must be a character value that can be turned into a date using as.Date() or NULL.
#' @param grepl_variables Regular Expression Character. Can be used to select variables to be plotted. Experimental feature so use with care.
#' @param return.data Logical. Do not return a plot but rather just the final dataset that has been created for the plot.
#' @param ... Additional arguments passed to the plotting function.
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
#' plot(a)
#'}
#' @export
plot.osem <- function(x, exclude.exogenous = TRUE, order.as.run = FALSE, interactive = FALSE, first_date = NULL, grepl_variables = NULL, return.data = FALSE, ...){

  if(!isa(x, "osem")){
    stop("Input object not of type osem. Run 'run_model' again and use the output of that function.")
  }

  if(!is.null(first_date)){if(!is.character(first_date) | !lubridate::is.Date(as.Date(first_date))){stop("When supplying 'first_date', the it must be a character and must be (able to be converted to) a Date.")}}

  x$full_data %>%
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

  plotting_df_ready <- plot_df %>%

    {if(order.as.run){
      dplyr::mutate(.,na_item = factor(.data$na_item, levels = x$module_order$dependent)) %>%
        tidyr::drop_na("na_item") %>%
        dplyr::arrange(.data$time, .data$na_item)} else {.}} %>%

    {if(!is.null(first_date)){dplyr::filter(., .data$time >= as.Date(first_date))} else {.}} %>%

    {if(!is.null(grepl_variables)){dplyr::filter(., grepl(grepl_variables,.data$na_item))} else {.}}

  plotting_df_ready %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$time, y = .data$values, color = .data$fit)) +

    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +

    # ggplot_options +

    ggplot2::facet_wrap(~.data$na_item, scales = "free") +

    ggplot2::labs(x = NULL, y = NULL) +

    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::scale_color_viridis_d() +

    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank()) -> p

  if(return.data){
    # prepare a nice dataset to be spit out
    plotting_df_ready %>%
      dplyr::select(-"var") %>%
      dplyr::mutate(fit = dplyr::case_when(fit == "forecast" ~ "Endogenous Forecast",
                                           fit == "TRUE" ~ "Insample Fit",
                                           fit == "FALSE" ~ "Observation",
                                           TRUE ~ fit)) %>%
      dplyr::rename(type = "fit") %>%
      dplyr::arrange(dplyr::desc(.data$time), .data$na_item, .data$type) %>%
      return()
  } else {
    if(interactive){
      plotly::ggplotly(p)
    } else {
      suppressWarnings(suppressMessages(print(p)))
    }
  }






}
