#' Plot a Forecast Object of the Aggregate Model
#'
#' @param object An object of class aggmod.forecast, which is the output from the forecast_model function.
#'
#' @export
#'
#' @examples
#' spec <- tibble(
#' type = c(
#'   "d",
#'   "d",
#'   "n"
#' ),
#' dependent = c(
#'   "JL",
#'   "TOTS",
#'   "B"
#' ),
#' independent = c(
#'   "TOTS - CP - CO - J - A",
#'   "YF + B",
#'   "CP + J"
#' )
#' )
#'
#' fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
#' fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
#' filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)
#' \dontrun{
#' a <- run_model(specification = spec, dictionary = NULL, inputdata_directory = NULL, filter_list = filter_list, download = TRUE, save_to_disk = NULL, present = FALSE)
#' }
#' plot(forecast(a))
#'
plot.aggmod.forecast <- function(object){

  if(class(object) != "aggmod.forecast"){
    stop("Input object not of type aggmod.forecast. Run 'forecast_model' again and use the output of that function.")
  }

  object$orig_model$full_data %>%
    mutate(var = na_item,
           na_item = gsub("\\.hat","",na_item),
           fit = as.character(str_detect(var, ".hat"))) %>%

    left_join(dict %>%
                rename(na_item = eurostat_code) %>%
                select(na_item, model_varname), by = "na_item") -> plot_df

  plot_df %>%
    ggplot(aes(x = time, y = values, color = fit, group = var, fit = var)) +
    geom_line(size = 1) +

    facet_wrap(~model_varname, scales = "free") +

    labs(x = NULL, y = NULL) +

    scale_y_continuous(labels = scales::comma) +
    scale_colour_viridis_d() +

    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) -> initial_plot


  object$forecast %>%
    select(central.estimate) %>%
    unnest(central.estimate) %>%
    pivot_longer(-c(time)) %>%
    drop_na %>%
    pivot_wider(id_cols = c(time), names_from = name, values_from = value) -> central_forecasts

  central_forecasts %>%
    names %>%
    str_detect(., "^ln.") -> to_exponentiate

  central_forecasts %>%
    mutate(across(.cols = all_of(names(central_forecasts)[to_exponentiate]), exp)) %>%
    rename_with(.fn = ~gsub("ln.","",.)) %>%

    pivot_longer(-time, names_to = "na_item", values_to = "values") %>%
    full_join(object$orig_model$module_order_eurostatvars %>%
                select(dependent, dependent_eu) %>%
                mutate(dependent_eu = tolower(dependent_eu)) %>%
                rename(na_item = dependent_eu,
                       model_varname = dependent), by = "na_item") %>%

    mutate(fit = "forecast") %>%
    bind_rows(plot_df) %>%


    ggplot(aes(x = time, y = values, color = fit)) +
    geom_line(size = 1) +

    facet_wrap(~model_varname, scales = "free") +

    labs(x = NULL, y = NULL) +

    scale_y_continuous(labels = scales::comma) +
    scale_colour_viridis_d() +

    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())










}
