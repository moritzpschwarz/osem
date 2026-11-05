#' Prepare the clean dataframe for modelling
#'
#' @param raw_data A tibble or data.frame with the y variable and the x
#'   variables. Needs to have a column called 'time', which is of class
#'   \code{\link[base:Dates]{Date}}. Variable names need to be in column
#'   'na_item', and values in column 'values'.
#' @param max.ar Integer. The maximum number of lags to use for the AR terms. as well as for the independent variables.
#' @param max.dl Integer. The maximum number of lags to use for the independent variables (the distributed lags).
#' @param trend Logical. Should a trend be added? Default is TRUE.
#' @param opts_df Internal object containing detailed options and information on individual modules.
#' @inheritParams run_module
#' @inheritParams run_model
#' @inheritParams estimate_module
#'
#' @return A tibble with the cleaned data.
#'
#'

clean_data <- function(raw_data,
                       max.ar = 4,
                       max.dl = 2,
                       trend = TRUE,
                       opts_df,
                       module,
                       use_logs){
  raw_data %>%
    dplyr::select("na_item", "time", "values") %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
    dplyr::arrange(.data$time) -> raw_data_processed

  depvars <- trimws(unlist(strsplit(module$dependent, ",")))
  variable_names <- setdiff(names(raw_data_processed), "time")

  available_transformations <- stats::setNames(
    vapply(
      raw_data_processed[variable_names],
      function(x) {
        if (any(x <= 0, na.rm = TRUE)) {
          return("asinh")
        } else {
          return("log")
        }
      },
      character(1)
    ),
    variable_names
  )

  transformed_vars <- switch(
    use_logs,
    none = character(0),
    y = intersect(variable_names, depvars),
    x = setdiff(variable_names, depvars),
    both = variable_names
  )

  log_opts_values <- stats::setNames(rep(NA_character_, length(variable_names)), variable_names)
  log_opts_values[transformed_vars] <- available_transformations[transformed_vars]
  model_transformations <- stats::setNames(rep("none", length(variable_names)), variable_names)
  model_transformations[transformed_vars] <- available_transformations[transformed_vars]

  log_opts_new <- dplyr::bind_cols(
    module,
    dplyr::tibble(log_opts = list(dplyr::as_tibble(as.list(log_opts_values))))
  )

  if (!"log_opts" %in% names(opts_df)) {
    opts_df <- opts_df %>% dplyr::mutate(log_opts = NA)
  }

  opts_df %>%
    dplyr::mutate(log_opts = dplyr::case_when(.data$index == module$index ~ log_opts_new$log_opts, TRUE ~ .data$log_opts)) -> opts_df

  # TODO: this seems to be done always, even when not use_logs == "both"; wasteful
  intermed <- raw_data_processed
  for (variable in variable_names) {
    intermed[[paste0("ln.", variable)]] <- transform_osem_values(
      intermed[[variable]],
      available_transformations[[variable]]
    )
  }

  intermed <- intermed %>%
    dplyr::mutate(
      dplyr::across(-"time", list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
    )

  to_be_added <- dplyr::tibble(.rows = nrow(intermed))
  # TODO: this could be skipped for CVAR because functions create lags/FD directly
  for (i in 1:max(max.ar, max.dl)){
    intermed %>%
      dplyr::mutate(dplyr::across(-"time", ~ dplyr::lag(., n = i), .names = paste0("L", i, ".{.col}")), .keep = "none") %>% # dplyr::mutate(dplyr::across(c(dplyr::starts_with("D."), dplyr::starts_with("ln.")), ~ dplyr::lag(., n = i))) %>%
      # dplyr::select(c(dplyr::starts_with("D."), dplyr::starts_with("ln."))) %>%
      # dplyr::rename_with(.fn = ~ paste0("L", i, ".", .)) %>%
      dplyr::bind_cols(to_be_added, .) -> to_be_added
  }

  intermed %>%
    dplyr::bind_cols(to_be_added) %>%
    dplyr::mutate(index = 1:dplyr::n()) %>%
    dplyr::relocate("index") %>%
    dplyr::mutate(q = lubridate::quarter(.data$time, with_year = FALSE)) %>%
    fastDummies::dummy_cols(
      select_columns = "q", remove_first_dummy = TRUE,
      remove_selected_columns = TRUE
    ) %>%
    {
      if (trend) {
        dplyr::mutate(., trend = as.numeric(as.factor(.data$time)), .after = "time")
      } else {
        .
      }
    } -> cleaned_data

  out <- list()
  out$df <- cleaned_data
  out$opts_df <- opts_df
  out$transformations <- model_transformations

  return(out)
}
