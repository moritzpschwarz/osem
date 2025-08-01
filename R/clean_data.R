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
                       use_logs) {
  raw_data %>%
    dplyr::select("na_item", "time", "values") %>%
    tidyr::pivot_wider(id_cols = "time", names_from = "na_item", values_from = "values") %>%
    dplyr::arrange(.data$time) -> raw_data_processed

  # save the relevant information whether Log or asinh transformation is needed in opts_df
  if (use_logs == "none") {
    raw_data_processed %>%
      dplyr::mutate(dplyr::across(-"time", ~NA)) %>%
      dplyr::select(-"time") %>%
      dplyr::distinct() %>%
      tidyr::nest() %>%
      setNames("log_opts") %>%
      dplyr::bind_cols(module, .) -> log_opts_new
  }

  if (use_logs == "y") {
    raw_data_processed %>%
      dplyr::mutate(dplyr::across(-"time", ~ dplyr::case_when(any(. <= 0, na.rm = TRUE) ~ "asinh", TRUE ~ "log"))) %>%
      dplyr::mutate(dplyr::across(-dplyr::all_of(module$dependent), ~NA)) %>%
      dplyr::select(-"time") %>%
      dplyr::distinct() %>%
      tidyr::nest() %>%
      setNames("log_opts") %>%
      dplyr::bind_cols(module, .) -> log_opts_new
  }

  if (use_logs == "x") {
    raw_data_processed %>%
      dplyr::mutate(dplyr::across(-"time", ~ dplyr::case_when(any(. <= 0, na.rm = TRUE) ~ "asinh", TRUE ~ "log"))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(module$dependent), ~NA)) %>%
      dplyr::select(-"time") %>%
      dplyr::distinct() %>%
      tidyr::nest() %>%
      setNames("log_opts") %>%
      dplyr::bind_cols(module, .) -> log_opts_new
  }

  if (use_logs == "both") {
    raw_data_processed %>%
      dplyr::mutate(dplyr::across(-"time", ~ dplyr::case_when(any(. <= 0, na.rm = TRUE) ~ "asinh", TRUE ~ "log"))) %>%
      dplyr::select(-"time") %>%
      dplyr::distinct() %>%
      tidyr::nest() %>%
      setNames("log_opts") %>%
      dplyr::bind_cols(module, .) -> log_opts_new
  }

  if (!"log_opts" %in% names(opts_df)) {
    opts_df <- opts_df %>% dplyr::mutate(log_opts = NA)
  }

  opts_df %>%
    dplyr::mutate(log_opts = dplyr::case_when(.data$index == module$index ~ log_opts_new$log_opts, TRUE ~ .data$log_opts)) -> opts_df

  raw_data_processed %>%
    dplyr::mutate(
      dplyr::across(-"time", .fns = ~ if (any(. <= 0, na.rm = TRUE)) {
        asinh(.)
      } else {
        log(.)
      }, .names = "ln.{.col}"),
      dplyr::across(-"time", list(D = ~ c(NA, diff(., ))), .names = "{.fn}.{.col}")
    ) -> intermed

  to_be_added <- dplyr::tibble(.rows = nrow(intermed))
  for (i in 1:max(max.ar, max.dl)) {
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

  return(out)
}
