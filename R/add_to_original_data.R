#' Add the estimated fitted values back to the original
#'
#' @param clean_data An input data.frame or tibble. Must be the output of
#'   clean_data() to fit all requirements.
#' @param isat_object An object of class 'isat'. Most likely should be the
#'   'best_model' element that is returned by the 'estimate_module()' function.
#' @param dep_var_basename A character string of the name of the dependent
#'   variable as contained in clean_data() in a level form (i.e. no ln or D in
#'   front of the name).
#' @param ardl_or_ecm Either 'ardl' or 'ecm' to determine whether to estimate
#'   the model as an Autoregressive Distributed Lag Function (ardl) or as an
#'   Equilibrium Correction Model (ecm).
#' @param opts_df Internal object containing detailed options and information on individual modules.
#' @inheritParams run_module
#' @return A tibble with the fitted values as one column.
#'
#'
add_to_original_data <- function(clean_data,
                                 isat_object,
                                 dep_var_basename = "imports_of_goods_and_services",
                                 ardl_or_ecm = "ardl",
                                 opts_df,
                                 module) {
  if (!"index" %in% names(clean_data)) {
    stop("Clean Data Object should have an index i.e. a 1:nrow(clean_data) column that allows us to join the estimated data again with model$aux$y.index.")
  }

  clean_data %>%
    dplyr::full_join(dplyr::tibble(
      time = isat_object$aux$y.index,
      fitted = as.numeric(isat_object$mean.fit)
    ), by = "time") -> intermed_init

  opts_df %>%
    dplyr::filter(.data$index == module$index) %>%
    dplyr::pull("log_opts") %>%
    dplyr::first() %>%
    dplyr::select(dplyr::all_of(module$dependent)) %>%
    dplyr::pull() -> dependent_log_opts

  if (ardl_or_ecm == "ecm") {
    dplyr::mutate(intermed_init,
                  fitted.cumsum = dplyr::case_when(
                    is.na(.data$fitted) & is.na(dplyr::lead(.data$fitted)) ~ 0,

                    is.na(.data$fitted) & !is.na(dplyr::lead(.data$fitted)) ~ get(paste0("ln.", dep_var_basename)), # L.imports_of_goods_and_services,
                    !is.na(.data$fitted) ~ .data$fitted
                  ),
                  fitted.cumsum = cumsum(.data$fitted.cumsum),
                  fitted.cumsum = ifelse(is.na(.data$fitted.cumsum), NA, .data$fitted.cumsum)) -> intermed_ecm

    fitted_vals <- fitted_vals <- dplyr::case_when(is.na(dependent_log_opts) ~ intermed_ecm$fitted.cumsum,
                                                   dependent_log_opts == "log" ~ exp(intermed_ecm$fitted.cumsum),
                                                   dependent_log_opts == "asinh" ~ sinh(intermed_ecm$fitted.cumsum))
  }

  if (ardl_or_ecm == "ardl") {

    fitted_vals <- dplyr::case_when(is.na(dependent_log_opts) ~ intermed_init$fitted,
                                    dependent_log_opts == "log" ~ exp(intermed_init$fitted),
                                    dependent_log_opts == "asinh" ~ sinh(intermed_init$fitted))

  }

  intermed_init %>%
    dplyr::mutate(fitted.level = fitted_vals) -> intermed

  # intermed %>% ggplot2::ggplot(ggplot2::aes(x = as.Date(time))) + ggplot2::geom_line(ggplot2::aes(y = fitted.level), col = "blue") + ggplot2::geom_line(ggplot2::aes(y = p5g))

  # Jonas: the code below gives me an error, including for the little example in the documentation
  # the reason is that the renaming then does not yield unique column names ("fitted" becomes dep_var_basename but it exists already)
  # replace by following suggestion: TO DO
  # Update Moritz 29/08/2022: does not give me an error - also the example in the documentation works

  intermed %>%
    dplyr::rename_with(.cols = dplyr::any_of(c("fitted", "fitted.level", "fitted.cumsum")),
                       .fn = ~ paste0(gsub("fitted", dep_var_basename, .), ".hat")) %>%
    return()

}
