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
#'
#' @return A tibble with the fitted values as one column.
#'
#'
#' @export
#'
#' @examples
#' sample_data <- dplyr::tibble(
#'   time = rep(seq.Date(
#'     from = as.Date("2000-01-01"),
#'     to = as.Date("2000-12-31"), by = 1
#'   ), each = 2),
#'   na_item = rep(c("yvar", "xvar"), 366), values = rnorm(366 * 2, mean = 100)
#' )
#' sample_data_clean <- clean_data(sample_data, max.lag = 4)
#' estimation <- estimate_module(sample_data_clean, "yvar", "xvar")
#' add_to_original_data(sample_data_clean, estimation$best_model, dep_var_basename = "yvar")
add_to_original_data <- function(clean_data,
                                 isat_object,
                                 dep_var_basename = "imports_of_goods_and_services",
                                 ardl_or_ecm = "ardl") {
  if (!"index" %in% names(clean_data)) {
    stop("Clean Data Object should have an index i.e. a 1:nrow(clean_data) column that allows us to join the estimated data again with model$aux$y.index.")
  }

  clean_data %>%
    dplyr::full_join(dplyr::tibble(
      index = isat_object$aux$y.index,
      fitted = as.numeric(isat_object$mean.fit)
    ), by = "index") %>%
    {
      if (ardl_or_ecm == "ecm") {
        dplyr::mutate(.,
          fitted.cumsum = dplyr::case_when(
            is.na(dplyr::all_of("fitted")) & is.na(lead(dplyr::all_of("fitted"))) ~ 0,
            # ATTENTION TO DO: here change by Moritz: used to be paste0("L.",dep_var_basename)
            is.na(dplyr::all_of("fitted")) & !is.na(lead(dplyr::all_of("fitted"))) ~ get(paste0("ln.", dep_var_basename)), # L.imports_of_goods_and_services,
            !is.na(dplyr::all_of("fitted")) ~ dplyr::all_of("fitted")
          ),
          fitted.cumsum = cumsum(dplyr::all_of("fitted.cumsum")),
          fitted.cumsum = ifelse(is.na(dplyr::all_of("fitted")), NA, dplyr::all_of("fitted.cumsum"))
        )
      } else {
        .
      }
    } %>%
    {
      if (ardl_or_ecm == "ecm") {
        dplyr::mutate(., fitted.level = exp(fitted.cumsum))
      } else if (ardl_or_ecm == "ardl") {
        dplyr::mutate(., fitted.level = exp(fitted))
      } else {
        .
      }
    } -> intermed

  # intermed %>% ggplot(aes(x = as.Date(time))) + geom_line(aes(y = fitted.level), col = "blue") + geom_line(aes(y = p5g))

  # Jonas: the code below gives me an error, including for the little example in the documentation
  # the reason is that the renaming then does not yield unique column names ("fitted" becomes dep_var_basename but it exists already)
  # replace by following suggestion: TO DO
  # Update Moritz 29/08/2022: does not give me an error - also the example in the documentation works

  intermed %>%
    dplyr::rename_with(.cols = dplyr::any_of(c("fitted", "fitted.level", "fitted.cumsum")), .fn = ~ paste0(gsub("fitted", dep_var_basename, .), ".hat")) %>%
    return()

}
