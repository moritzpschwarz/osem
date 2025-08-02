#' Add the estimated fitted values back to the original
#'
#' @param clean_data An input data.frame or tibble. Must be the output of
#'   clean_data() to fit all requirements.
#' @param model_object Most likely should be returned by the 'estimate_module()'
#'  function.
#' @param dep_var_basename A character string of the name of the dependent
#'   variable as contained in clean_data() in a level form (i.e. no ln or D in
#'   front of the name).
#' @param model_type Either 'ardl', 'ecm', or 'cvar'.
#' @param opts_df Internal object containing detailed options and information on individual modules.
#' @inheritParams run_module
#' @return A tibble with the fitted values as one column.
#'
#'
add_to_original_data <- function(clean_data,
                                 model_object,
                                 dep_var_basename = "imports_of_goods_and_services",
                                 model_type = "ardl",
                                 opts_df,
                                 module) {
  if (!"index" %in% names(clean_data)) {
    stop("Clean Data Object should have an index i.e. a 1:nrow(clean_data) column that allows us to join the estimated data again with model$aux$y.index.")
  }
  if (model_type %in% c("ardl", "ecm")) {
    clean_data %>%
      dplyr::full_join(dplyr::tibble(
        time = model_object$aux$y.index,
        fitted = as.numeric(model_object$mean.fit)
      ), by = "time") -> intermed_init

    opts_df %>%
      dplyr::filter(.data$index == module$index) %>%
      dplyr::pull("log_opts") %>%
      dplyr::first() %>%
      dplyr::select(dplyr::all_of(module$dependent)) %>%
      dplyr::pull() -> dependent_log_opts

    if (model_type == "ecm") {
      dplyr::mutate(intermed_init,
        fitted.cumsum = dplyr::case_when(
          is.na(.data$fitted) & is.na(dplyr::lead(.data$fitted)) ~ 0,
          is.na(.data$fitted) & !is.na(dplyr::lead(.data$fitted)) ~ get(paste0("ln.", dep_var_basename)), # L.imports_of_goods_and_services,
          !is.na(.data$fitted) ~ .data$fitted
        ),
        fitted.cumsum = cumsum(.data$fitted.cumsum),
        fitted.cumsum = ifelse(is.na(.data$fitted.cumsum), NA, .data$fitted.cumsum)
      ) -> intermed_ecm

      fitted_vals <- fitted_vals <- dplyr::case_when(
        is.na(dependent_log_opts) ~ intermed_ecm$fitted.cumsum,
        dependent_log_opts == "log" ~ exp(intermed_ecm$fitted.cumsum),
        dependent_log_opts == "asinh" ~ sinh(intermed_ecm$fitted.cumsum)
      )
    }

    if (model_type == "ardl") {
      fitted_vals <- dplyr::case_when(
        is.na(dependent_log_opts) ~ intermed_init$fitted,
        dependent_log_opts == "log" ~ exp(intermed_init$fitted),
        dependent_log_opts == "asinh" ~ sinh(intermed_init$fitted)
      )
    }

    intermed_init %>%
      dplyr::mutate(fitted.level = fitted_vals) -> intermed

    out <- intermed %>%
      dplyr::rename_with(
        .cols = dplyr::any_of(c("fitted", "fitted.level", "fitted.cumsum")),
        .fn = ~ paste0(gsub("fitted", dep_var_basename, .), ".hat")
      )
  } else if (identical(model_type, "cvar")) {
    # extract whether dependent variable was transformed
    dependent_log_opts <- opts_df %>%
      dplyr::filter(.data$index == module$index) %>%
      dplyr::pull("log_opts") %>%
      dplyr::first() %>%
      dplyr::select(dplyr::all_of(dep_var_basename)) %>%
      tidyr::pivot_longer(cols = dplyr::everything(), names_to = "na_item", values_to = "transformation")

    # for CVAR, have to add multiple fitted values because have multiple depvars
    cvar_fitted <- fitted(model_object$varm) %>%
      as.data.frame(.data) %>%
      # remove the "fit of " in varname, add ".hat" at end
      dplyr::rename_with(~ gsub("fit of (ln\\.)?", "", .x)) %>%
      # add index for merging with full data later; lose first K=ar observations
      dplyr::mutate(index = (model_object$args$ar + 1):NROW(clean_data))
    # should now correspond to "basename", add failsafe:
    if (!setdiff(colnames(cvar_fitted), dep_var_basename) == "index") {
      stop(paste0("Problem in module ", module$order, ". Computation of fitted values failed. Debug at add_to_original_data()."))
    }

    # transform fitted values to levels
    cvar_fitted <- cvar_fitted %>%
      tidyr::pivot_longer(
        cols = !dplyr::all_of("index"),
        names_to = "na_item", values_to = "values"
      ) %>%
      dplyr::left_join(dependent_log_opts, by = "na_item") %>%
      dplyr::mutate(level.values = dplyr::case_when(
        .data$transformation == "log" ~ exp(values),
        .data$transformation == "asinh" ~ sinh(values),
        is.na(.data$transformation) ~ values
      )) %>%
      dplyr::select(-"transformation") %>%
      tidyr::pivot_wider(
        id_cols = "index", names_from = "na_item",
        values_from = c("values", "level.values"), names_glue = "{na_item}.{.value}"
      ) %>%
      dplyr::rename_with(~ sub("\\.values$", ".hat", .x), tidyselect::ends_with(".values"))

    # add fitted values to original data
    out <- clean_data %>%
      dplyr::full_join(cvar_fitted, by = "index")
  } else {
    stop("Argument model_type not recognised.")
  }

  # intermed %>% ggplot2::ggplot(ggplot2::aes(x = as.Date(time))) + ggplot2::geom_line(ggplot2::aes(y = fitted.level), col = "blue") + ggplot2::geom_line(ggplot2::aes(y = p5g))

  # Jonas: the code below gives me an error, including for the little example in the documentation
  # the reason is that the renaming then does not yield unique column names ("fitted" becomes dep_var_basename but it exists already)
  # replace by following suggestion: TO DO
  # Update Moritz 29/08/2022: does not give me an error - also the example in the documentation works

  return(out)
}
