#' Compile the information required to forecast an estimated OSEM module
#'
#' @keywords internal
compile_forecast_recipe <- function(model_object,
                                    model_form,
                                    dep_var_basename,
                                    x_vars_basename,
                                    use_logs,
                                    transformations,
                                    term_specs,
                                    lag_only_vars = character(0)) {
  selected_terms <- names(stats::coef(model_object))
  if (is.null(selected_terms)) {
    selected_terms <- row.names(model_object$mean.results)
  }

  term_specs <- term_specs %>%
    dplyr::distinct(.data$term, .keep_all = TRUE) %>%
    dplyr::filter(.data$term %in% selected_terms)

  missing_terms <- setdiff(selected_terms, term_specs$term)
  missing_spec <- dplyr::tibble(
    term = missing_terms,
    role = "fixed_regressor",
    source = NA_character_,
    transformation = "none",
    difference = 0L,
    lag = 0L
  )

  if (length(missing_terms) > 0) {
    missing_spec$role[missing_terms == "mconst"] <- "constant"
    missing_spec$role[missing_terms == "trend"] <- "trend"
    missing_spec$role[grepl("^q_[0-9]+$", missing_terms)] <- "seasonal"
    missing_spec$role[grepl("^(iis|sis|tis)", missing_terms)] <- "indicator"

    ar_terms <- grepl("^ar[0-9]+$", missing_terms)
    missing_spec$role[ar_terms] <- "response_lag"
    missing_spec$source[ar_terms] <- dep_var_basename
    missing_spec$lag[ar_terms] <- as.integer(sub("^ar", "", missing_terms[ar_terms]))
  }

  term_specs <- dplyr::bind_rows(term_specs, missing_spec)
  coefficients <- stats::coef(model_object)
  term_specs$coefficient <- as.numeric(coefficients[term_specs$term])

  dependent_transformation <- if (
    !is.null(transformations) &&
    dep_var_basename %in% names(transformations) &&
    use_logs %in% c("both", "y")
  ) {
    unname(transformations[[dep_var_basename]])
  } else {
    "none"
  }

  list(
    version = 1L,
    model_form = model_form,
    response_scale = if (model_form == "ardl") "level" else "difference",
    dependent = dep_var_basename,
    regressors = x_vars_basename,
    transformations = transformations,
    dependent_transformation = dependent_transformation,
    response_name = model_object$aux$y.name,
    transformed_level_name = paste0(
      if (dependent_transformation %in% c("log", "asinh")) "ln." else "",
      dep_var_basename
    ),
    lag_only_vars = lag_only_vars,
    selected_terms = term_specs,
    mXnames = model_object$aux$mXnames,
    ar_lags = sort(term_specs$lag[term_specs$role == "response_lag"])
  )
}
