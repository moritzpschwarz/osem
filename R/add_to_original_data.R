#' Add the estimated fitted values back to the original
#'
#' @param clean_data An input data.frame or tibble. Must be the output of
#'   clean_data() to fit all requirements.
#' @param model_object Most likely should be returned by the 'estimate_module()'
#'  function.
#' @param dep_var_basename A character string of the name of the dependent
#'   variable as contained in clean_data() in a level form (i.e. no ln or D in
#'   front of the name).
#' @param model_type Either 'ardl', 'ecm', 'diff', or 'cvar' to determine whether
#'   the model was estimated as an Autoregressive Distributed Lag model, an
#'   Equilibrium Correction Model, a fully differenced model, or as a cointegrated
#'   vector autoregression.
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

  if (model_type %in% c("ardl", "ecm", "diff")) {
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

    if (model_type %in% c("ecm", "diff")) {
      # ECM and fully differenced models are both estimated with a differenced
      # dependent variable, e.g. D.ln.y or D.y. To add fitted values back to the
      # original data, we need to cumulate fitted differences into fitted levels.
      #
      # For log variables:
      #   fitted ln(y_t) = actual ln(y_{t-1}) + fitted D.ln(y_t)
      #
      # For untransformed variables:
      #   fitted y_t = actual y_{t-1} + fitted D.y_t
      #
      # For asinh variables:
      #   fitted asinh(y_t) = actual asinh(y_{t-1}) + fitted D.asinh(y_t)
      #
      # The previous observed transformed value is used as the initial condition.

      transformed_dep_candidates <- if (is.na(dependent_log_opts)) {
        dep_var_basename
      } else if (dependent_log_opts == "log") {
        c(paste0("ln.", dep_var_basename), dep_var_basename)
      } else if (dependent_log_opts == "asinh") {
        c(paste0("asinh.", dep_var_basename), paste0("ln.", dep_var_basename), dep_var_basename)
      } else {
        dep_var_basename
      }

      transformed_dep_var <- transformed_dep_candidates[transformed_dep_candidates %in% names(intermed_init)][1]

      if (is.na(transformed_dep_var) || length(transformed_dep_var) == 0) {
        stop(paste0(
          "Could not find the transformed dependent variable needed to reconstruct fitted levels for ",
          dep_var_basename,
          ". Checked: ",
          paste(transformed_dep_candidates, collapse = ", "),
          "."
        ))
      }

      fitted_diff <- intermed_init$fitted
      actual_transformed <- intermed_init[[transformed_dep_var]]

      fitted_cumsum <- rep(NA_real_, length(fitted_diff))
      first_fit_pos <- which(!is.na(fitted_diff))[1]

      if (!is.na(first_fit_pos)) {
        seed_candidates <- which(seq_along(actual_transformed) < first_fit_pos & !is.na(actual_transformed))

        if (length(seed_candidates) > 0) {
          seed_pos <- max(seed_candidates)
          current_value <- actual_transformed[seed_pos]

          for (j in first_fit_pos:length(fitted_diff)) {
            if (!is.na(fitted_diff[j])) {
              current_value <- current_value + fitted_diff[j]
              fitted_cumsum[j] <- current_value
            } else {
              fitted_cumsum[j] <- NA_real_
            }
          }
        } else if (!is.na(actual_transformed[first_fit_pos]) && !is.na(fitted_diff[first_fit_pos])) {
          # Fallback if there is no previous observed value. This should rarely
          # be needed, because differenced models usually lose at least one
          # initial observation. It reconstructs the starting level from the
          # observed level and the fitted change at the first fitted period.
          current_value <- actual_transformed[first_fit_pos] - fitted_diff[first_fit_pos]

          for (j in first_fit_pos:length(fitted_diff)) {
            if (!is.na(fitted_diff[j])) {
              current_value <- current_value + fitted_diff[j]
              fitted_cumsum[j] <- current_value
            } else {
              fitted_cumsum[j] <- NA_real_
            }
          }
        }
      }

      intermed_init %>%
        dplyr::mutate(fitted.cumsum = fitted_cumsum) -> intermed_ecm

      fitted_vals <- if(is.na(dependent_log_opts)) {
        intermed_ecm$fitted.cumsum
      } else if(dependent_log_opts == "log"){
        exp(intermed_ecm$fitted.cumsum)
      } else if(dependent_log_opts == "asinh"){
        sinh(intermed_ecm$fitted.cumsum)
      } else {
        intermed_ecm$fitted.cumsum
      }
    }

    if (model_type == "ardl") {
      fitted_vals <- if(is.na(dependent_log_opts)) {
        intermed_init$fitted
      } else if(dependent_log_opts == "log"){
        exp(intermed_init$fitted)
      } else if(dependent_log_opts == "asinh"){
        sinh(intermed_init$fitted)
      } else {
        intermed_init$fitted
      }
    }

    intermed_init %>%
      dplyr::mutate(fitted.level = fitted_vals) -> intermed

    if (model_type %in% c("ecm", "diff")) {
      intermed <- intermed %>%
        dplyr::mutate(fitted.cumsum = fitted_cumsum)
    }

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
    # prepare index, lose first K=ar observations
    index_subset <- model_object$index[-(1:model_object$args$ar)]
    cvar_fitted <- fitted(model_object$varm) %>%
      as.data.frame(.data) %>%
      # remove the "fit of " in varname, add ".hat" at end
      dplyr::rename_with(~ gsub("fit of (ln\\.)?", "", .x)) %>%
      # add index for merging with full data later
      dplyr::mutate(index = index_subset)
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
      dplyr::rename_with(~ sub("\\.values$", ".hat", .x), dplyr::ends_with(".values"))

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
