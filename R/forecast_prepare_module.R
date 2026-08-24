#' Re-estimate a module when its fixed log recipe is outside its domain
#'
#' A forecast never silently changes a fitted transformation. If a raw future
#' regressor required by a log specification is non-positive, only the affected
#' module is re-estimated locally with that regressor fixed to asinh.
#'
#' @keywords internal
forecast_prepare_module <- function(model,
                                    i,
                                    exog_df_ready_full,
                                    prediction_list) {
  module_row <- model$module_collection %>%
    dplyr::filter(.data$order == i)
  recipe <- module_row$model.args[[1]]$forecast_recipe

  log_regressors <- names(recipe$transformations)[
    recipe$transformations == "log" &
      names(recipe$transformations) %in% recipe$regressors
  ]
  if (length(log_regressors) == 0L) {
    return(list(model = model, reestimation = NULL))
  }

  invalid <- character(0)
  for (variable in log_regressors) {
    raw_future <- numeric(0)

    if (variable %in% names(exog_df_ready_full)) {
      raw_future <- c(raw_future, exog_df_ready_full[[variable]])
    }
    if (variable %in% prediction_list$dep_var) {
      upstream_prediction <- prediction_list %>%
        dplyr::filter(.data$dep_var == variable) %>%
        dplyr::slice_tail(n = 1L)

      if (NROW(upstream_prediction) > 0L &&
          !is.null(upstream_prediction$central.estimate[[1]]) &&
          is.data.frame(upstream_prediction$central.estimate[[1]])) {
        upstream_index <- upstream_prediction$index[[1]]
        upstream_recipe <- model$module_collection %>%
          dplyr::filter(.data$index == upstream_index) %>%
          dplyr::pull(.data$model.args) %>%
          .[[1]] %>%
          .$forecast_recipe

        raw_future <- c(
          raw_future,
          upstream_prediction$central.estimate[[1]] %>%
            dplyr::select(-"time") %>%
            dplyr::pull(1) %>%
            inverse_transform_osem_values(
              upstream_recipe$dependent_transformation
            )
        )

        upstream_draws <- upstream_prediction$all.estimates[[1]]
        if (!is.null(upstream_draws) && is.data.frame(upstream_draws)) {
          raw_future <- c(
            raw_future,
            upstream_draws %>%
              dplyr::select(-dplyr::any_of("time")) %>%
              dplyr::mutate(
                dplyr::across(
                  dplyr::everything(),
                  ~ inverse_transform_osem_values(
                    ., upstream_recipe$dependent_transformation
                  )
                )
              ) %>%
              unlist(use.names = FALSE)
          )
        }
      }
    }

    if (any(raw_future <= 0, na.rm = TRUE)) {
      invalid <- c(invalid, variable)
    }
  }

  invalid <- unique(invalid)
  if (length(invalid) == 0L) {
    return(list(model = model, reestimation = NULL))
  }

  value_or <- function(value, fallback) {
    if (is.null(value)) fallback else value
  }
  overrides <- stats::setNames(rep("asinh", length(invalid)), invalid)
  module <- model$module_order %>% dplyr::filter(.data$order == i)
  classification <- classify_variables(specification = model$module_order)

  reestimated <- run_module(
    module = module,
    data = model$full_data,
    classification = classification,
    use_logs = value_or(model$args$use_logs, "both"),
    trend = value_or(model$args$trend, TRUE),
    ardl_or_ecm = value_or(
      module_row$model.args[[1]]$ardl_or_ecm_requested,
      value_or(model$args$ardl_or_ecm, "ardl")
    ),
    ecm_pretest = value_or(model$args$ecm_pretest, "auto"),
    max.ar = value_or(model$args$max.ar, 4),
    max.dl = value_or(model$args$max.dl, 2),
    saturation = model$args$saturation,
    saturation.tpval = value_or(model$args$saturation.tpval, 0.01),
    max.block.size = value_or(model$args$max.block.size, 20),
    gets_selection = value_or(model$args$gets_selection, TRUE),
    selection.tpval = value_or(model$args$selection.tpval, 0.01),
    opts_df = model$opts_df,
    keep = model$args$keep,
    pretest_steps = value_or(model$args$pretest_steps, FALSE),
    quiet = TRUE,
    cvar.ar = value_or(model$args$cvar.ar, 4),
    freq = NULL,
    coint_deterministic = value_or(model$args$coint_deterministic, "const"),
    coint_significance = value_or(model$args$coint_significance, "5pct"),
    indicator_compression = value_or(model$args$indicator_compression, TRUE),
    transformation_overrides = overrides
  )

  row <- model$module_collection$order == i
  model$module_collection[row, "dataset"] <- dplyr::tibble(
    dataset = list(reestimated$data)
  )
  model$module_collection[row, "model"] <- dplyr::tibble(
    model = list(reestimated$model)
  )
  model$module_collection[row, "model.args"] <- dplyr::tibble(
    model.args = list(reestimated$args)
  )
  model$module_collection[row, "indep"] <- dplyr::tibble(
    indep = list(reestimated$indep)
  )
  model$module_collection[row, "dep"] <- dplyr::tibble(
    dep = list(reestimated$dep)
  )
  model$module_collection[row, "diagnostics"] <- dplyr::tibble(
    diagnostics = list(reestimated$diagnostics)
  )
  model$opts_df <- reestimated$opts_df

  list(
    model = model,
    reestimation = list(
      module = module$dependent[[1]],
      reason = "non-positive future value outside the stored log domain",
      variables = invalid,
      transformations = overrides,
      forecast_local = TRUE
    )
  )
}
