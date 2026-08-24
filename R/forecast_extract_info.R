#' Extract relevant information from the osem model object for forecasting and nowcasting
#' @inheritParams forecast_model
#' @inheritParams forecast_setup_estimated_relationships
#' @return The relevant information in a list format that enables forecasting and nowcasting
#'
forecast_extract_info <- function(model, i, n.ahead, exog_df_ready){

  module_row <- model$module_collection %>%
    dplyr::filter(.data$order == i)

  isat_obj <- module_row %>%
    dplyr::pull(.data$model) %>%
    .[[1]]

  if (!inherits(isat_obj, "isat")) {
    return(list(isat_obj = isat_obj))
  }

  data_obj <- module_row %>%
    dplyr::pull(.data$dataset) %>%
    .[[1]]

  module_args <- module_row %>%
    dplyr::pull(.data$model.args) %>%
    .[[1]]

  recipe <- module_args$forecast_recipe
  if (is.null(recipe)) {
    stop(
      "The estimated module does not contain a forecast recipe. ",
      "Re-estimate the OSEM model before forecasting."
    )
  }

  x_vars_basename <- recipe$regressors
  y_vars_basename <- recipe$dependent
  ylog <- recipe$dependent_transformation %in% c("log", "asinh")
  x_transformations <- recipe$transformations[x_vars_basename]
  xlog <- any(x_transformations %in% c("log", "asinh"))
  is_ardl <- identical(recipe$model_form, "ardl")

  q_pred_todrop <- c("q_1", "q_2", "q_3", "q_4")[
    !c("q_1", "q_2", "q_3", "q_4") %in% colnames(isat_obj$aux$mX)
  ]

  mconst <- "mconst" %in% colnames(isat_obj$aux$mX)

  pred_ar_needed <- recipe$selected_terms$term[
    recipe$selected_terms$role == "response_lag"
  ]
  pred_dl_needed <- recipe$selected_terms$term[
    recipe$selected_terms$lag > 0L &
      recipe$selected_terms$role %in% c(
        "dependent_level",
        "regressor_level",
        "regressor_difference"
      )
  ]

  ar_vec <- c(0L, recipe$ar_lags)
  y_names_vec <- recipe$transformed_level_name

  if (length(x_vars_basename) > 0) {
    x_names_vec_nolag <- vapply(
      x_vars_basename,
      function(variable) {
        paste0(
          if (recipe$transformations[[variable]] %in% c("log", "asinh")) "ln." else "",
          variable
        )
      },
      character(1)
    )
    x_names_vec <- c(x_names_vec_nolag, pred_dl_needed)
  } else {
    x_names_vec <- NULL
    x_names_vec_nolag <- NULL
  }

  isat_dates <- gets::isatdates(isat_obj)

  if (!is.null(isat_dates$iis)) {
    iis_pred <- matrix(
      0,
      nrow = nrow(exog_df_ready),
      ncol = nrow(isat_dates$iis),
      dimnames = list(NULL, isat_dates$iis$breaks)
    ) %>%
      dplyr::as_tibble()
  }

  if (!is.null(isat_dates$sis)) {
    sis_pred <- matrix(
      1,
      nrow = nrow(exog_df_ready),
      ncol = nrow(isat_dates$sis),
      dimnames = list(NULL, isat_dates$sis$breaks)
    ) %>%
      dplyr::as_tibble()
  }

  if (!is.null(isat_dates$tis)) {
    tis_indices <- length(isat_obj$aux$y.index) + seq_len(n.ahead)

    tis_pred <- dplyr::tibble(
      breaks = isat_dates$tis[, "index"],
      name = isat_dates$tis[, "breaks"],
      vals_ahead = list(tis_indices)
    ) %>%
      dplyr::mutate(
        value = purrr::map2(
          .x = .data$breaks,
          .y = .data$vals_ahead,
          .f = function(x, y) y - x
        )
      ) %>%
      tidyr::unnest("value") %>%
      dplyr::mutate(index = seq_len(dplyr::n()), .by = "name") %>%
      dplyr::select("index", "name", "value") %>%
      tidyr::pivot_wider(
        names_from = "name",
        values_from = "value",
        id_cols = "index"
      ) %>%
      dplyr::select(-"index")
  }

  if ("trend" %in% names(stats::coef(isat_obj))) {
    trend_pred <- dplyr::tibble(
      trend = (max(isat_obj$aux$mX[, "trend"]) + 1):
        (max(isat_obj$aux$mX[, "trend"]) + n.ahead)
    )
  }

  current_pred_raw <- exog_df_ready %>%
    dplyr::select(
      "time",
      dplyr::any_of(c("q_1", "q_2", "q_3", "q_4")),
      dplyr::any_of(names(data_obj))
    ) %>%
    dplyr::select(-dplyr::any_of(q_pred_todrop)) %>%
    {if (exists("trend_pred")) dplyr::bind_cols(., trend_pred) else .} %>%
    {if (exists("iis_pred")) dplyr::bind_cols(., iis_pred) else .} %>%
    {if (exists("sis_pred")) dplyr::bind_cols(., sis_pred) else .} %>%
    {if (exists("tis_pred")) dplyr::bind_cols(., tis_pred) else .}

  for (variable in x_vars_basename) {
    transformation <- recipe$transformations[[variable]]
    if (
      transformation %in% c("log", "asinh") &&
      variable %in% names(current_pred_raw)
    ) {
      current_pred_raw[[paste0("ln.", variable)]] <- transform_osem_values(
        current_pred_raw[[variable]],
        transformation
      )
    }
  }

  list(
    y_names_vec = y_names_vec,
    x_names_vec = x_names_vec,
    x_names_vec_nolag = x_names_vec_nolag,
    ar_vec = ar_vec,
    ylog = ylog,
    xlog = xlog,
    mconst = mconst,
    current_pred_raw = current_pred_raw,
    current_pred_raw_all = current_pred_raw,
    is_ardl = is_ardl,
    isat_obj = isat_obj,
    data_obj = data_obj,
    exog_df_ready = exog_df_ready,
    pred_ar_needed = pred_ar_needed,
    pred_dl_needed = pred_dl_needed,
    recipe = recipe
  )
}

