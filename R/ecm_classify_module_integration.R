classify_module_integration <- function(clean_data,
                                        dep_var_basename,
                                        x_vars_basename,
                                        use_logs = "both",
                                        max.ar = 4,
                                        alpha = c("1pct", "5pct", "10pct"),
                                        selectlags = c("Fixed", "AIC", "BIC")) {
  alpha <- match.arg(alpha)
  selectlags <- match.arg(selectlags)

  vars <- dplyr::bind_rows(
    dplyr::tibble(basevarname = dep_var_basename, type = "dependent"),
    dplyr::tibble(basevarname = x_vars_basename, type = "independent")
  ) %>%
    dplyr::filter(!is.na(.data$basevarname), .data$basevarname != "") %>%
    dplyr::mutate(
      transformed_name = dplyr::case_when(
        .data$type == "dependent" & use_logs %in% c("both", "y") ~ paste0("ln.", .data$basevarname),
        .data$type == "independent" & use_logs %in% c("both", "x") ~ paste0("ln.", .data$basevarname),
        TRUE ~ .data$basevarname
      ),
      ur_level = vector(mode = "list", length = dplyr::n()),
      ur_difference = vector(mode = "list", length = dplyr::n()),
      order = NA_character_,
      stationarity_type = NA_character_,
      deterministic_used = NA_character_,
      reason = NA_character_
    )

  for (i in seq_len(NROW(vars))) {

    varname <- vars$transformed_name[i]

    if (!varname %in% names(clean_data)) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- paste0("Variable ", varname, " not found in clean_data.")
      next
    }

    x <- clean_data[[varname]]
    x <- as.numeric(x)

    non_na <- which(!is.na(x))

    if (length(non_na) == 0) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- "Variable contains only missing values."
      next
    }

    x_trimmed <- x[min(non_na):max(non_na)]

    if (any(is.na(x_trimmed))) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- "Internal missing values detected after trimming ragged edges."
      next
    }

    x <- x_trimmed

    if (length(x) <= max(10, max.ar + 5)) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- "Insufficient observations for unit-root testing."
      next
    }

    level_test <- try(
      decide_unit_roots(
        test_unit_roots(
          x = x,
          max.ar = max.ar,
          selectlags = selectlags
        ),
        alpha = alpha
      ),
      silent = TRUE
    )

    if (inherits(level_test, "try-error")) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- "Unit-root test failed in levels."
      next
    }

    vars$ur_level[[i]] <- level_test

    # -----------------------------------------------------------------------
    # Determine which deterministic specification rejects the unit-root null
    # -----------------------------------------------------------------------

    if (isTRUE(level_test$decision$reject_ur)) {
      vars$order[i] <- "I0"
      vars$stationarity_type[i] <- level_test$decision$stationarity_type
      vars$deterministic_used[i] <- level_test$decision$deterministic

      vars$reason[i] <- dplyr::case_when(
        identical(level_test$decision$stationarity_type, "level_stationary") ~
          "Unit root rejected without deterministic terms.",
        identical(level_test$decision$stationarity_type, "drift_stationary") ~
          "Unit root rejected with a constant.",
        identical(level_test$decision$stationarity_type, "trend_stationary") ~
          "Unit root rejected when a deterministic trend is included.",
        TRUE ~
          "Unit root rejected in levels."
      )

      next
    }

    # -----------------------------------------------------------------------
    # First differences
    # -----------------------------------------------------------------------

    dx <- diff(x)

    if (length(dx) <= max(10, max.ar + 5)) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- "Insufficient observations for unit-root testing in first differences."
      next
    }

    diff_test <- try(
      decide_unit_roots(
        test_unit_roots(
          x = dx,
          max.ar = max.ar,
          selectlags = selectlags
        ),
        alpha = alpha
      ),
      silent = TRUE
    )

    if (inherits(diff_test, "try-error")) {
      vars$order[i] <- "uncertain"
      vars$reason[i] <- "Unit-root test failed in first differences."
      next
    }

    vars$ur_difference[[i]] <- diff_test

    if (isTRUE(diff_test$decision$reject_ur)) {
      vars$order[i] <- "I1"
      vars$reason[i] <- "Unit root not rejected in levels but rejected in first differences."
    } else {
      vars$order[i] <- "I2_or_uncertain"
      vars$reason[i] <- "Unit root not rejected in levels or first differences."
    }
  }

  return(vars)
}
