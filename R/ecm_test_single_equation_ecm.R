test_single_equation_ecm <- function(clean_data,
                                     dep_var_basename,
                                     x_vars_basename,
                                     level_x_vars_basename = x_vars_basename,
                                     use_logs = "both",
                                     trend = TRUE,
                                     module,
                                     alpha = 0.05,
                                     transformation_map) {
  # Set-up ------------------------------------------------------------------
  x_vars_basename <- x_vars_basename[!is.na(x_vars_basename)]
  x_vars_basename <- x_vars_basename[x_vars_basename != ""]

  level_x_vars_basename <- level_x_vars_basename[!is.na(level_x_vars_basename)]
  level_x_vars_basename <- level_x_vars_basename[level_x_vars_basename != ""]

  if (identical(level_x_vars_basename, character(0))) {
    return(list(
      decision = FALSE,
      method = "level_block_f",
      statistic = NA_real_,
      p.value = NA_real_,
      alpha_hat = NA_real_,
      level_terms = character(0),
      x_vars_basename = x_vars_basename,
      level_x_vars_basename = level_x_vars_basename,
      nobs_total = NA_integer_,
      nobs_complete = NA_integer_,
      first_complete_time = NA,
      last_complete_time = NA,
      reason = "No I(1) conditioning variables supplied; a single-equation cointegrating relation cannot be assessed."
    ))
  }

  # Build ECM testing equation ----------------------------------------------
  # The design is built using all module regressors. This means stationary
  # controls can remain in the testing equation. However, only the dependent
  # level and the I(1) regressors supplied through level_x_vars_basename are
  # tested as the long-run level block.
  design <- build_module_design(
    clean_data = clean_data,
    dep_var_basename = dep_var_basename,
    x_vars_basename = x_vars_basename,
    use_logs = use_logs,
    trend = trend,
    model_form = "ecm",
    dl_order = 0,
    module = module,
    transformation_map = transformation_map
  )

  dep_level_term <- paste0(
    ifelse(use_logs %in% c("both", "y"), "L1.ln.", "L1."),
    dep_var_basename
  )

  x_level_terms <- paste0(
    ifelse(use_logs %in% c("both", "x"), "L1.ln.", "L1."),
    level_x_vars_basename
  )

  level_terms <- unique(c(dep_level_term, x_level_terms))

  # Construct complete-case estimation sample -------------------------------
  df <- dplyr::bind_cols(
    dplyr::tibble(
      .time = if ("time" %in% names(clean_data)) clean_data$time else seq_len(NROW(clean_data)),
      y = design$yvar
    ),
    as.data.frame(design$xvars)
  )

  nobs_total <- NROW(df)

  complete_vars <- setdiff(names(df), ".time")
  df <- df[stats::complete.cases(df[, complete_vars, drop = FALSE]), , drop = FALSE]

  nobs_complete <- NROW(df)
  first_complete_time <- if (nobs_complete > 0) df$.time[1] else NA
  last_complete_time <- if (nobs_complete > 0) df$.time[nobs_complete] else NA

  df <- df %>%
    dplyr::select(-.time)

  if (NROW(df) <= length(level_terms) + 5) {
    return(list(
      decision = FALSE,
      method = "level_block_f",
      statistic = NA_real_,
      p.value = NA_real_,
      alpha_hat = NA_real_,
      level_terms = level_terms,
      x_vars_basename = x_vars_basename,
      level_x_vars_basename = level_x_vars_basename,
      nobs_total = nobs_total,
      nobs_complete = nobs_complete,
      first_complete_time = first_complete_time,
      last_complete_time = last_complete_time,
      reason = "Insufficient complete observations for the level-block test."
    ))
  }

  if (!all(level_terms %in% names(df))) {
    return(list(
      decision = FALSE,
      method = "level_block_f",
      statistic = NA_real_,
      p.value = NA_real_,
      alpha_hat = NA_real_,
      level_terms = level_terms,
      x_vars_basename = x_vars_basename,
      level_x_vars_basename = level_x_vars_basename,
      nobs_total = nobs_total,
      nobs_complete = nobs_complete,
      first_complete_time = first_complete_time,
      last_complete_time = last_complete_time,
      reason = "Not all lagged level terms were found in the ECM testing equation."
    ))
  }

  # Level-block test ---------------------------------------------------------
  # Full model:
  #
  #   D.y ~ L1.y + L1.x_I1 + stationary controls + short-run terms
  #
  # Restricted model:
  #
  #   D.y ~ stationary controls + short-run terms
  #
  # This tests whether the candidate long-run level block adds explanatory power.
  full_df <- df
  restricted_df <- df[, !names(df) %in% level_terms, drop = FALSE]

  full_model <- try(stats::lm(y ~ ., data = full_df), silent = TRUE)
  restricted_model <- try(stats::lm(y ~ ., data = restricted_df), silent = TRUE)

  if (inherits(full_model, "try-error") | inherits(restricted_model, "try-error")) {
    return(list(
      decision = FALSE,
      method = "level_block_f",
      statistic = NA_real_,
      p.value = NA_real_,
      alpha_hat = NA_real_,
      level_terms = level_terms,
      x_vars_basename = x_vars_basename,
      level_x_vars_basename = level_x_vars_basename,
      nobs_total = nobs_total,
      nobs_complete = nobs_complete,
      first_complete_time = first_complete_time,
      last_complete_time = last_complete_time,
      reason = "The level-block test failed."
    ))
  }

  ftest <- try(stats::anova(restricted_model, full_model), silent = TRUE)

  if (inherits(ftest, "try-error")) {
    return(list(
      decision = FALSE,
      method = "level_block_f",
      statistic = NA_real_,
      p.value = NA_real_,
      alpha_hat = NA_real_,
      level_terms = level_terms,
      x_vars_basename = x_vars_basename,
      level_x_vars_basename = level_x_vars_basename,
      nobs_total = nobs_total,
      nobs_complete = nobs_complete,
      first_complete_time = first_complete_time,
      last_complete_time = last_complete_time,
      reason = "The level-block F-test failed."
    ))
  }

  statistic <- suppressWarnings(as.numeric(ftest$F[2]))
  p.value <- suppressWarnings(as.numeric(ftest$`Pr(>F)`[2]))
  alpha_hat <- suppressWarnings(as.numeric(stats::coef(full_model)[dep_level_term]))

  decision <- is.finite(p.value) &&
    is.finite(alpha_hat) &&
    p.value < alpha &&
    alpha_hat < 0

  reason <- dplyr::case_when(
    isTRUE(decision) ~ "Lagged I(1) level block is significant and the adjustment coefficient is negative.",
    !is.finite(p.value) ~ "The level-block p-value is not finite.",
    !is.finite(alpha_hat) ~ "The adjustment coefficient is not finite.",
    p.value >= alpha ~ "The lagged I(1) level block is not significant at the chosen threshold.",
    alpha_hat >= 0 ~ "The adjustment coefficient is not negative.",
    TRUE ~ "No evidence for a valid equilibrium-correction term."
  )

  # Output ------------------------------------------------------------------
  return(list(
    decision = decision,
    method = "level_block_f",
    statistic = statistic,
    p.value = p.value,
    alpha_hat = alpha_hat,
    level_terms = level_terms,
    x_vars_basename = x_vars_basename,
    level_x_vars_basename = level_x_vars_basename,
    nobs_total = nobs_total,
    nobs_complete = nobs_complete,
    first_complete_time = first_complete_time,
    last_complete_time = last_complete_time,
    reason = reason
  ))
}
