# ---------------------------------------------------------------------------
# Real-world style quarterly simulation for ECM-auto testing
# ---------------------------------------------------------------------------
#
# Purpose:
# This simulation mimics a small empirical macro-emissions OSEM model.
#
# It includes:
#
#   1. Household consumption:      ConsHH
#   2. Industrial value added:     VAIndustry
#   3. Electricity consumption:    ElectrCons
#   4. Manufacturing CO2 emissions: EmiCO2ManInd
#
# The design deliberately includes:
#
#   - I(1) variables
#   - I(0) controls
#   - cointegrated equations
#   - non-cointegrated / weakly related equations
#   - multiple covariates
#   - contemporaneous interdependencies
#   - lag-only behaviour
#   - ragged edges
#   - seasonal patterns
#   - crisis breaks / outliers
#
# The variables are simulated in logs and then exponentiated, so that OSEM can
# be run with use_logs = "both".
#
# Suggested expected behaviour:
#
#   ConsHH:
#     Cointegrated with IncomeHH, affected by HICPlocal and FinWealthHH.
#     Expected ECM-auto result: "ecm"
#
#   VAIndustry:
#     Cointegrated with ConsHH and CapForm, but PriceETS only matters with lags.
#     Expected ECM-auto result: "ecm"
#
#   ElectrCons:
#     Driven by RealConsHH and HICPElectricity, but not cleanly cointegrated.
#     Expected ECM-auto result: often "fully_differenced"
#
#   EmiCO2ManInd:
#     Depends on RealVAIndustry and HICPlocal, with a structural break and
#     lagged ETS-price effects.
#     Expected ECM-auto result: likely "ecm" if RealVAIndustry is retained as
#     long-run driver, but this is intentionally a harder case.
#
# ---------------------------------------------------------------------------


simulate_real_world_ecm_auto_case <- function(nobs = 180, seed = 123) {
  set.seed(seed)

  # Quarterly time index -----------------------------------------------------
  time <- seq.Date(
    from = as.Date("1980-01-01"),
    by = "quarter",
    length.out = nobs
  )

  quarter <- as.integer(format(time, "%m"))
  quarter <- dplyr::case_when(
    quarter == 1 ~ 1L,
    quarter == 4 ~ 2L,
    quarter == 7 ~ 3L,
    quarter == 10 ~ 4L
  )

  q1 <- as.numeric(quarter == 1)
  q2 <- as.numeric(quarter == 2)
  q3 <- as.numeric(quarter == 3)
  q4 <- as.numeric(quarter == 4)

  # Containers ---------------------------------------------------------------
  l_income_hh <- numeric(nobs)
  l_finwealth_hh <- numeric(nobs)
  l_hicp <- numeric(nobs)
  inflation <- numeric(nobs)
  l_capform <- numeric(nobs)
  l_price_ets <- numeric(nobs)
  l_price_oil <- numeric(nobs)
  l_hicp_electricity <- numeric(nobs)

  l_cons_hh <- numeric(nobs)
  l_va_industry <- numeric(nobs)
  l_real_cons_hh <- numeric(nobs)
  l_real_va_industry <- numeric(nobs)
  l_electr_cons <- numeric(nobs)
  l_emi_co2_manind <- numeric(nobs)

  gap_cons <- numeric(nobs)
  gap_va <- numeric(nobs)
  gap_emi <- numeric(nobs)

  covid <- as.numeric(time >= as.Date("2020-01-01") & time <= as.Date("2020-10-01"))
  energy_crisis <- as.numeric(time >= as.Date("2022-01-01"))
  decarb_shift <- as.numeric(time >= as.Date("2010-01-01"))

  # DGP ----------------------------------------------------------------------
  for (t in 2:nobs) {
    inflation[t] <- 0.35 * inflation[t - 1] +
      0.010 +
      0.020 * energy_crisis[t] +
      rnorm(1, sd = 0.006)

    l_hicp[t] <- l_hicp[t - 1] + inflation[t]

    l_income_hh[t] <- l_income_hh[t - 1] +
      0.006 -
      0.035 * covid[t] +
      0.010 * covid[t - 1] +
      rnorm(1, sd = 0.012)

    l_finwealth_hh[t] <- l_finwealth_hh[t - 1] +
      0.004 +
      0.35 * (l_income_hh[t] - l_income_hh[t - 1]) +
      rnorm(1, sd = 0.020)

    l_capform[t] <- l_capform[t - 1] +
      0.004 +
      0.25 * (l_income_hh[t - 1] - l_income_hh[max(1, t - 2)]) -
      0.025 * covid[t] +
      rnorm(1, sd = 0.018)

    l_price_ets[t] <- l_price_ets[t - 1] +
      0.010 +
      0.025 * energy_crisis[t] +
      rnorm(1, sd = 0.035)

    l_price_oil[t] <- 0.80 * l_price_oil[t - 1] +
      0.10 * energy_crisis[t] +
      rnorm(1, sd = 0.060)

    l_hicp_electricity[t] <- l_hicp_electricity[t - 1] +
      0.004 +
      0.06 * (l_price_oil[t] - l_price_oil[t - 1]) +
      0.03 * (l_price_ets[t] - l_price_ets[t - 1]) +
      0.020 * energy_crisis[t] +
      rnorm(1, sd = 0.012)

    cons_eq <- 0.85 * l_income_hh[t] +
      0.12 * l_finwealth_hh[t] -
      0.25 * l_hicp[t] +
      0.015 * q4[t] -
      0.020 * covid[t]

    gap_cons[t] <- 0.55 * gap_cons[t - 1] + rnorm(1, sd = 0.012)
    l_cons_hh[t] <- cons_eq + gap_cons[t]

    l_real_cons_hh[t] <- l_cons_hh[t] - l_hicp[t]

    va_eq <- 0.55 * l_cons_hh[t] +
      0.40 * l_capform[t] -
      0.04 * l_price_ets[max(1, t - 2)] -
      0.030 * covid[t]

    gap_va[t] <- 0.65 * gap_va[t - 1] + rnorm(1, sd = 0.018)
    l_va_industry[t] <- va_eq + gap_va[t]

    l_real_va_industry[t] <- l_va_industry[t] - l_hicp[t]

    # l_electr_cons[t] <- l_electr_cons[t - 1] +
    #   0.002 +
    #   0.10 * (l_real_cons_hh[t] - l_real_cons_hh[t - 1]) -
    #   0.07 * (l_hicp_electricity[t] - l_hicp_electricity[t - 1]) +
    #   0.020 * q1[t] -
    #   0.015 * q3[t] -
    #   0.010 * covid[t] +
    #   rnorm(1, sd = 0.014)

    l_electr_cons[t] <- l_electr_cons[t - 1] +
      0.002 +
      0.35 * (l_real_cons_hh[t] - l_real_cons_hh[t - 1]) -
      0.25 * (l_hicp_electricity[t] - l_hicp_electricity[t - 1]) +
      0.020 * q1[t] -
      0.015 * q3[t] -
      0.010 * covid[t] +
      rnorm(1, sd = 0.008)

    emi_eq <- 0.80 * l_real_va_industry[t] -
      0.10 * l_hicp[t] -
      0.06 * l_price_ets[max(1, t - 3)] -
      0.18 * decarb_shift[t] -
      0.08 * covid[t]

    gap_emi[t] <- 0.50 * gap_emi[t - 1] + rnorm(1, sd = 0.020)
    l_emi_co2_manind[t] <- emi_eq + gap_emi[t]
  }

  out <- dplyr::tibble(
    time = time,

    IncomeHH = exp(11.5 + l_income_hh),
    FinWealthHH = exp(12.2 + l_finwealth_hh),
    HICPlocal = exp(4.6 + l_hicp),
    CapForm = exp(10.8 + l_capform),
    PriceETS = exp(3.2 + l_price_ets),
    PriceOil = exp(4.0 + l_price_oil),
    HICP_Electricity = exp(4.5 + l_hicp_electricity),

    ConsHH = exp(11.2 + l_cons_hh),
    RealConsHH = exp(11.0 + l_real_cons_hh),
    VAIndustry = exp(10.9 + l_va_industry),
    RealVAIndustry = exp(10.7 + l_real_va_industry),
    ElectrCons = exp(7.0 + l_electr_cons),
    EmiCO2ManInd = exp(8.0 + l_emi_co2_manind)
  )

  # Ragged edges -------------------------------------------------------------
  out$PriceETS[1:32] <- NA_real_
  out$HICP_Electricity[1:20] <- NA_real_
  out$ElectrCons[(nobs - 1):nobs] <- NA_real_
  out$EmiCO2ManInd[nobs] <- NA_real_

  # One explicit outlier -----------------------------------------------------
  out$EmiCO2ManInd[which(out$time == as.Date("2009-01-01"))] <-
    out$EmiCO2ManInd[which(out$time == as.Date("2009-01-01"))] * 0.85

  return(out)
}


# Local dictionary / input helpers ------------------------------------------

make_real_world_ecm_dictionary <- function(vars) {
  dplyr::tibble(
    model_varname = vars,
    full_name = vars,
    database = "local",
    geo = "DE",
    dataset_id = NA_character_,
    freq = ""
  )
}


make_real_world_ecm_input <- function(df) {
  df %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")
}


# Specification --------------------------------------------------------------
#
# This specification has four equations and recursive interdependencies:
#
#   1. ConsHH depends on IncomeHH, HICPlocal, and FinWealthHH.
#   2. VAIndustry depends on ConsHH, CapForm, PriceETS, and PriceOil.
#      PriceETS is specified as lag-only to mimic policy variables whose effects
#      are delayed.
#   3. ElectrCons depends on RealConsHH, HICP_Electricity, PriceETS, and PriceOil.
#      This is intentionally less clean and may fall back to differencing.
#   4. EmiCO2ManInd depends on RealVAIndustry, HICPlocal, PriceETS, and ElectrCons.
#      This mixes real activity, prices, policy, and energy demand.
#
# The lag column tests the existing lag-only machinery.
make_real_world_ecm_specification <- function() {
  dplyr::tibble(
    type = c("n", "n", "n", "n"),
    dependent = c(
      "ConsHH",
      "VAIndustry",
      "ElectrCons",
      "EmiCO2ManInd"
    ),
    independent = c(
      "IncomeHH + HICPlocal + FinWealthHH",
      "ConsHH + CapForm + PriceETS + PriceOil",
      "RealConsHH + HICP_Electricity + PriceETS + PriceOil",
      "RealVAIndustry + HICPlocal + PriceETS + ElectrCons"
    ),
    lag = c(
      "",
      "PriceETS",
      "PriceETS",
      "PriceETS"
    ),
    cvar = c("", "", "", "")
  )
}


# Optional extraction helpers ------------------------------------------------

extract_ecm_decision <- function(model, dep_var) {
  model$opts_df %>%
    dplyr::filter(.data$dependent == dep_var) %>%
    dplyr::pull(.data$ecm_decision) %>%
    purrr::pluck(1)
}


extract_ecm_selected <- function(model, dep_var) {
  model$opts_df %>%
    dplyr::filter(.data$dependent == dep_var) %>%
    dplyr::pull(.data$ardl_or_ecm_selected)
}


# Example run ----------------------------------------------------------------
#
# This is deliberately not written as strict testthat code yet. The point is to
# inspect whether the decisions look sensible before freezing expectations.
#
# Once stable, this can be converted into expectations such as:
#
#   expect_identical(extract_ecm_selected(model, "ConsHH"), "ecm")
#   expect_identical(extract_ecm_selected(model, "VAIndustry"), "ecm")
#   expect_true(extract_ecm_selected(model, "ElectrCons") %in% c("fully_differenced", "ecm"))
#   expect_true("ecm_decision" %in% names(model$opts_df))
#
# Because unit-root and level-block diagnostics are sample-dependent, I would
# initially inspect the selected forms rather than over-constrain every equation.

sim_data <- simulate_real_world_ecm_auto_case(nobs = 180, seed = 123)

#sim_data <- simulate_real_world_ecm_auto_case(nobs = 180, seed = 123)

sim_data %>%
  tidyr::pivot_longer(-time, names_to = "variable", values_to = "value") %>%
  ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
  ggplot2::geom_line(na.rm = TRUE) +
  ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 3) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Simulated quarterly macro-emissions data",
    x = NULL,
    y = NULL
  )

specification <- make_real_world_ecm_specification()

dictionary <- make_real_world_ecm_dictionary(
  vars = unique(c(
    specification$dependent,
    unlist(strsplit(gsub(" ", "", specification$independent), "\\+"))
  ))
)

model <- run_model(
  specification = specification,
  dictionary = dictionary,
  input = make_real_world_ecm_input(sim_data),
  primary_source = "local",
  use_logs = "both",
  ardl_or_ecm = "ecm",
  ecm_pretest = "auto",
  trend = TRUE,
  max.ar = 4,
  max.dl = 4,
  saturation = c("IIS", "SIS"),
  saturation.tpval = 0.01,
  gets_selection = TRUE,
  selection.tpval = 0.01,
  present = FALSE,
  plot = TRUE,
  quiet = FALSE
)


# Inspect decisions ----------------------------------------------------------

model$opts_df %>%
  dplyr::select(
    dependent,
    independent,
    lag,
    ardl_or_ecm_requested,
    ardl_or_ecm_selected,
    ecm_decision
  )

purrr::map(
  c("ConsHH", "VAIndustry", "ElectrCons", "EmiCO2ManInd"),
  ~ extract_ecm_decision(model, .x)
) %>%
  stats::setNames(c("ConsHH", "VAIndustry", "ElectrCons", "EmiCO2ManInd"))


# Inspect compact decision summary ------------------------------------------

decision_summary <- model$opts_df %>%
  dplyr::filter(.data$type == "n") %>%
  dplyr::mutate(
    selected = purrr::map_chr(.data$ecm_decision, ~ .x$selected),
    reason = purrr::map_chr(.data$ecm_decision, ~ .x$reason),
    coint_p_value = purrr::map_dbl(
      .data$ecm_decision,
      ~ {if (!is.null(.x$coint_test)) .x$coint_test$p.value else NA_real_}
    ),
    alpha_hat = purrr::map_dbl(
      .data$ecm_decision,
      ~ {if (!is.null(.x$coint_test)) .x$coint_test$alpha_hat else NA_real_}
    ),
    nobs_complete = purrr::map_int(
      .data$ecm_decision,
      ~ {if (!is.null(.x$coint_test)) .x$coint_test$nobs_complete else NA_integer_}
    )
  ) %>%
  dplyr::select(
    dependent,
    selected,
    coint_p_value,
    alpha_hat,
    nobs_complete,
    reason
  )

decision_summary
