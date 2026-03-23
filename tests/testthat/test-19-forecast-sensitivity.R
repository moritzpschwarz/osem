# To use expect_snapshot_file() you'll typically need to start by writing
# a helper function that creates a file from your code, returning a path
save_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")

  if (ggplot2::is_ggplot(code)) {
    ggplot2::ggsave(filename = path, plot = code, width = 7, height = 5)
  } else {
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
  }
  path
}

# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  # skip_on_os("windows")
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path <- save_png(code)
  expect_snapshot_file(path, name)
}

df <- read.csv(test_path("testdata", "ragged_edge", "ragged_edge_emissions_data.csv"))

spec <- dplyr::tibble(
  type = c(
    "n",
    "n",
    "n",
    "n"
  ),
  dependent = c(
    "Import",
    "FinConsExpHH",
    "GCapitalForm",
    "EmiCO2Combustion"
  ),
  independent = c(
    "FinConsExpHH + GCapitalForm",
    "",
    "GValueAdd",
    "FinConsExpHH + GCapitalForm + GValueAdd"
  )
)

mod <- run_model(
  specification = spec,
  dictionary = dict,
  input = df,
  primary_source = "local",
  present = FALSE,
  quiet = TRUE, saturation = "IIS"
)


test_that("forecast_sensitivity() basic output structure (default)", {

  # Ensure deterministic where possible
  set.seed(1)

  res <- forecast_sensitivity(
    model = mod,
    size = 0.5,
    size_type = "pct",
    quiet = TRUE,
    impulse_response = TRUE,
    include_uncertainty = TRUE,
    exclude_zero_change = TRUE,
    exog_fill_method = "AR"
  )

  # Top-level list members
  expect_type(res, "list")
  expect_true("forecast_sensitivity" %in% names(res))
  expect_true("plot" %in% names(res))

  # Central results tibble sanity
  expect_s3_class(res$forecast_sensitivity, "tbl_df")
  expect_true(all(c("time", "na_item", "modified", "values", "init", "diff") %in% names(res$forecast_sensitivity)))
  expect_gt(nrow(res$forecast_sensitivity), 0)

  # Plot sanity
  expect_s3_class(res$plot, "ggplot")

  # Uncertainty outputs present if include_uncertainty=TRUE
  expect_true("uncertainties" %in% names(res))
  expect_s3_class(res$uncertainties, "tbl_df")
  expect_true(all(c("na_item", "time", "fit", "p05", "p95", "p25", "p75", "p025", "p975") %in% names(res$uncertainties)))

  # Impulse response outputs present if impulse_response=TRUE
  expect_true("forecast_sensitivity_impulse_response" %in% names(res))
  expect_true("plot_impulse_response" %in% names(res))
  expect_s3_class(res$forecast_sensitivity_impulse_response, "tbl_df")
  expect_s3_class(res$plot_impulse_response, "ggplot")

  if ("uncertainties_impulse_response" %in% names(res)) {
    expect_s3_class(res$uncertainties_impulse_response, "tbl_df")
  }

  # Snapshot the ggplot objects
  expect_snapshot_plot("forecast_sensitivity_full", res$plot)
  expect_snapshot_plot("forecast_sensitivity_impulse", res$plot_impulse_response)

})

test_that("forecast_sensitivity() validates size_type and size", {
  expect_error(
    forecast_sensitivity(model = mod, size_type = "bad", quiet = TRUE),
    "size_type must be either 'pct' or 'unit'"
  )

  expect_error(
    forecast_sensitivity(model = mod, size_type = "pct", size = -1, quiet = TRUE),
    "size must be larger than -1"
  )

  # Should work for unit size
  expect_no_error(
    forecast_sensitivity(model = mod, size_type = "unit", size = 1, quiet = TRUE)
  )
})

test_that("forecast_sensitivity() toggles impulse_response and include_uncertainty", {
  set.seed(1)

  res_no_unc <- forecast_sensitivity(
    model = mod,
    quiet = TRUE,
    impulse_response = TRUE,
    include_uncertainty = FALSE
  )
  expect_true("uncertainties" %in% names(res_no_unc) == FALSE)
  expect_true("uncertainties_impulse_response" %in% names(res_no_unc) == FALSE)

  res_no_imp <- forecast_sensitivity(
    model = mod,
    quiet = TRUE,
    impulse_response = FALSE,
    include_uncertainty = TRUE
  )
  expect_true("forecast_sensitivity_impulse_response" %in% names(res_no_imp) == FALSE)
  expect_true("plot_impulse_response" %in% names(res_no_imp) == FALSE)
  expect_true("uncertainties_impulse_response" %in% names(res_no_imp) == FALSE)

  # Still should always provide central sensitivity and plot (unless you suppress plot upstream)
  expect_true("forecast_sensitivity" %in% names(res_no_imp))
})

test_that("forecast_sensitivity() grepl_variables limits modified set", {

  # Get an actual exogenous name from the model's initial forecast object
  init_fc <- forecast_model(mod, quiet = TRUE, exog_fill_method = "AR", plot = FALSE)
  exog_names <- init_fc$exog_data_nowcast %>%
    dplyr::select(-"time", -dplyr::starts_with("q_")) %>%
    names()

  # If there are no exogenous variables, skip this test gracefully
  if (length(exog_names) == 0) skip("No exogenous variables available in this model/forecast.")

  target <- exog_names[[1]]

  # Anchor regex to match exactly that variable name
  # (escape any regex meta characters just in case)
  target_re <- paste0("^", gsub("([][{}()+*.^$|\\\\?])", "\\\\\\1", target), "$")

  res <- forecast_sensitivity(
    model = mod,
    quiet = TRUE,
    grepl_variables = target_re,
    impulse_response = FALSE,
    include_uncertainty = FALSE
  )

  # The "modified" column should only contain the selected variable
  expect_true(all(unique(res$forecast_sensitivity$modified) == target))


  # Snapshot the ggplot objects
  expect_snapshot_plot("forecast_sensitivity_full_nouncertainty", res$plot)
  expect_null(res$plot_impulse_response)

})




