test_that("lagged differences respect the requested transformation", {
  df <- data.frame(
    D.Y = 1:10, D.X = 2:11, D.ln.X = 3:12,
    L1.D.X = 4:13, L1.D.ln.X = 5:14
  )

  for (logs in c("none", "x")) {
    design <- build_module_design(
      clean_data = df, dep_var_basename = "Y", x_vars_basename = "X",
      use_logs = logs, trend = FALSE, model_form = "diff",
      dl_order = 1, module = list(lag = ""), transformation_map = NULL
    )

    expect_identical(
      grep("^L1\\.D\\.", names(design$xvars), value = TRUE),
      if (logs == "none") "L1.D.X" else "L1.D.ln.X"
    )
  }
})
