test_that("error raised when columns missing", {
  cfg <- data.frame(type = "n", dependent = "Y", independent = "X", lag = "")
  expect_error(check_config_table(cfg), "config_table does not contain all required columns")
})

test_that("rejects cycles", {
  # direct simultaneity
  cfg <- tibble::tibble(
    type        = c("n", "n"),
    dependent   = c("A", "B"),
    independent = c("B", "A"),
    lag         = "",
    cvar        = ""
  )
  expect_error(check_config_table(cfg), "Contemporaneous simultaneity detected")

  # indirect simultaneity
  cfg <- tibble::tibble(
    type        = c("n", "n", "n"),
    dependent   = c("B", "C", "A"),
    independent = c("A", "B", "C"), # A->B->C->A
    lag         = "",
    cvar        = ""
  )
  expect_error(check_config_table(cfg), "Contemporaneous simultaneity detected")

  # long indirect simultaneity
  cfg <- tibble::tibble(
    type        = c("n", "n", "n", "n", "n"),
    dependent   = c("B", "C", "D", "E", "A"),
    independent = c("A", "B", "C", "D", "E"), # A->B->C->D->E->A
    lag         = "",
    cvar        = ""
  )
  expect_error(check_config_table(cfg), "Contemporaneous simultaneity detected")
})

test_that("valid configurations are not rejected and correctly ordered", {
  # long acyclic simultaneity: no error, correct ordering
  cfg <- tibble::tibble(
    type        = c("n", "n", "n", "n", "n"),
    dependent   = c("B", "C", "D", "E", "F"),
    independent = c("A", "B", "C", "D", "E"), # A->B->C->D->E->F
    lag         = "",
    cvar        = ""
  )
  cfg <- cfg[c(4, 5, 1, 3, 2), ] # reshuffle, so not already ordered
  out <- check_config_table(cfg)
  expect_identical(out$dependent, c("B", "C", "D", "E", "F"))
  expect_setequal(colnames(out), c("order", "type", "dependent", "independent", "lag", "cvar", "index"))

  # direct simultaneity broken when lag only is specified
  cfg <- tibble::tibble(
    type        = c("n", "n"),
    dependent   = c("A", "B"),
    independent = c("B", "A"),
    lag         = "A",
    cvar        = ""
  )
  expect_no_error(check_config_table(cfg))

  # more complex example with CVARs and lags
  # systems should be reduced to single row; dependent should contain both vars separated by comma
  cfg <- dplyr::tibble(
    type = c("n", "n", "n", "n", "n", "n", "d", "n", "n", "n", "n"),
    dependent = c("X", "Y", "U", "V", "W", "M", "T", "Q", "S", "A", "B"),
    independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R", "X", "X"),
    lag = c("", "", "", "W", "", "U, Y", "", "", "", "", ""),
    cvar = c("system1", "system1", "", "", "", "", "", "", "", "system2", "system2")
  )
  out <- check_config_table(cfg)
  expect_identical(NROW(out), NROW(cfg) - 2L) # dimension reduced because of system
  expect_setequal(colnames(out), c("order", "type", "dependent", "independent", "lag", "cvar", "index"))
  # valid ordering is not unique (and I believe topo_sort() can sometimes pick different ones)
  # Check that the set of variables is correct, regardless of order
  expected_vars <- c("M", "Q", "U", "S", "X,Y", "V", "A,B", "W", "T")
  expect_setequal(out$dependent, expected_vars)
  # V must come after its parent U
  expect_true(which(out$dependent == "U") < which(out$dependent == "V"))
  # W must come after its parents U and V
  expect_true(which(out$dependent == "U") < which(out$dependent == "W"))
  expect_true(which(out$dependent == "V") < which(out$dependent == "W"))
  # The "X,Y" system must come after its parent U
  expect_true(which(out$dependent == "U") < which(out$dependent == "X,Y"))
  # The "A,B" system must come after its parent "X,Y" system
  expect_true(which(out$dependent == "X,Y") < which(out$dependent == "A,B"))
  expect_true(which(out$cvar == "system1") < which(out$cvar == "system2"))
  # The T identity must come after its parents U, V, W
  expect_true(which(out$dependent == "U") < which(out$dependent == "T"))
  expect_true(which(out$dependent == "V") < which(out$dependent == "T"))
  expect_true(which(out$dependent == "W") < which(out$dependent == "T"))
})

# "legacy tests" that Moritz had put in the config_table.R file, now extended by columns lag and cvar
test_that("legacy tests run successfully", {
  # This has a direct endogeneity
  cfg <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n"
    ),
    dependent = c(
      "JL",
      "TOTS",
      "B"
    ),
    independent = c(
      "TOTS - CP - CO - J - A",
      "YF + B",
      "CP + J + B"
    ),
    lag = c("", "", ""),
    cvar = c("", "", "")
  )
  expect_error(check_config_table(cfg), "Contemporaneous simultaneity detected")

  # This has an indirect endogeneity
  cfg <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n"
    ),
    dependent = c(
      "JL",
      "TOTS",
      "B",
      "CP"
    ),
    independent = c(
      "TOTS - CP - CO - J - A",
      "YF + B",
      "CP + J",
      "B + CO"
    ),
    lag = c("", "", "", ""),
    cvar = c("", "", "", "")
  )
  expect_error(check_config_table(cfg), "Contemporaneous simultaneity detected")

  # This is the correct specification
  cfg <- dplyr::tibble(
    type = c("d", "d", "n"),
    dependent = c(
      "JL",
      "TOTS",
      "B"
    ),
    independent = c(
      "TOTS - CP - CO - J - A",
      "YF + B",
      "CP + J"
    ),
    lag = c("", "", ""),
    cvar = c("", "", "")
  )
  expect_no_error(check_config_table(cfg))

  # This cycle was previously not detected (see GitHub Issue #27)
  # This is now successfully detected
  cfg <- dplyr::tibble(
    type = c("n", "n", "n"),
    dependent = c("JL", "TOTS", "B"),
    independent = c("TOTS", "B", "JL"), # JL <U+2190> TOTS <U+2190> B <U+2190> JL
    lag = c("", "", ""),
    cvar = c("", "", "")
  )
  expect_error(check_config_table(cfg))
})

test_that("classify_variables() works correctly with CVAR entries", {
  # takes as input the result from check_config_table()

  # baseline case without cvar
  cfg <- dplyr::tibble(
    type = c("d", "d", "n"),
    dependent = c(
      "JL",
      "TOTS",
      "B"
    ),
    independent = c(
      "TOTS - CP - CO - J - A",
      "YF + B",
      "CP + J"
    ),
    lag = c("", "", ""),
    cvar = c("", "", "")
  )
  mdl <- osem:::check_config_table(cfg)
  clv <- osem:::classify_variables(mdl)
  def <- clv %>%
    dplyr::filter(class == "d") %>%
    dplyr::pull(var)
  exo <- clv %>%
    dplyr::filter(class == "x") %>%
    dplyr::pull(var)
  end <- clv %>%
    dplyr::filter(class == "n") %>%
    dplyr::pull(var)
  expect_setequal(def, c("JL", "TOTS"))
  expect_setequal(end, "B")
  expect_setequal(exo, c("CP", "J", "YF", "CO", "A"))

  # with cvar
  cfg <- dplyr::tibble(
    type = c("n", "n", "n", "n", "n", "n", "d", "n", "n", "n", "n", "d", "n"),
    dependent = c("Y", "Z", "U", "V", "W", "M", "T", "Q", "S", "A", "B", "C", "D"),
    independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R", "", "", "A - B", "Y + L"),
    lag = c("", "", "", "W", "", "U, Y", "", "", "", "", "", "", "L"),
    cvar = c("system1", "system1", "", "", "", "", "", "", "", "system2", "system2", "", "")
  )
  mdl <- osem:::check_config_table(cfg)
  clv <- osem:::classify_variables(mdl)
  def <- clv %>%
    dplyr::filter(class == "d") %>%
    dplyr::pull(var)
  exo <- clv %>%
    dplyr::filter(class == "x") %>%
    dplyr::pull(var)
  end <- clv %>%
    dplyr::filter(class == "n") %>%
    dplyr::pull(var)
  expect_setequal(def, c("T", "C"))
  expect_setequal(exo, c("L", "R"))
  expect_setequal(end, c("Y", "Z", "U", "V", "W", "M", "Q", "S", "A", "B", "D"))
})

test_that("identify_module_data() works with CVAR", {
  cfg <- dplyr::tibble(
    type = c("n", "n", "n", "n", "n", "n", "d", "n", "n", "n", "n", "d", "n"),
    dependent = c("Y", "Z", "U", "V", "W", "M", "T", "Q", "S", "A", "B", "C", "D"),
    independent = c("U", "U", "", "U + W", "U + V", "Y + U", "U + V + W", "", "R", "", "", "A - B", "Y + L"),
    lag = c("", "", "", "W", "", "U, Y", "", "", "", "", "", "", "L"),
    cvar = c("system1", "system1", "", "", "", "", "", "", "", "system2", "system2", "", "")
  )
  mdl <- osem:::check_config_table(cfg)
  classification <- osem:::classify_variables(mdl)
  df <- readRDS(test_path("testdata", "cvar", "artificial_cvar_data.rds"))
  # "normal", single equation module
  single1 <- osem:::identify_module_data(mdl[which(mdl$dependent == "M"), ], classification, data = df)
  expect_named(single1, c("time", "na_item", "values", "geo"))
  expect_setequal(unique(single1$na_item), c("M", "Y", "U"))
  expect_equal(NROW(single1), 300L)
  # "system1"
  cvar1 <- osem:::identify_module_data(mdl[which(mdl$cvar == "system1"), ], classification, data = df)
  expect_named(cvar1, c("time", "na_item", "values", "geo"))
  expect_setequal(unique(cvar1$na_item), c("Y", "Z", "U"))
  expect_equal(NROW(cvar1), 300L)
  # "system2"
  cvar2 <- osem:::identify_module_data(mdl[which(mdl$cvar == "system2"), ], classification, data = df)
  expect_named(cvar2, c("time", "na_item", "values", "geo"))
  expect_setequal(unique(cvar2$na_item), c("A", "B"))
  expect_equal(NROW(cvar2), 200L)
})

test_that("clean_data() works with CVAR", {
  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Y", "Z"),
    independent = c("U", "U"),
    lag = c("", ""),
    cvar = c("system1", "system1")
  )
  mdl <- osem:::check_config_table(specification)
  df <- readRDS(test_path("testdata", "cvar", "artificial_cvar_data.rds")) %>%
    dplyr::filter(na_item %in% c("Y", "Z", "U"))
  df_wide <- tidyr::pivot_wider(df, names_from = "na_item", values_from = "values")
  # note: module arg takes single module and here our whole specification is a single module
  # note: opts_df can here be mdl too because no log_opts stored yet anyway
  a <- osem:::clean_data(
    raw_data = df, max.ar = 1, max.dl = 1, trend = TRUE,
    opts_df = mdl, module = mdl, use_logs = "both"
  )
  expect_named(a, c("df", "opts_df"))
  expect_identical(mdl[, 1:7], a$opts_df[, 1:7])
  expect_named(a$opts_df, c("order", "type", "dependent", "independent", "lag", "cvar", "index", "log_opts"))
  expect_type(a$opts_df$log_opts, "list")
  expect_length(a$opts_df$log_opts, 1L)
  expect_s3_class(a$opts_df$log_opts[[1]], "tbl")
  expect_identical(dim(a$opts_df$log_opts[[1]]), c(1L, 3L))
  expect_named(a$opts_df$log_opts[[1]], c("Y", "Z", "U"))
  y_transformation <- dplyr::if_else(any(df_wide$Y < 0), "asinh", "log")
  z_transformation <- dplyr::if_else(any(df_wide$Z < 0), "asinh", "log")
  u_transformation <- dplyr::if_else(any(df_wide$U < 0), "asinh", "log")
  expect_identical(a$opts_df$log_opts[[1]] %>% dplyr::pull(Y), y_transformation)
  expect_identical(a$opts_df$log_opts[[1]] %>% dplyr::pull(Z), z_transformation)
  expect_identical(a$opts_df$log_opts[[1]] %>% dplyr::pull(U), u_transformation)
})
