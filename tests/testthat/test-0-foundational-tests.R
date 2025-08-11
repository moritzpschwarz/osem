test_that("error raised when columns missing", {
  cfg <- data.frame(type = "n", dependent = "Y", independent = "X", lag = "")
  expect_error(osem:::check_config_table(cfg), "config_table does not contain all required columns")
})

test_that("input errors raised", {
  cfg <- data.frame(
    type = c("n", "n"),
    dependent = c("A", "B"),
    independent = c("C", "C"),
    lag = c("C", "C"),
    cvar = c("sys1", "sys1")
  )
  expect_error(osem:::check_config_table(cfg),
    "CVAR modules cannot specify exogenous variables that enter only as lags.",
    fixed = TRUE
  )

  cfg <- data.frame(
    type = c("n", "n"),
    dependent = c("A", "B"),
    independent = c("C", "D"),
    lag = c("", ""),
    cvar = c("sys1", "sys1")
  )
  expect_error(osem:::check_config_table(cfg),
    "Please specify the same independent variables within each CVAR system.",
    fixed = TRUE
  )

  cfg <- data.frame(
    type = c("d", "d"),
    dependent = c("A", "B"),
    independent = c("", ""),
    lag = c("", ""),
    cvar = c("sys1", "sys1")
  )
  expect_error(osem:::check_config_table(cfg),
    "All CVAR modules must be of type 'n'.",
    fixed = TRUE
  )

  cfg <- data.frame(
    type = "n",
    dependent = "A",
    independent = "C",
    lag = "D",
    cvar = ""
  )
  expect_error(osem:::check_config_table(cfg),
    "lagged (column 'lag') has to also be specified in the 'independent' formula.",
    fixed = TRUE
  )

  cfg <- data.frame(
    type = c("n", "n"),
    dependent = c("A", "B"),
    independent = c("C + D", "E + F"),
    lag = c("C, D", "F, E"), # swapped order for B module, should still work
    cvar = c("", "")
  )
  expect_no_error(osem:::check_config_table(cfg))
})

test_that("rejects cycles", {
  # direct simultaneity
  cfg <- dplyr::tibble(
    type        = c("n", "n"),
    dependent   = c("A", "B"),
    independent = c("B", "A"),
    lag         = "",
    cvar        = ""
  )
  expect_error(osem:::check_config_table(cfg), "Contemporaneous simultaneity detected")

  # indirect simultaneity
  cfg <- dplyr::tibble(
    type        = c("n", "n", "n"),
    dependent   = c("B", "C", "A"),
    independent = c("A", "B", "C"), # A->B->C->A
    lag         = "",
    cvar        = ""
  )
  expect_error(check_config_table(cfg), "Contemporaneous simultaneity detected")

  # long indirect simultaneity
  cfg <- dplyr::tibble(
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
  cfg <- dplyr::tibble(
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
  cfg <- dplyr::tibble(
    type        = c("n", "n"),
    dependent   = c("A", "B"),
    independent = c("B", "A"),
    lag         = c("", "A"),
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

test_that("add_to_original_data() works with CVAR", {
  spec <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Y", "Z"),
    independent = c("U", "U"),
    lag = c("", ""),
    cvar = c("system1", "system1")
  )
  dict <- dplyr::tibble(
    model_varname = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "A", "B"),
    full_name = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "A", "B"),
    database = c("local", "local", "local", "local", "local", "local", "local", "local", NA, "local", "local", "local"),
    geo = "DE",
    dataset_id = NA,
    freq = ""
  )
  mdl <- osem:::check_config_table(spec)
  opts <- mdl
  opts$log_opts <- list(dplyr::tibble(Y = NA, Z = NA, U = NA))
  df <- readRDS(test_path("testdata", "cvar", "artificial_cvar_data.rds")) %>%
    dplyr::filter(na_item %in% c("Y", "Z", "U"))
  df_wide <- tidyr::pivot_wider(df, names_from = "na_item", values_from = "values") %>%
    dplyr::mutate(index = 1:100)
  depnames <- c("Y", "Z")
  yvars <- df_wide %>%
    dplyr::select(Y, Z)
  xvars <- df_wide %>%
    dplyr::select(U)
  K <- 2
  cointtest <- urca::ca.jo(
    x = yvars, type = "trace", ecdet = "const", K = K,
    season = NULL, dumvar = xvars, spec = "transitory"
  )
  cvar_vecm <- urca::cajorls(cointtest, r = 1)
  cvar_varm <- vars::vec2var(cointtest, r = 1)
  # mimic object that would be returned by estimate_cvar(), but only necessary components for this functions
  cvar_out <- list(vecm = cvar_vecm, varm = cvar_varm, args = list(ar = K))
  a <- osem:::add_to_original_data(
    clean_data = df_wide,
    model_object = cvar_out,
    dep_var_basename = depnames,
    model_type = "cvar",
    opts_df = opts,
    module = mdl[1, ]
  )
  expect_s3_class(a, c("data.frame", "tbl"))
  expect_identical(dim(a), as.integer(c(NROW(df_wide), NCOL(df_wide) + 4))) # fitted values of Y,Z and "level"
  expect_named(a, c("time", "geo", "Y", "Z", "U", "index", "Y.hat", "Z.hat", "Y.level.hat", "Z.level.hat"))
  # since we did not take log transformations, fitted values and level.fitted values should be same
  expect_identical(a$Y.hat, a$Y.level.hat)
  expect_identical(a$Z.hat, a$Z.level.hat)
  # since we specified K = 2 (lags in level of VAR), we should lose the first two observations
  expect_identical(a$Y.hat %>% head(2), c(NA_real_, NA_real_))
  expect_identical(a$Z.hat %>% head(2), c(NA_real_, NA_real_))
  expect_true(all(!is.na(a$Y.hat %>% tail(-2))))
  expect_true(all(!is.na(a$Z.hat %>% tail(-2))))

  ### CVAR with log transformations
  opts <- mdl
  opts$log_opts <- list(dplyr::tibble(
    Y = "log", Z = "asinh", U = NA
  ))
  df_wide <- df_wide %>%
    # Y has no negative values but Z does
    dplyr::mutate(ln.Y = log(Y), ln.Z = asinh(Z))
  yvars <- df_wide %>%
    dplyr::select(ln.Y, ln.Z)
  xvars <- df_wide %>%
    dplyr::select(U)
  cointtest <- urca::ca.jo(
    x = yvars, type = "trace", ecdet = "const", K = K,
    season = NULL, dumvar = xvars, spec = "transitory"
  )
  cvar_vecm <- urca::cajorls(cointtest, r = 1)
  cvar_varm <- vars::vec2var(cointtest, r = 1)
  # mimic object that would be returned by estimate_cvar(), but only necessary components for this functions
  cvar_out <- list(vecm = cvar_vecm, varm = cvar_varm, args = list(ar = K))
  a <- osem:::add_to_original_data(
    clean_data = df_wide,
    model_object = cvar_out,
    dep_var_basename = depnames,
    model_type = "cvar",
    opts_df = opts,
    module = mdl[1, ]
  )
  expect_s3_class(a, c("data.frame", "tbl"))
  expect_identical(dim(a), as.integer(c(NROW(df_wide), NCOL(df_wide) + 4))) # fitted values of Y,Z and "level"
  expect_named(a, c("time", "geo", "Y", "Z", "U", "index", "ln.Y", "ln.Z", "Y.hat", "Z.hat", "Y.level.hat", "Z.level.hat"))
  # level.fitted values are now transformed
  expect_identical(a$Y.level.hat, exp(a$Y.hat))
  expect_identical(a$Z.level.hat, sinh(a$Z.hat))
  # since we specified K = 2 (lags in level of VAR), we should lose the first two observations
  expect_identical(a$Y.hat %>% head(2), c(NA_real_, NA_real_))
  expect_identical(a$Z.hat %>% head(2), c(NA_real_, NA_real_))
  expect_true(all(!is.na(a$Y.hat %>% tail(-2))))
  expect_true(all(!is.na(a$Z.hat %>% tail(-2))))
})

test_that("run_module() works with CVAR", {
  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Y", "Z"),
    independent = c("U", "U"),
    lag = c("", ""),
    cvar = c("system1", "system1")
  )
  mdl <- osem:::check_config_table(specification)
  opts <- mdl
  df <- readRDS(test_path("testdata", "cvar", "artificial_cvar_data.rds"))
  cls <- osem:::classify_variables(mdl)
  a <- run_module(
    module = mdl[1, ],
    data = df,
    classification = cls,
    use_logs = "none",
    trend = TRUE,
    opts_df = opts,
    keep = NULL,
    quiet = TRUE,
    cvar.ar = 2,
    freq = NULL,
    coint_deterministic = "const",
    coint_significance = "5pct"
  )
  expect_named(a, c("model", "data", "args", "indep", "dep", "diagnostics", "opts_df"))
  # a$model should be output of estimate_cvar(), check whether all elements are present
  expect_named(a$model, c("cointtest", "vecm", "varm", "rank", "urtest", "args"))
  # data should be output of add_to_original_data()
  expect_s3_class(a$data, c("data.frame", "tbl"))
  expect_true(all(c("index", "time", "trend", "Y", "Z", "U", "Y.hat", "Z.hat", "Y.level.hat", "Z.level.hat") %in% colnames(a$data)))
  expect_identical(a$data$Y.hat, a$data$Y.level.hat) # no log transformations
  expect_identical(a$data$Z.hat, a$data$Z.level.hat) # no log transformations
  expect_identical(a$dep, c("Y", "Z"))
  expect_identical(a$indep, "U")
  expect_identical(a$diagnostics, list(super.exogeneity = NA)) # should always be NA
  # first module run, so now should have added "log_opts" column to opts_df
  expect_named(a$opts_df, c("order", "type", "dependent", "independent", "lag", "cvar", "index", "log_opts"))
  expect_identical(a$opts_df[, 1:7], mdl)
  expect_type(a$opts_df %>% dplyr::pull(log_opts), "list")
  expect_s3_class(a$opts_df %>% dplyr::pull(log_opts) %>% dplyr::first(), c("data.frame", "tibble"))
  expect_identical(a$opts_df %>% dplyr::pull(log_opts) %>% dplyr::first(), dplyr::tibble(Y = NA, Z = NA, U = NA))

  #### test CVAR with log transformations
  a <- run_module(
    module = mdl[1, ],
    data = df,
    classification = cls,
    use_logs = "both",
    trend = TRUE,
    opts_df = opts,
    keep = NULL,
    quiet = TRUE,
    cvar.ar = 2,
    freq = NULL,
    coint_deterministic = "const",
    coint_significance = "5pct"
  )
  expect_named(a, c("model", "data", "args", "indep", "dep", "diagnostics", "opts_df"))
  # a$model should be output of estimate_cvar(), check whether all elements are present
  expect_named(a$model, c("cointtest", "vecm", "varm", "rank", "urtest", "args"))
  # data should be output of add_to_original_data()
  expect_s3_class(a$data, c("data.frame", "tbl"))
  expect_true(all(c("index", "time", "trend", "Y", "Z", "U", "ln.Y", "ln.Z", "Y.hat", "Z.hat", "Y.level.hat", "Z.level.hat") %in% colnames(a$data)))
  expect_identical(exp(a$data$Y.hat), a$data$Y.level.hat) # log transformation
  expect_identical(sinh(a$data$Z.hat), a$data$Z.level.hat) # asinh transformation
  expect_identical(a$dep, c("Y", "Z"))
  expect_identical(a$indep, "U")
  expect_identical(a$diagnostics, list(super.exogeneity = NA)) # should always be NA
  # first module run, so now should have added "log_opts" column to opts_df
  expect_named(a$opts_df, c("order", "type", "dependent", "independent", "lag", "cvar", "index", "log_opts"))
  expect_identical(a$opts_df[, 1:7], mdl)
  expect_type(a$opts_df %>% dplyr::pull(log_opts), "list")
  expect_s3_class(a$opts_df %>% dplyr::pull(log_opts) %>% dplyr::first(), c("data.frame", "tibble"))
  expect_identical(a$opts_df %>% dplyr::pull(log_opts) %>% dplyr::first(), dplyr::tibble(Y = "log", Z = "asinh", U = "asinh"))
})

test_that("update_data() works with CVAR", {
  specification <- dplyr::tibble(
    type = c("n", "n"),
    dependent = c("Y", "Z"),
    independent = c("U", "U"),
    lag = c("", ""),
    cvar = c("system1", "system1")
  )
  dictionary <- dplyr::tibble(
    model_varname = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "A", "B"),
    full_name = c("Y", "Z", "U", "V", "W", "Q", "R", "S", "T", "M", "A", "B"),
    database = c("local", "local", "local", "local", "local", "local", "local", "local", NA, "local", "local", "local"),
    geo = "DE",
    dataset_id = NA,
    freq = ""
  )
  mdl <- osem:::check_config_table(specification)
  opts <- mdl
  df <- readRDS(test_path("testdata", "cvar", "artificial_cvar_data.rds")) %>%
    dplyr::filter(na_item %in% c("Y", "Z", "U")) %>%
    dplyr::select(-geo)
  cls <- osem:::classify_variables(mdl)
  tmp_before <- df
  module_out <- run_module(
    module = mdl[1, ],
    data = df,
    classification = cls,
    use_logs = "none",
    trend = TRUE,
    opts_df = opts,
    keep = NULL,
    quiet = TRUE,
    cvar.ar = 2,
    freq = NULL,
    coint_deterministic = "const",
    coint_significance = "5pct"
  )
  tmp_after <- osem:::update_data(tmp_before, new_data = module_out$data)
  expect_s3_class(tmp_after, c("data.frame", "tibble"))
  expect_named(tmp_after, c("time", "na_item", "values"))
  expect_setequal(unique(tmp_after$na_item), c("Y", "Z", "U", "Y.hat", "Z.hat"))
  expect_identical(dim(tmp_after), c(500L, 3L))
})
