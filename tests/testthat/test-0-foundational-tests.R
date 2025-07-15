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
  expect_setequal(colnames(out), c("order", "type", "dependent", "independent", "lag", "cvar"))

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
  expect_setequal(colnames(out), c("order", "type", "dependent", "independent", "lag", "cvar"))
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
