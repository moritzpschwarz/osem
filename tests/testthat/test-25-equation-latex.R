# test-25-equation-latex.R

# Helper ----------------------------------------------------------------------

render_equation_latex <- function(...) {
  out <- NULL
  capture.output(out <- equation_latex(...))
  out
}


# Basic lm behaviour ----------------------------------------------------------

test_that("equation_latex() returns valid display math for a simple lm", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(50),
    x = rnorm(50),
    z = rnorm(50)
  )

  fit <- lm(y ~ x + z, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_stars = FALSE
  )

  expect_type(eq, "character")
  expect_length(eq, 1)

  expect_true(startsWith(eq, "$$"))
  expect_true(endsWith(eq, "$$"))

  expect_true(grepl("\\begin{aligned}", eq, fixed = TRUE))
  expect_true(grepl("\\end{aligned}", eq, fixed = TRUE))

  expect_true(grepl("\\text{y}_t", eq, fixed = TRUE))
  expect_true(grepl("\\text{x}_t", eq, fixed = TRUE))
  expect_true(grepl("\\text{z}_t", eq, fixed = TRUE))
  expect_true(grepl("+u_t", eq, fixed = TRUE))
})


test_that("equation_latex() puts non-ECM intercept first", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(80),
    x = rnorm(80)
  )

  fit <- lm(y ~ x, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_stars = FALSE
  )

  rhs <- sub("^.*&= ", "", eq)
  rhs <- sub("\n\\\\end\\{aligned\\}\n\\$\\$$", "", rhs)

  # First RHS term should be numeric intercept, not the x term.
  expect_false(startsWith(rhs, "+"))
  expect_false(startsWith(rhs, "-0."))
  expect_true(grepl("\\text{x}_t", rhs, fixed = TRUE))
})


test_that("equation_latex() supports significance stars for lm objects", {

  set.seed(123)

  dat <- data.frame(
    x = rnorm(100)
  )
  dat$y <- 1 + 2 * dat$x + rnorm(100, sd = 0.1)

  fit <- lm(y ~ x, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_stars = TRUE
  )

  expect_true(grepl("^{***}", eq, fixed = TRUE))
})


test_that("equation_latex() wraps long equations when width is supplied", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100),
    x2 = rnorm(100),
    x3 = rnorm(100),
    x4 = rnorm(100),
    x5 = rnorm(100)
  )

  fit <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "y",
    width = 60
  )

  expect_true(grepl("\\begin{aligned}", eq, fixed = TRUE))
  expect_true(grepl("\\end{aligned}", eq, fixed = TRUE))
  expect_true(grepl("\\\\", eq, fixed = TRUE))
  expect_true(grepl("&\\quad", eq, fixed = TRUE))
})


# Variable formatting ---------------------------------------------------------

test_that("equation_latex() escapes underscores in variable names", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(50),
    HICP_AviaInt = rnorm(50)
  )

  fit <- lm(y ~ HICP_AviaInt, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "D.ln.Flights"
  )

  expect_true(grepl("\\Delta \\ln(\\text{Flights})_t", eq, fixed = TRUE))
  expect_true(grepl("\\text{HICP\\_AviaInt}_t", eq, fixed = TRUE))
})


test_that("equation_latex() formats differenced and logged variables", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(50),
    `D.ln.RealConsHH` = rnorm(50),
    check.names = FALSE
  )

  fit <- lm(y ~ `D.ln.RealConsHH`, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "D.ln.Flights"
  )

  expect_true(grepl("\\Delta \\ln(\\text{Flights})_t", eq, fixed = TRUE))
  expect_true(grepl("\\Delta \\ln(\\text{RealConsHH})_t", eq, fixed = TRUE))
})


# Indicators and seasonals ----------------------------------------------------

test_that("equation_latex() can show indicators", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(50),
    x = rnorm(50),
    `sis2020-04-01` = as.numeric(seq_len(50) == 20),
    check.names = FALSE
  )

  fit <- lm(y ~ x + `sis2020-04-01`, data = dat)

  eq_show <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_indicators = TRUE
  )

  eq_hide <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_indicators = FALSE,
  )

  expect_true(grepl("SIS_{2020Q2}", eq_show, fixed = TRUE))
  expect_false(grepl("SIS_{2020Q2}", eq_hide, fixed = TRUE))
})


test_that("equation_latex() adds stars to indicators when requested", {

  set.seed(123)

  dat <- data.frame(
    x = rnorm(80),
    `sis2020-04-01` = as.numeric(seq_len(80) == 40),
    check.names = FALSE
  )

  #dat$y <- 0.2 * dat$x + 5 * dat$`sis2020-04-01` + rnorm(80, sd = 0.1)
  dat$y <- 0.2 * dat$x + 50 * dat$`sis2020-04-01` + rnorm(80, sd = 0.001)
  fit <- lm(y ~ x + `sis2020-04-01`, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_indicators = TRUE,
    show_stars = TRUE
  )

  expect_true(grepl("^{***}\\,SIS_{2020Q2}", eq, fixed = TRUE))
})


test_that("equation_latex() can show seasonal dummies", {

  set.seed(123)

  dat <- data.frame(
    y = rnorm(60),
    x = rnorm(60),
    q_2 = rep(c(0, 1, 0, 0), length.out = 60),
    q_3 = rep(c(0, 0, 1, 0), length.out = 60)
  )

  fit <- lm(y ~ x + q_2 + q_3, data = dat)

  eq_show <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_seasonals = TRUE
  )

  eq_hide <- render_equation_latex(
    fit,
    lhs_name = "y",
    show_seasonals = FALSE,
  )

  expect_true(grepl("Q_{2,t}", eq_show, fixed = TRUE))
  expect_true(grepl("Q_{3,t}", eq_show, fixed = TRUE))
  expect_false(grepl("Q_{2,t}", eq_hide, fixed = TRUE))
})


# ECM representation ----------------------------------------------------------

test_that("equation_latex() detects ECM representation automatically", {

  set.seed(123)

  n <- 100

  dat <- data.frame(
    `D.ln.y` = rnorm(n),
    `L1.ln.y` = rnorm(n),
    `L1.ln.x` = rnorm(n),
    `D.ln.x` = rnorm(n),
    check.names = FALSE
  )

  fit <- lm(`D.ln.y` ~ `L1.ln.y` + `L1.ln.x` + `D.ln.x`, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "D.ln.y"
  )

  expect_true(grepl("\\left(", eq, fixed = TRUE))
  expect_true(grepl("\\right)", eq, fixed = TRUE))

  expect_true(grepl("\\ln(\\text{y})_{t-1}", eq, fixed = TRUE))
  expect_true(grepl("\\ln(\\text{x})_{t-1}", eq, fixed = TRUE))
  expect_true(grepl("\\Delta \\ln(\\text{x})_t", eq, fixed = TRUE))
})


test_that("equation_latex() adds stars to the ECM adjustment coefficient", {

  set.seed(123)

  n <- 150

  dat <- data.frame(
    `L1.ln.y` = rnorm(n),
    `L1.ln.x` = rnorm(n),
    `D.ln.x` = rnorm(n),
    check.names = FALSE
  )

  dat$`D.ln.y` <- -0.8 * dat$`L1.ln.y` +
    0.4 * dat$`L1.ln.x` +
    0.3 * dat$`D.ln.x` +
    rnorm(n, sd = 0.1)

  fit <- lm(`D.ln.y` ~ `L1.ln.y` + `L1.ln.x` + `D.ln.x`, data = dat)

  eq <- render_equation_latex(
    fit,
    lhs_name = "D.ln.y",
    show_stars = TRUE
  )

  expect_true(grepl("^{***}\\left(", eq, fixed = TRUE))
})


# Full OSEM object behaviour --------------------------------------------------


test_that("equation_latex() extracts the requested module from an osem object", {

  set.seed(123)

  dat1 <- data.frame(y = rnorm(50), x = rnorm(50))
  dat2 <- data.frame(z = rnorm(50), w = rnorm(50))

  fit_y <- lm(y ~ x, data = dat1)
  fit_z <- lm(z ~ w, data = dat2)

  fake_osem <- structure(
    list(
      module_collection = dplyr::tibble(
        dependent = c("y", "z"),
        model = list(fit_y, fit_z),
        type = "n"
      )
    ),
    class = "osem"
  )

  eq_y <- render_equation_latex(
    fake_osem,
    lhs_name = "y"
  )

  eq_z <- render_equation_latex(
    fake_osem,
    lhs_name = "z"
  )

  expect_true(grepl("\\text{y}_t", eq_y, fixed = TRUE))
  expect_true(grepl("\\text{x}_t", eq_y, fixed = TRUE))
  expect_false(grepl("\\text{w}_t", eq_y, fixed = TRUE))

  expect_true(grepl("\\text{z}_t", eq_z, fixed = TRUE))
  expect_true(grepl("\\text{w}_t", eq_z, fixed = TRUE))
  expect_false(grepl("\\text{x}_t", eq_z, fixed = TRUE))
})


test_that("equation_latex() errors cleanly if lhs_name is not in an osem object", {

  fit <- lm(y ~ x, data = data.frame(y = rnorm(20), x = rnorm(20)))

  fake_osem <- structure(
    list(
      module_collection = dplyr::tibble(
        dependent = "y",
        type = "n",
        model = list(fit)
      )
    ),
    class = "osem"
  )

  expect_error(render_equation_latex(fake_osem, lhs_name = "missing"),regexp = "No module found ")
})


# Integration test using sim_cointegration ------------------------------------

test_that("equation_latex() works on sim_cointegration OSEM models", {

  skip_on_cran()

  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  skip_if_not_installed("purrr")
  skip_if_not_installed("gets")

  # helper_path <- test_path("helper-sim_cointegration.R")
  #
  # skip_if_not(
  #   file.exists(helper_path),
  #   "sim_cointegration.R helper not available"
  # )
  #
  # source(helper_path)
  #
  # skip_if_not(
  #   exists("run_model", mode = "function"),
  #   "run_model() not available"
  # )
  #
  # sim_data <- simulate_real_world_ecm_auto_case(nobs = 120, seed = 123)

  # specification <- make_real_world_ecm_specification()

  # dictionary <- make_real_world_ecm_dictionary(
  #   vars = unique(c(
  #     specification$dependent,
  #     unlist(strsplit(gsub(" ", "", specification$independent), "\\+"))
  #   ))
  # )
  # coint_data <- list(sim_data = sim_data, specification = specification, dictionary = dictionary)
  # saveRDS(coint_data, file = test_path("testdata","cointegration","cointegration_data.rds"))
  coint_data <- readRDS(test_path("testdata","cointegration","cointegration_data.rds"))

  model <- run_model(
    specification = coint_data$specification,
    dictionary = coint_data$dictionary,
    input = coint_data$sim_data %>% tidyr::pivot_longer(-"time", names_to = "na_item", values_to = "values"),
    primary_source = "local",
    use_logs = "both",
    ardl_or_ecm = "ecm",
    ecm_pretest = "auto",
    trend = TRUE,
    max.ar = 2,
    max.dl = 2,
    saturation = c("IIS", "SIS"),
    saturation.tpval = 0.01,
    gets_selection = TRUE,
    selection.tpval = 0.01,
    present = FALSE,
    plot = FALSE,
    quiet = TRUE
  )

  expect_true("opts_df" %in% names(model))
  expect_true("module_collection" %in% names(model))
  expect_true(all(c("dependent", "model") %in% names(model$module_collection)))

  dep <- "ConsHH"

  eq_from_osem <- render_equation_latex(
    model,
    lhs_name = dep,
    show_indicators = FALSE,
    show_seasonals = FALSE,
    show_stars = TRUE,
    width = 100
  )

  module_model <- model$module_collection %>%
    dplyr::filter(.data$dependent == dep) %>%
    dplyr::pull(.data$model) %>%
    purrr::pluck(1)

  eq_from_module <- render_equation_latex(
    module_model,
    lhs_name = dep,
    show_indicators = FALSE,
    show_seasonals = FALSE,
    show_stars = TRUE,
    width = 100
  )

  expect_identical(eq_from_osem, eq_from_module)

  expect_true(startsWith(eq_from_osem, "$$"))
  expect_true(endsWith(eq_from_osem, "$$"))
  expect_true(grepl("\\begin{aligned}", eq_from_osem, fixed = TRUE))
  expect_true(grepl("+u_t", eq_from_osem, fixed = TRUE))
})
