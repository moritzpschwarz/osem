options(timeout=1000)
test_that("no errors when running very simple model", {

  skip_on_ci()
  skip_on_cran()

  spec <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm"
    )
  )

  expect_silent(
    a <- run_model(
      specification = spec,
      dictionary = NULL,
      primary_source = "download",
      save_to_disk = NULL,
      present = FALSE,
      quiet = TRUE
    )
  )

  # trying to run AR = 0 as well
  expect_silent(
    a <- run_model(
      specification = spec,
      dictionary = NULL,
      primary_source = "local",
      save_to_disk = NULL,
      present = FALSE,
      quiet = TRUE,
      max.ar = 0,
      max.dl = 0
    )
  )

})


test_that("Check that single variable model works", {

  specification <- dplyr::tibble(
    type = c(
      "n"
    ),
    dependent = c(
      "FinConsExpHH"
    ),
    independent = c(
      ""
    )
  )

  set.seed(123)
  testdata <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), to = as.Date("2023-10-01"), by = "quarter"),
                            FinConsExpGov = rnorm(mean = 100, n = length(time)),
                            HICP_Gas = rnorm(mean = 200, n = length(time)),
                            FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.2))

  # now modify this to simulate the effect of providing external exogenous data but
  # where we do not provide any values for the values that would otherwise be nowcasted
  # so first we get the exogenous values based of a base case
  # then we set one value in the historical data to NA
  testdata %>%
    dplyr::mutate(FinConsExpHH = dplyr::case_when(time == as.Date("2023-10-01") ~ NA,
                                                  TRUE ~ FinConsExpHH)) %>%
    dplyr::mutate(HICP_Gas = dplyr::case_when(time == as.Date("2023-10-01") ~ NA,
                                              TRUE ~ HICP_Gas)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values") -> testdata_modified_long

  # before the fix on 25.07.2025 caused an error
  expect_output(
    run_model(specification = specification,
              dictionary = dict,
              input = testdata_modified_long,
              primary_source = "local",
              present = FALSE,
              quiet = FALSE,
              plot = FALSE,
              constrain.to.minimum.sample = TRUE),
    regexp = "Variable provided as 'inputdata_directory' seems to be a data.frame type. Used as data source"
  )
})
