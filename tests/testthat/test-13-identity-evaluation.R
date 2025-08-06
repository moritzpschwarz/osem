

test_that("the order of the equation should not matter", {

  # GValueAdd / Import + FinConsExpHH
  specification <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "TOTS",
      "Import"
    ),
    independent = c(
      "GValueAdd / Import + FinConsExpHH",
      "FinConsExpHH + GCapitalForm"
    )
  )

  modified_4 <- run_model(specification = specification,
                          input = test_path("testdata", "ragged_edge", "ragged_edge_emissions_data.csv"),
                          primary_source = "local",
                          plot = FALSE,
                          quiet = TRUE)



  # FinConsExpHH + GValueAdd / Import
  specification <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "TOTS",
      "Import"
    ),
    independent = c(
      "FinConsExpHH + GValueAdd / Import ",
      "FinConsExpHH + GCapitalForm"
    )
  )

  modified_5 <- run_model(specification = specification,
                          input = test_path("testdata", "ragged_edge", "ragged_edge_emissions_data.csv"),
                          primary_source = "local",
                          plot = FALSE,
                          quiet = TRUE)




  set.seed(123)
  fc_4 <- forecast_model(modified_4, plot = FALSE, quiet = TRUE)
  set.seed(123)
  fc_5 <- forecast_model(modified_5, plot = FALSE, quiet = TRUE)

  expect_identical(modified_4$full_data, modified_5$full_data)
  expect_identical(fc_4$forecast$central.estimate, fc_5$forecast$central.estimate)
  expect_identical(fc_4$forecast$all.estimates, fc_5$forecast$all.estimates)
  expect_identical(round(fc_4$forecast$central.estimate[[2]]$TOTS,4),
                   c(38496.5241, 38276.3459, 38182.5269, 37973.051, 37820.3351,
                     37694.0061, 37494.0114, 37350.8693, 37210.0018, 37062.3174))
  # fc_4$forecast$data[[2]]
  # fc_5$forecast$data[[2]]




})

