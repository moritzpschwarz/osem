

test_that("Error messages are working",{


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

  expect_error(run_model(specification = spec, gets_selection = NULL), "must be logical")
  expect_error(run_model(specification = spec, saturation = NULL), "must be logical")

  expect_error(run_model(specification = spec, gets_selection = "NULL"), "must be logical")
  expect_error(run_model(specification = spec, saturation = "NULL"), "must be logical")

  expect_error(run_model(specification = spec, gets_selection = 1), "must be logical")
  expect_error(run_model(specification = spec, saturation = 0.5), "must be logical")



})
