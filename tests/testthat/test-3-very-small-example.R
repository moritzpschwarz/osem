options(timeout=1000)
test_that("no errors when running very simple model", {

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
      inputdata_directory = NULL,
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
      inputdata_directory = NULL,
      primary_source = "local",
      save_to_disk = NULL,
      present = FALSE,
      quiet = TRUE,
      max.ar = 0,
      max.dl = 0
    )
  )

})
