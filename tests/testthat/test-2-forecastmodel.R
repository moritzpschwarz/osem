
test_that("Test that forecasting works",{

  ## Test AR1 and fully exogenous ----

  spec <- tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "JL",
      "TOTS",
      "B",
      "CP",
      "J"
    ),
    independent = c(
      "TOTS - CP - CO - J - A",
      "YF + B",
      "CP + J",
      "",
      "CO"
    )
  )

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
  filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

  b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  )


  expect_warning(forecast_model(b), regexp = "No exogenous values")

})
