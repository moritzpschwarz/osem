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

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
  filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)

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

})
