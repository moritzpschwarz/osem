test_that("no errors when running very simple model", {

  spec <- tibble(
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

  expect_message(
    a <- run_model(
      specification = spec,
      dictionary = NULL,
      inputdata_directory = NULL,
      filter_list = filter_list,
      download = TRUE,
      #save_to_disk = here::here("input_data/test.xlsx"),
      save_to_disk = NULL,
      present = FALSE
    )
  )

  expect_output(print(a))

})

test_that("no errors when running a slightly more complicated model", {


  ## Test AR1 and fully exogenous ----

  spec <- tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "FinConsExpHH",
      "GCapitalForm",
      "Emissions",
      "GDP"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "",
      "FinConsExpGov",
      "GDP",
      ""
    )
  )

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR", varcolumn = "na_item")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR", varcolumn = "na_item")
  fc <- list(geo = "AT", unit = "THS_T", nace_r2 = "TOTAL_HH", varcolumn = "airpol")
  filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa, "GHG" = fc, "B1GQ" = fa)

  # Execute the first time to get the data
  # b <- run_model(
  #   specification = spec,
  #   dictionary = NULL,
  #   inputdata_directory = NULL,
  #   filter_list = filter_list,
  #   download = TRUE,
  #   save_to_disk = here::here("input_data/"),
  #   present = FALSE
  # )

  expect_message(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE
  ))

  expect_output(print(b))

  #checking what happens when download is false and inputdata is also false
  expect_error(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = FALSE,
    save_to_disk = NULL,
    present = FALSE
  ))


  #checking what happens when download is false and inputdata is also false
  expect_error(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = FALSE,
    save_to_disk = NULL,
    present = FALSE
  ))


  # Let's check that an ecm also works
  expect_message(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    ardl_or_ecm = "ecm"
  ))


})


test_that("Incorporate Emissions", {

  spec <- tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "FinConsExpHH",
      "GCapitalForm",
      "Emissions",
      "GDP"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "",
      "FinConsExpGov",
      "GDP",
      ""
    )
  )

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
  fc <- list(geo = "AT", unit = "THS_T", nace_r2 = "TOTAL_HH")
  filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa, "GHG" = fc, "B1GQ" = fa)

  # Execute the first time to get the data
  # b <- run_model(
  #   specification = spec,
  #   dictionary = NULL,
  #   inputdata_directory = NULL,
  #   filter_list = filter_list,
  #   download = TRUE,
  #   save_to_disk = here::here("input_data/"),
  #   present = FALSE
  # )

  expect_message(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE
  ))

  expect_output(print(b))



})


