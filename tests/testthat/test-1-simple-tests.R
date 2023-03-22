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

  # expect_message(
  #   a <- run_model(
  #     specification = spec,
  #     dictionary = NULL,
  #     inputdata_directory = NULL,
  #     filter_list = filter_list,
  #     download = TRUE,
  #     #save_to_disk = here::here("input_data/test.xlsx"),
  #     save_to_disk = NULL,
  #     present = FALSE
  #   ), regexp = NULL #regexp = "Reading cache file |trying URL| Table "
  # )

  expect_silent(
    a <- run_model(
      specification = spec,
      dictionary = NULL,
      inputdata_directory = NULL,
      filter_list = filter_list,
      download = TRUE,
      #save_to_disk = here::here("input_data/test.xlsx"),
      save_to_disk = NULL,
      present = FALSE,
      quiet = TRUE
    )
  )

  expect_output(print(a))

})

test_that("no errors when running a slightly more complicated model", {


  spec <- dplyr::tibble(
    type = c(
      "d",
      "d",
      "n",
      "n",
      "n"
    ),
    dependent = c(
      "StatDiscrep",
      "TOTS",
      "Import",
      "FinConsExpHH",
      "GCapitalForm"
    ),
    independent = c(
      "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "",
      "FinConsExpGov"
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

  expect_silent(run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE,
    quiet = TRUE
  ))




  ## Test AR1 and fully exogenous ----

  spec <- dplyr::tibble(
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
      "FinConsExpGov",
      "Emissions",
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

  expect_warning(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE
  ), "Table |Unbalanced panel")

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
  ), "Must specify 'inputdata_directory'")


  #checking what happens when download is false and inputdata is also false
  expect_error(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = FALSE,
    save_to_disk = NULL,
    present = FALSE
  ), "Must specify 'inputdata_directory'")


  # Let's check that an ecm also works
  expect_warning(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    ardl_or_ecm = "ecm"
  ), "Unbalanced panel")


})


test_that("Incorporate Emissions", {

  spec <- dplyr::tibble(
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

  expect_warning(b <- run_model(
    specification = spec,
    dictionary = NULL,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE
  ), "Unbalanced panel")

  expect_output(print(b))



})



test_that("Extensive Model", {


  spec <- dplyr::tibble(
    type = c(
      #"d",
      "d",
      "n",
      "n",
      "n",
      "n",
      "d",
      "n",
      "n",
      "d",
      "n",
      "n"
    ),
    dependent = c(
      #"StatDiscrep",
      "TOTS",
      "Import",
      "FinConsExpHH",
      "GCapitalForm",
      "Emissions",
      "GDP",
      "GValueAddGov", # as in NAM, technical relationship
      "GValueAddManuf", # more complicated in NAM, see 2.3.3 and 6.3.1
      "DomDemand", # as in NAM
      "GValueAddConstr" ,
      "GValueAddWholesaletrade"
    ),
    independent = c(
      #"TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
      "GValueAdd + Import",
      "FinConsExpHH + GCapitalForm",
      "",
      "FinConsExpHH",
      "GDP",
      "GValueAddGov + GValueAddAgri + GValueAddIndus + GValueAddConstr + GValueAddWholesaletrade + GValueAddInfocom + GValueAddFinance + GValueAddRealest + GValueAddResearch + GValueAddArts",
      "FinConsExpGov", # as in NAM, technical relationship
      "DomDemand + Export + LabCostManuf", # NAM uses 'export market indicator' not exports - unclear what this is, NAM uses unit labour cost in NOR manufacturing relative to the foreign price level - here is just total labour cost
      "FinConsExpHH + FinConsExpGov + GCapitalForm",
      "DomDemand + LabCostConstr + BuildingPermits", # in NAM some form of YFP2J = 0.3JBOL + 0.2JF P N + 0.3JO + 0.3JOIL. Unclear what this is. Using Building Permits instead
      "DomDemand + Export + LabCostService"
    )
  )

  fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
  fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
  fc <- list(geo = "AT", unit = "THS_T")
  fd <- list(geo = "AT", s_adj = "SCA")
  fe <- list(geo = "AT", s_adj = "SCA", unit = "I15")
  ff <- list(geo = "AT", s_adj = "SCA", unit = "I16")

  filter_list <- list(
    "P7" = fa,
    "YA0" = fb,
    "P31_S14_S15" = fa,
    "P5G" = fa,
    "B1G" = fa,
    "P3_S13" = fa,
    "P6" = fa,
    "GHG" = fc,
    "B1GQ" = fa,
    "PSQM" = fe,
    "LM-LCI-TOT" = ff
  )



  # ab <- run_model(
  #   specification = spec,
  #   dictionary = dict.devel,
  #   filter_list = filter_list,
  #   save_to_disk = "data/use/Test3.xlsx",
  #   download = TRUE,
  #   present = FALSE
  # )

  # ab <- run_model(
  #   specification = spec,
  #   dictionary = dict.devel,
  #   inputdata_directory = "data/use",
  #   filter_list = filter_list,
  #   download = FALSE,
  #   present = FALSE,
  #   saturation.tpval = 0.1
  # )





  expect_warning(ab <- run_model(
    specification = spec,
    dictionary = dict,
    inputdata_directory = NULL,
    filter_list = filter_list,
    download = TRUE,
    save_to_disk = NULL,
    present = FALSE,
    saturation.tpval = 0.1/NROW(clean_data)
  ), "Unbalanced panel")

  abf <- forecast_model(ab)
  plot(abf, order.as.run = TRUE)





})


