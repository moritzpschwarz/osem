

specification <- dplyr::tibble(
  type = c(
    "n"
  ),
  dependent = c(
    "FinConsExpHH"
  ),
  independent = c(
    "FinConsExpGov + HICP_Gas"
  )
)

time_to_sim <- 30


# Simulated Data ----------------------------------------------------------


set.seed(123)
daily <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "day"),
                       FinConsExpGov = rnorm(mean = 100, n = length(time)),
                       HICP_Gas = rnorm(mean = 200, n = length(time)),
                       FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.1)) %>%
  tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")

monthly <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "month"),
                         FinConsExpGov = rnorm(mean = 100, n = length(time)),
                         HICP_Gas = rnorm(mean = 200, n = length(time)),
                         FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.1)) %>%
  tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")

quarterly <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "quarter"),
                           FinConsExpGov = rnorm(mean = 100, n = length(time)),
                           HICP_Gas = rnorm(mean = 200, n = length(time)),
                           FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.1)) %>%
  tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")

annualy <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "year"),
                         FinConsExpGov = rnorm(mean = 100, n = length(time)),
                         HICP_Gas = rnorm(mean = 200, n = length(time)),
                         FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.1)) %>%
  tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")




test_that("Test the reaction of the model for different frequencies",{

  model_d <- run_model(specification = specification,
                       inputdata_directory = daily,
                       primary_source = "local",
                       max.ar = 1, max.dl = 0,
                       quiet = TRUE)

  model_m <- run_model(specification = specification,
                       inputdata_directory = monthly,
                       primary_source = "local",
                       max.ar = 1, max.dl = 0,
                       quiet = TRUE)

  model_q <- run_model(specification = specification,
                       inputdata_directory = quarterly,
                       primary_source = "local",
                       max.ar = 1, max.dl = 0,
                       quiet = TRUE)

  model_a <- run_model(specification = specification,
                       inputdata_directory = annualy,
                       primary_source = "local",
                       max.ar = 1, max.dl = 0,
                       quiet = TRUE)

  expect_error(forecast_model(model_d),regexp = "Mixed frequency forecasts or forecasts with daily or monthly data are not yet implemented.")
  expect_error(forecast_model(model_m),regexp = "Mixed frequency forecasts or forecasts with daily or monthly data are not yet implemented.")

  fc_q <- forecast_model(model_q, quiet = TRUE, plot.forecast = FALSE)
  fc_a <- forecast_model(model_a, quiet = TRUE, plot.forecast = FALSE)

  expect_equal(fc_q$forecast$data[[1]]$time, structure(c(15522, 15614, 15706, 15796, 15887, 15979, 16071, 16161, 16252, 16344), class = "Date"))
  expect_equal(fc_a$forecast$data[[1]]$time, structure(c(23741, 24106, 24472, 24837, 25202, 25567, 25933, 26298, 26663, 27028), class = "Date"))

  expect_equal(unique(rev(fc_q$full_forecast_data$time)), structure(c(12784, 12874, 12965, 13057, 13149, 13239, 13330, 13422, 13514, 13604, 13695, 13787, 13879, 13970, 14061, 14153, 14245, 14335, 14426, 14518, 14610, 14700, 14791, 14883, 14975, 15065, 15156, 15248, 15340, 15431, 15522, 15614, 15706, 15796, 15887, 15979, 16071, 16161, 16252, 16344), class = "Date"))
  expect_equal(unique(rev(fc_a$full_forecast_data$time)), structure(c(12784, 13149, 13514, 13879, 14245, 14610, 14975, 15340, 15706, 16071, 16436, 16801, 17167, 17532, 17897, 18262, 18628, 18993, 19358, 19723, 20089, 20454, 20819, 21184, 21550, 21915, 22280, 22645, 23011, 23376, 23741, 24106, 24472, 24837, 25202, 25567, 25933, 26298, 26663, 27028), class = "Date"))
})



# Real Data ---------------------------------------------------------------

test_that("Annual Models run with EUROSTAT data",{


  specification <- dplyr::tibble(
    type = c(
      "n"
    ),
    dependent = c(
      "EmiCO2Combustion"
    ),
    independent = c(
      "FinConsExpHH + GCapitalForm"
    )
  )

  osem::dict %>%
    dplyr::mutate(dataset_id = dplyr::case_when(model_varname == "FinConsExpHH" ~ "nama_10_gdp",
                                                model_varname == "GCapitalForm" ~ "nama_10_gdp",
                                                TRUE ~ dataset_id)) -> dict_new

  testdata_annual <- structure(list(
    unit = c("CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR", "CLV05_MEUR",
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA),
    na_item = c("GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "GCapitalForm",
                "GCapitalForm", "GCapitalForm", "GCapitalForm", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "FinConsExpHH", "FinConsExpHH", "FinConsExpHH", "FinConsExpHH",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion",
                "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion", "EmiCO2Combustion"
    ),
    geo = c("AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
            "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT"),
    time = structure(c(788918400,
                       820454400, 852076800, 883612800, 915148800, 946684800, 978307200,
                       1009843200, 1041379200, 1072915200, 1104537600, 1136073600, 1167609600,
                       1199145600, 1230768000, 1262304000, 1293840000, 1325376000, 1356998400,
                       1388534400, 1420070400, 1451606400, 1483228800, 1514764800, 1546300800,
                       1577836800, 1609459200, 1640995200, 1672531200, 788918400, 820454400,
                       852076800, 883612800, 915148800, 946684800, 978307200, 1009843200,
                       1041379200, 1072915200, 1104537600, 1136073600, 1167609600, 1199145600,
                       1230768000, 1262304000, 1293840000, 1325376000, 1356998400, 1388534400,
                       1420070400, 1451606400, 1483228800, 1514764800, 1546300800, 1577836800,
                       1609459200, 1640995200, 1672531200, 0, 7776000, 15638400, 23587200,
                       31536000, 39312000, 47174400, 55123200, 63072000, 70934400, 78796800,
                       86745600, 94694400, 102470400, 110332800, 118281600, 126230400,
                       134006400, 141868800, 149817600, 157766400, 165542400, 173404800,
                       181353600, 189302400, 197164800, 205027200, 212976000, 220924800,
                       228700800, 236563200, 244512000, 252460800, 260236800, 268099200,
                       276048000, 283996800, 291772800, 299635200, 307584000, 315532800,
                       323395200, 331257600, 339206400, 347155200, 354931200, 362793600,
                       370742400, 378691200, 386467200, 394329600, 402278400, 410227200,
                       418003200, 425865600, 433814400, 441763200, 449625600, 457488000,
                       465436800, 473385600, 481161600, 489024000, 496972800, 504921600,
                       512697600, 520560000, 528508800, 536457600, 544233600, 552096000,
                       560044800, 567993600, 575856000, 583718400, 591667200, 599616000,
                       607392000, 615254400, 623203200, 631152000, 638928000, 646790400,
                       654739200, 662688000, 670464000, 678326400, 686275200, 694224000,
                       702086400, 709948800, 717897600, 725846400, 733622400, 741484800,
                       749433600, 757382400, 765158400, 773020800, 780969600, 788918400,
                       796694400, 804556800, 812505600, 820454400, 828316800, 836179200,
                       844128000, 852076800, 859852800, 867715200, 875664000, 883612800,
                       891388800, 899251200, 907200000, 915148800, 922924800, 930787200,
                       938736000, 946684800, 954547200, 962409600, 970358400, 978307200,
                       986083200, 993945600, 1001894400, 1009843200, 1017619200, 1025481600,
                       1033430400, 1041379200, 1049155200, 1057017600, 1064966400, 1072915200,
                       1080777600, 1088640000, 1096588800, 1104537600, 1112313600, 1120176000,
                       1128124800, 1136073600, 1143849600, 1151712000, 1159660800, 1167609600,
                       1175385600, 1183248000, 1191196800, 1199145600, 1207008000, 1214870400,
                       1222819200, 1230768000, 1238544000, 1246406400, 1254355200, 1262304000,
                       1270080000, 1277942400, 1285891200, 1293840000, 1301616000, 1309478400,
                       1317427200, 1325376000, 1333238400, 1341100800, 1349049600, 1356998400,
                       1364774400, 1372636800, 1380585600, 1388534400, 1396310400, 1404172800,
                       1412121600, 1420070400, 1427846400, 1435708800, 1443657600, 1451606400,
                       1459468800, 1467331200, 1475280000, 1483228800, 1491004800, 1498867200,
                       1506816000, 1514764800, 1522540800, 1530403200, 1538352000, 1546300800,
                       1554076800, 1561939200, 1569888000, 1577836800, 1585699200, 1593561600,
                       1601510400, 1609459200, 1617235200, 1625097600, 1633046400, 1640995200,
                       1648771200, 1656633600, 1664582400), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
    values = c(52556.4, 52957.7, 53893, 55423.1,
               57317.6, 58753.5, 58252.5, 56021.7, 58625.7, 59601.5, 60534,
               61881.2, 66497.2, 66137.9, 59288.1, 59182.5, 64393.9, 63941.6,
               63471.9, 63918.3, 65990.3, 68937.7, 72024, 76389.2, 75856.1,
               71750.6, 79527.7, 78975, 74783.5, 112118.8, 115413.9, 115898.7,
               119265.9, 122149.2, 125955.9, 127569.3, 128439.3, 130677.8, 133710.7,
               136717.1, 139577.7, 141118.3, 142453.7, 143665.5, 145094.1, 147045.7,
               147813.2, 147696.4, 148144.9, 148867.8, 151168.3, 154126, 155832.2,
               156687.2, 143323.2, 149365.8, 157825, 157392.5, 14175.597688013,
               9836.55493160127, 9069.4884773141, 13345.2619834244, 14846.8779063184,
               10369.3011929264, 9619.36307670027, 13851.7666482795, 15294.963066914,
               10833.3854914676, 10058.1356327401, 14301.3873757292, 16431.2385175357,
               11430.6167036882, 10614.4334275544, 15382.7668356804, 15539.1021206553,
               10890.8029926198, 10056.6675175287, 14550.7230100602, 15128.4671560627,
               10477.2575367268, 9723.0154389957, 14183.2619600177, 16583.9357656865,
               11406.0553527014, 10618.8460701199, 15434.5451706042, 15565.5069142683,
               10995.6319134071, 10215.5495153998, 14627.2208361599, 16267.6590717671,
               11449.0084208461, 10655.5944283446, 15293.7232896876, 16916.8115498522,
               11932.8058141751, 11123.3712772256, 15943.1560213631, 16584.0784930894,
               11427.9755909648, 10654.6018956851, 15688.6231896196, 17298.1593067575,
               10444.9420480594, 8816.26371619238, 14995.1854330095, 16722.4916860156,
               10660.7571653721, 8226.9055665028, 14167.5588985164, 16084.3876976316,
               10204.2713384402, 8442.21533065167, 14669.2416369229, 16550.6747597478,
               11063.2859976327, 9188.82572030314, 14167.4316087439, 17799.8667891515,
               10903.6585179313, 8841.38287695982, 15104.1108551713, 17369.8375920182,
               10399.4914488053, 8964.14323655091, 14907.5195038025, 17615.8668317454,
               11256.5554654459, 8963.48154421277, 14551.6445131227, 16086.6426652114,
               10676.0072550318, 9024.32721152832, 14553.371481531, 15663.4823028331,
               10873.9558705501, 9142.38047177933, 14716.5356211547, 17511.7054259968,
               11972.928095036, 10495.4541770309, 16262.5921860235, 19210.1548161633,
               13062.913130036, 10683.9428486547, 17461.2159414674, 17610.4493307751,
               11737.3787971042, 10056.7480219616, 16158.4161554358, 17736.5618513278,
               11570.3755933647, 10479.955786261, 15985.0607991245, 17780.3559083018,
               12046.0324614922, 10234.3232580004, 16125.4647339067, 18564.2467508906,
               12909.3697736751, 11360.6156842041, 16721.1297903277, 20347.175947673,
               13114.8559157545, 12145.3595770553, 17661.5336754737, 19653.4817318869,
               13426.8836268666, 11571.8919314216, 17810.8791189103, 19628.5398903506,
               13179.8084298984, 11815.5915999581, 18450.1453325765, 19380.7905495748,
               13036.7862766856, 11301.4741814766, 17729.0455580176, 19807.7110291067,
               12830.3497038929, 11785.2447942955, 17403.2767337381, 19833.7559799752,
               14247.2313968598, 12607.4147678593, 19506.0294745351, 20313.9422344195,
               14818.2553523048, 13341.762687525, 19164.2584697573, 22276.1005021244,
               15417.4224118708, 14309.0127715569, 20711.3663562869, 22513.2233286817,
               16535.2740264965, 14461.4355002954, 20502.2040662235, 22569.5841828841,
               16306.9082987444, 14700.7549754948, 20868.1949704603, 22899.4551910475,
               15471.3468455729, 14160.371530857, 19619.2184669194, 19790.1304841359,
               15026.2267806444, 14338.2708005912, 19962.3697955645, 20236.0461452174,
               15012.3237516968, 14123.822010844, 19229.5229084159, 18866.1494324406,
               13496.2854939954, 12318.4925704722, 17940.7299538504, 20023.980445513,
               15145.3887669373, 13840.8279323451, 19332.9073503591, 19948.706262584,
               15066.838907399, 13270.4409102007, 18324.0040124158, 18757.3989981324,
               14068.1572816783, 12753.9011493272, 17859.5510556265, 19174.9026574395,
               14431.8864434811, 13029.6862606859, 17460.2042167926, 17071.8186973067,
               13953.0411694103, 12684.0721360015, 16706.5246288538, 17879.0043106231,
               13666.9436586921, 12643.4073460136, 17364.3648538143, 17815.0124755873,
               13814.2707143814, 12392.2834008, 17946.5252344025, 19415.437830094,
               14151.9130170292, 12894.1789396974, 17696.1419813414, 18300.9587805413,
               13667.4190868271, 12513.742169421, 16822.2690710381, 18444.0433680085,
               14188.3213344043, 12955.7002081601, 17138.7091336328, 17021.7734696453,
               12874.0395869261, 11683.0994280262, 15742.4921598523, 18197.1728652289,
               13218.7606761543, 12024.3957016646, 16554.9047761597, 17246.2016120506,
               12091.2922992026, 11095.2575219905, 15427.8237698063)),
    class = c("tbl_df",
              "tbl", "data.frame"), row.names = c(NA, -270L))



  test <- run_model(specification = specification,
                    dictionary = dict_new,
                    #save_to_disk = "data-raw/test/test.xlsx",
                    primary_source = "local",
                    inputdata_directory = testdata_annual %>% dplyr::mutate(time = as.Date(time)),
                    max.ar = 1, # for annual models, would not go beyond 1 (otherwise sample is too short)
                    max.dl = 1, # for annual models, would not go beyond 1 (otherwise sample is too short)
                    max.block.size = 5,
                    use_logs = "both",
                    constrain.to.minimum.sample = FALSE)

  test_fc <- forecast_model(test, plot.forecast = FALSE)

  expect_equal(test_fc$forecast$central.estimate[[1]]$time, structure(c(19723, 20089, 20454, 20819, 21184, 21550, 21915,
                                                                        22280, 22645, 23011), class = "Date"))
  plot(test_fc, exclude.exogenous = TRUE)

})






# Mixed Frequency ---------------------------------------------------------

test_that("Mixed Frequency across variables (different variables have different mixed frequencies)",{

  mixed_freq <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "year"),
                              FinConsExpGov = rnorm(mean = 100, n = length(time)),
                              HICP_Gas = rnorm(mean = 200, n = length(time)),
                              FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.1)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")

  # modify frequency for HICP_Gas
  mixed_freq <- mixed_freq %>%
    dplyr::filter(na_item != "HICP_Gas") %>%
    dplyr::bind_rows(mixed_freq %>%
                       dplyr::filter(na_item == "HICP_Gas") %>%
                       dplyr::mutate(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "day")))

  expect_error(
    model_mf <- run_model(specification = specification,
                          inputdata_directory = mixed_freq,
                          primary_source = "local",
                          max.ar = 1, max.dl = 0,
                          constrain.to.minimum.sample = FALSE,
                          quiet = TRUE),
    regexp = "Mixed frequency models are not yet implemented.")

})



test_that("Mixed Frequency within variable (same variables has different mixed frequencies)",{

  mixed_freq_orig <- dplyr::tibble(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "3 months"),
                                   FinConsExpGov = rnorm(mean = 100, n = length(time)),
                                   HICP_Gas = rnorm(mean = 200, n = length(time)),
                                   FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 * HICP_Gas + rnorm(length(time),mean = 0, sd = 0.1)) %>%
    tidyr::pivot_longer(-time, names_to = "na_item", values_to = "values")

  ###
  # simplest case: add a single annual observation to the start of an otherwise quarterly series
  ###

  # modify frequency for HICP_Gas
  mixed_freq <- mixed_freq_orig %>%
    dplyr::bind_rows(
      dplyr::tibble(na_item = "HICP_Gas",
                    time = as.Date("2004-01-01")) %>%
        dplyr::mutate(values = rnorm(dplyr::n()))
    ) %>%
    dplyr::arrange(na_item, time)

  expect_silent(model_mf1 <- run_model(specification = specification,
                                       inputdata_directory = mixed_freq,
                                       primary_source = "local",
                                       max.ar = 1, max.dl = 0,
                                       constrain.to.minimum.sample = FALSE,
                                       quiet = TRUE))

  # expect the 2004-01-01 to be dropped
  model_mf1_times <- model_mf1$full_data %>% dplyr::filter(na_item == "HICP_Gas") %>% dplyr::pull(time)
  expect_false("2004-01-01" %in% model_mf1_times)
  # expect all other observations to be there
  expect_equal(seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "3 months"), model_mf1_times)



  ###
  # add three annual observations to the start of an otherwise quarterly series
  ###

  # modify frequency for HICP_Gas
  mixed_freq <- mixed_freq_orig %>%
    dplyr::bind_rows(
      dplyr::tibble(na_item = "HICP_Gas",
                    time = c(as.Date("2002-01-01"),as.Date("2003-01-01"),as.Date("2004-01-01"))) %>%
        dplyr::mutate(values = rnorm(dplyr::n()))
    ) %>%
    dplyr::arrange(na_item, time)

  expect_silent(model_mf2 <- run_model(specification = specification,
                                       inputdata_directory = mixed_freq,
                                       primary_source = "local",
                                       max.ar = 1, max.dl = 0,
                                       constrain.to.minimum.sample = FALSE,
                                       quiet = TRUE))


  ###
  # add 15 annual observations to the start of an otherwise quarterly series
  # this should result in a warning
  ###

  mixed_freq <- mixed_freq_orig %>%
    dplyr::bind_rows(
      dplyr::tibble(na_item = "HICP_Gas",
                    time = c(seq.Date(from = as.Date("1990-01-01"), length.out = 15, by = "year"))) %>%
        dplyr::mutate(values = rnorm(dplyr::n()))
    ) %>%
    dplyr::arrange(na_item, time)

  expect_warning(
    suppressMessages(
      model_mf2 <- run_model(specification = specification,
                             inputdata_directory = mixed_freq,
                             primary_source = "local",
                             max.ar = 1, max.dl = 0,
                             constrain.to.minimum.sample = FALSE,
                             quiet = FALSE)),
    regexp = "Some observations were removed from the data due to different frequencies")
  #regexp = "Variable provided as 'inputdata_directory' seems to be a data.frame type. Used as data source"),


  # expect the annual observations to be dropped
  model_mf2_times <- model_mf2$full_data %>% dplyr::filter(na_item == "HICP_Gas") %>% dplyr::pull(time)
  expect_false(any(seq.Date(from = as.Date("1990-01-01"), length.out = 15, by = "year") %in% model_mf1_times))
  # expect all other observations to be there
  expect_equal(seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "3 months"), model_mf2_times)




  ###
  # slightly more complex case: add a single annual observation to the end of an otherwise quarterly series
  # should fail as the final observations always takes the difference from the second-to-last observations
  # this will be different than all others (will be annual rather than quarterly)
  # hence the model will fail
  ###

  # modify frequency for HICP_Gas
  mixed_freq <- mixed_freq_orig %>%
    dplyr::bind_rows(
      dplyr::tibble(na_item = "HICP_Gas",
                    time = as.Date("2013-01-01")) %>%
        dplyr::mutate(values = rnorm(dplyr::n()))
    ) %>%
    dplyr::arrange(na_item, time)

  expect_error(model_mf2 <- run_model(specification = specification,
                                      inputdata_directory = mixed_freq,
                                      primary_source = "local",
                                      max.ar = 1, max.dl = 0,
                                      constrain.to.minimum.sample = FALSE,
                                      quiet = TRUE), regexp = "Mixed frequency models are not yet implemented.")





  ###
  # slightly more complex case: add a second annual observation in the middle of an otherwise quarterly series (so will create a duplicate)
  # should fail as duplicates will be detected
  ###

  # modify frequency for HICP_Gas
  mixed_freq <- mixed_freq_orig %>%
    dplyr::bind_rows(
      dplyr::tibble(na_item = "HICP_Gas",
                    time = as.Date("2010-01-01")) %>%
        dplyr::mutate(values = rnorm(dplyr::n()))
    ) %>%
    dplyr::arrange(na_item, time)

  expect_error(
    model_mf <- run_model(specification = specification,
                          inputdata_directory = mixed_freq,
                          primary_source = "local",
                          max.ar = 1, max.dl = 0,
                          constrain.to.minimum.sample = FALSE,
                          quiet = TRUE),
    regexp = "The data contains duplicates. Please remove them.")

})



