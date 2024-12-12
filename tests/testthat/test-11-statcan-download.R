test_that('statCan_load_or_download works', {

  skip_on_cran()
  skip_on_ci()

  spec <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "IndProdGDP",
      "HICP_Energy"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy + WORLD_OIL",
      "IndProd"
    )
  )

  dict_statCan <- dplyr::tribble(
    ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col ,~freq, ~GEO, ~geo, ~unit, ~s_adj, ~`Seasonal adjustment`, ~nace_r2, ~`North American Industry Classification System (NAICS)`, ~`North American Product Classification System (NAPCS)`,~Prices, ~`Type of fuel`, ~`Products and product groups`,~found, ~ipcc_sector, ~cpa2_1, ~siec,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
    "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "HICP_GAS", "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", NA, "18-10-0004-01", "na_item", "m", "Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Gasoline", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "GAS", "Monthly Average Retail Price for gas", "statcan", NA, "18-10-0001-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, NA, NA, "Regular unleaded gasoline at self service filling stations", NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProdGDP", "Industrial production [T010] in 2017 constant prices", "statcan", NA, "36-10-0434-01", "na_item" ,"m", "Canada", NA, NA, NA, "Seasonally adjusted at annual rates", NA, "Industrial production [T010]", NA, "2017 constant prices", NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProd", "Total, Industrial product price index (IPPI)", "statcan", NA, "18-10-0266-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, "Total, Industrial product price index (IPPI)", NA, NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "WORLD_OIL", "World Oil Price USD", "imf", NA, "PCPS", "na_item","M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,"W00","POILAPSP","USD",NA,NA,
    "FISH", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,

  )
  dict_statCan <- as.data.frame(dict_statCan)

  dictionary <- dict_statCan


  actual_cols = colnames(dictionary)
  # basic functionality
  module_order <- check_config_table(spec)
  to_obtain <- determine_variables(specification = module_order,
                                   dictionary = dictionary)
  data <- download_statcan(to_obtain = to_obtain,
                           #column_filters = additional_filters,
                           column_filters = actual_cols,
                           quiet = FALSE)
  imf_data <- download_imf(to_obtain = to_obtain,
                           #column_filters = additional_filters,
                           column_filters = actual_cols,
                           quiet = FALSE)

  expect_length(data, 2)
  expect_type(data, "list")
  expect_named(data, c("df", "to_obtain"))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_GAS") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "HICP_Energy") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProd") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(imf_data$df %>% dplyr::filter(na_item == "WORLD_OIL") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProdGDP") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))



})

test_that('statcan_load_and_download_forecasting_functionality',{

  skip_on_cran()
  skip_on_ci()

  #setup
  spec <- dplyr::tibble(
    type = c(
      "d",
      "n"
    ),
    dependent = c(
      "IndProdGDP",
      "HICP_Energy"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy + WORLD_OIL",
      "IndProd"
    )
  )

  dict_statCan <- dplyr::tribble(
    ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col ,~freq, ~GEO, ~geo, ~unit, ~s_adj, ~`Seasonal adjustment`, ~nace_r2, ~`North American Industry Classification System (NAICS)`, ~`North American Product Classification System (NAPCS)`,~Prices, ~`Type of fuel`, ~`Products and product groups`,~found, ~ipcc_sector, ~cpa2_1, ~siec,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
    "HICP_Energy", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "HICP_GAS", "Harmonised Index of Consumer Prices, Gas, index 100 = 2002", "statcan", NA, "18-10-0004-01", "na_item", "m", "Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Gasoline", NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "GAS", "Monthly Average Retail Price for gas", "statcan", NA, "18-10-0001-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, NA, NA, "Regular unleaded gasoline at self service filling stations", NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProdGDP", "Industrial production [T010] in 2017 constant prices", "statcan", NA, "36-10-0434-01", "na_item" ,"m", "Canada", NA, NA, NA, "Seasonally adjusted at annual rates", NA, "Industrial production [T010]", NA, "2017 constant prices", NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "IndProd", "Total, Industrial product price index (IPPI)", "statcan", NA, "18-10-0266-01", "na_item", "m", "Canada", NA, NA, NA, NA, NA, NA, "Total, Industrial product price index (IPPI)", NA, NA, NA, NA, NA, NA, NA,NA,NA,NA,NA,NA,
    "WORLD_OIL", "World Oil Price USD", "imf", NA, "PCPS", "na_item","M", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,"W00","POILAPSP","USD",NA,NA,
    "FISH", "Harmonised Index of Consumer Prices, Energy, index 100 = 2002", "statcan", NA,"18-10-0004-01","na_item","m","Canada", NA, "units", NA, NA, NA, NA, NA, NA, NA, "Energy", NA, NA, NA, NA,NA,NA,NA,NA,NA,

  )


  dictionary <- dict_statCan

  df <- read.csv(test_path("testdata", "canada_imf_data", "canada_imf_stable_data.csv"))

  #run the model
  model_run <- run_model(specification = spec,
                         dictionary = dictionary,
                         inputdata_directory = df,
                         primary_source = "local",
                         quiet = TRUE)


  #forcast the model
  model_forecast <- forecast_model(model_run, plot = FALSE)

  #plot model
  expect_s3_class(plot(model_forecast,order.as.run = TRUE),class = c("gg","ggplot"))

  bb_df <- plot(model_forecast, return.data = TRUE)
  expect_s3_class(bb_df, class = "tbl_df")
  expect_true(all(names(bb_df) == c("time", "na_item", "values", "type", "p95", "p05", "p975",
                                    "p025", "p75", "p25")))
  expect_true(all(unique(bb_df$type) == c("Forecast", "Insample Fit", "Observation")))

  expect_s3_class(plot(model_forecast, return.data = TRUE), class = "tbl_df")
  expect_identical(round(model_forecast$full_forecast_data$values,4),
                   c(580.7463, 1434.0429, 598.4211, 1492.413, 599.2092, 1506.1238,
                     582.3351, 1455.2385, 593.061, 1477.6224, 611.8439, 1537.198,
                     612.5249, 1550.3462, 594.7291, 1498.5463, 608.2394, 1528.7305,
                     634.0136, 1601.0483, 646.0395, 646.0395, 636.3, 1624.5099, 1624.5099,
                     1614.7704, 589.2943, 600, 1487.1492, 1497.8549, 618.0969, 605.5,
                     1523.8427, 1511.2458, 627.6964, 645.3, 1611.7758, 1629.3794,
                     598.1991, 617.6, 1524.8299, 1544.2308, 600.8635, 597.4, 1491.9465,
                     1488.4829, 643.3377, 630.2, 1595.4294, 1582.2917, 645.779, 649,
                     1677.4021, 1680.623, 681.3335, 691, 1842.6042, 1852.2707, 614.8411,
                     602.9, 1592.8004, 1580.8593, 551.925, 560.1, 1410.9046, 1419.0795,
                     535.5454, 539.3, 1353.3675, 1357.1221, 518.1, 518.1, 1288.8527,
                     1288.8527, 493.5742, 482.1, 1186.0196, 1174.5454, 451.2786, 450.5,
                     1031.3831, 1030.6045, 437.1118, 448.8, 1020.3493, 1032.0375,
                     411.5, 411.5, 898.9556, 898.9556, 463.9758, 455.8, 1074.7642,
                     1066.5884, 472.6097, 475.3, 1156.1127, 1158.803, 488.4703, 481.4,
                     1187.99, 1180.9197, 481.1587, 497.7, 1226.516, 1243.0573, 455.5819,
                     457.7, 1109.7899, 1111.908, 483.8319, 469.6, 1179.0446, 1164.8127,
                     508.1565, 502.3, 1299.9649, 1294.1084, 512.8291, 503.7, 1306.0674,
                     1296.9384, 469.3918, 479.6, 1188.9227, 1199.1309, 460.7659, 465.4,
                     1143.2041, 1147.8382, 443.4034, 452.7, 1075.4331, 1084.7297,
                     476.2391, 457.7, 1105.4289, 1086.8898, 449.8737, 456.7, 1076.8478,
                     1083.6741, 444.3409, 446.1, 1042.3045, 1044.0636, 454.1732, 441.7,
                     1030.2951, 1017.8219, 422.1253, 440.9, 1007.3913, 1026.166, 422.7589,
                     411.7, 913.1737, 902.1148, 444.7307, 436.5, 1011.8213, 1003.5906,
                     468.3785, 464.6, 1103.9368, 1100.1584, 449.7409, 458, 1115.4893,
                     1123.7483, 457.873, 434.4, 1041.2188, 1017.7458, 479.9208, 468.8,
                     1199.4856, 1188.3648, 513.6962, 507.6, 1386.0963, 1380.0001,
                     501.1959, 516.9, 1407.2313, 1422.9354, 489.605, 490, 1353.4183,
                     1353.8133, 475.9721, 473.3, 1324.8709, 1322.1988, 484.2773, 490.2,
                     1374.8212, 1380.7439, 475.6205, 479.4, 1326.8223, 1330.6018,
                     463.1397, 472.8, 1322.9571, 1332.6173, 466.1525, 465.2, 1306.2805,
                     1305.3279, 470.2194, 475.8, 1332.9915, 1338.5721, 481.9762, 474.4,
                     1350.1857, 1342.6094, 459.104, 471.9, 1337.254, 1350.05, 458.5841,
                     463.7, 1292.128, 1297.244, 482.2609, 471.8, 1335.967, 1325.5061,
                     473.5583, 476.4, 1364.9714, 1367.8131, 441.07, 444, 1244.2307,
                     1247.1607, 428.405, 425.8, 1149.1537, 1146.5487, 412.3039, 417.7,
                     1081.6798, 1087.0759, 414.6387, 409.1, 1086.9662, 1081.4276,
                     390.8877, 400.7, 1056.9693, 1066.7816, 386.4632, 391.5, 1035.7987,
                     1040.8354, 387.0604, 393.5, 1020.1953, 1026.6349, 384.5235, 387.2,
                     972.7129, 975.3894, 376.7749, 378.1, 873.6327, 874.9578, 401.2,
                     401.2, 964.5122, 964.5122, 495.0101, 494.6, 1407.4495, 1407.0394,
                     468.2133, 473.9, 1378.442, 1384.1287, 422.3874, 421.6, 1174.0159,
                     1173.2285, 393.6045, 409.2, 1095.9038, 1111.4994, 404.0176, 412.6,
                     1069.2171, 1077.7995, 405.4508, 421.2, 1068.1669, 1083.916, 385.1181,
                     388.2, 962.4737, 965.5555, 396.6796, 374.7, 954.6205, 932.6408,
                     424.5926, 412.5, 1075.716, 1063.6234, 413.9384, 417.1, 1073.1972,
                     1076.3588, 382.9928, 389.8, 965.1879, 971.9952, 395.0916, 388.2,
                     968.7197, 961.8281, 402.6, 402.6, 1022.8037, 1022.8037, 370.7734,
                     371.5, 912.961, 913.6875, 353.9825, 353.5, 851.2388, 850.7563,
                     333.6462, 350.2, 817.0461, 833.5999, 358.8353, 352.1, 840.2646,
                     833.5292, 362.2754, 352.7, 837.7993, 828.2238, 327.2475, 327.2,
                     751.1751, 751.1276, 315.0127, 315.2, 707.113, 707.3002, 322.1479,
                     327.5, 732.2699, 737.6219, 324.5349, 315.4, 710.7571, 701.6223,
                     336.6, 336.6, 772.0628, 772.0628, 307.7578, 310.3, 701.8196,
                     704.3618, 306.8804, 306.3, 700.4057, 699.8253, 299.9266, 297,
                     678.5304, 675.6038, 287.6648, 286.5, 621.5802, 620.4154, 300.3666,
                     285.1, 629.4666, 614.2001, 322.5314, 311.3, 701.926, 690.6946,
                     318.69, 325.2, 726.8957, 733.4057, 308.8802, 302.7, 695.8546,
                     689.6744, 306.533, 312.9, 718.4077, 724.7746, 296.5319, 300.6,
                     706.1155, 710.1835, 301.173, 288.8, 688.7957, 676.4227, 276.5518,
                     283.6, 651.0229, 658.0712, 264.505, 270.7, 614.8003, 620.9953,
                     266.0862, 261.6, 597.4659, 592.9797, 253.0524, 249.6, 549.4255,
                     545.9731, 238.3394, 238.4, 497.3424, 497.4029, 241.9168, 240.5,
                     507.7877, 506.3709, 243.6899, 239.8, 514.6312, 510.7413, 245.8843,
                     242.3, 522.762, 519.1776, 239.3309, 243, 519.1699, 522.839, 244.2501,
                     250.3, 552.559, 558.6089, 246.4757, 252.4, 561.3497, 567.274,
                     258.9328, 249.9, 569.1207, 560.0878, NA, 253.8, NA, 575.2499))


#hindcast model
hind_cast <- forecast_insample(
  model_run,
  sample_share = 0.99,
  uncertainty_sample = 100,
  exog_fill_method = "last",
  quiet = TRUE
)

expect_s3_class(plot(hind_cast),class = c("gg","ggplot"))
expect_s3_class(hind_cast$central,"tbl_df")
expect_identical(round(hind_cast$central$values,4),c(613.8678, 616.3204, 1511.7227, 1514.1753))

})

# test_that("IMF Download works",{
#
#
#
# })

test_that("Test behaviour when everything is an identity",{
  #toy specs for example
  spec <- dplyr::tibble(
    type = c(
      "d"
    ),
    dependent = c(
      "EmiCO2Industry"
    ),
    independent = c(
      "HICP_GAS + HICP_Energy"

    )
  )

  #toy dictionary for example

  test_imf <- tibble::tribble(
    ~model_varname, ~full_name, ~database, ~variable_code, ~dataset_id, ~var_col, ~nace_r2, ~geo, ~unit, ~s_adj,~freq,~ref_area,~commodity,~unit_measure,~start_period,~end_period,
    "HICP_GAS","The PCPS","imf",NA,"PCPS","na_item", NA, NA, NA, NA,"M","W00","POILAPSP","USD",NA,NA,
    "HICP_Energy","The TEST","imf",NA,"PCPS","na_item", NA, NA, NA, NA,"M","W00","POILAPSP","USD",NA,NA
  )

  imf_data_identical_identity <- structure(list(time = structure(c(7305, 7305, 7305, 7395, 7395,
                                                                   7395, 7486, 7486, 7486, 7578, 7578, 7578, 7670, 7670, 7670, 7760,
                                                                   7760, 7760, 7851, 7851, 7851, 7943, 7943, 7943, 8035, 8035, 8035,
                                                                   8126, 8126, 8126, 8217, 8217, 8217, 8309, 8309, 8309, 8401, 8401,
                                                                   8401, 8491, 8491, 8491, 8582, 8582, 8582, 8674, 8674, 8674, 8766,
                                                                   8766, 8766, 8856, 8856, 8856, 8947, 8947, 8947, 9039, 9039, 9039,
                                                                   9131, 9131, 9131, 9221, 9221, 9221, 9312, 9312, 9312, 9404, 9404,
                                                                   9404, 9496, 9496, 9496, 9587, 9587, 9587, 9678, 9678, 9678, 9770,
                                                                   9770, 9770, 9862, 9862, 9862, 9952, 9952, 9952, 10043, 10043,
                                                                   10043, 10135, 10135, 10135, 10227, 10227, 10227, 10317, 10317,
                                                                   10317, 10408, 10408, 10408, 10500, 10500, 10500, 10592, 10592,
                                                                   10592, 10682, 10682, 10682, 10773, 10773, 10773, 10865, 10865,
                                                                   10865, 10957, 10957, 10957, 11048, 11048, 11048, 11139, 11139,
                                                                   11139, 11231, 11231, 11231, 11323, 11323, 11323, 11413, 11413,
                                                                   11413, 11504, 11504, 11504, 11596, 11596, 11596, 11688, 11688,
                                                                   11688, 11778, 11778, 11778, 11869, 11869, 11869, 11961, 11961,
                                                                   11961, 12053, 12053, 12053, 12143, 12143, 12143, 12234, 12234,
                                                                   12234, 12326, 12326, 12326, 12418, 12418, 12418, 12509, 12509,
                                                                   12509, 12600, 12600, 12600, 12692, 12692, 12692, 12784, 12784,
                                                                   12784, 12874, 12874, 12874, 12965, 12965, 12965, 13057, 13057,
                                                                   13057, 13149, 13149, 13149, 13239, 13239, 13239, 13330, 13330,
                                                                   13330, 13422, 13422, 13422, 13514, 13514, 13514, 13604, 13604,
                                                                   13604, 13695, 13695, 13695, 13787, 13787, 13787, 13879, 13879,
                                                                   13879, 13970, 13970, 13970, 14061, 14061, 14061, 14153, 14153,
                                                                   14153, 14245, 14245, 14245, 14335, 14335, 14335, 14426, 14426,
                                                                   14426, 14518, 14518, 14518, 14610, 14610, 14610, 14700, 14700,
                                                                   14700, 14791, 14791, 14791, 14883, 14883, 14883, 14975, 14975,
                                                                   14975, 15065, 15065, 15065, 15156, 15156, 15156, 15248, 15248,
                                                                   15248, 15340, 15340, 15340, 15431, 15431, 15431, 15522, 15522,
                                                                   15522, 15614, 15614, 15614, 15706, 15706, 15706, 15796, 15796,
                                                                   15796, 15887, 15887, 15887, 15979, 15979, 15979, 16071, 16071,
                                                                   16071, 16161, 16161, 16161, 16252, 16252, 16252, 16344, 16344,
                                                                   16344, 16436, 16436, 16436, 16526, 16526, 16526, 16617, 16617,
                                                                   16617, 16709, 16709, 16709, 16801, 16801, 16801, 16892, 16892,
                                                                   16892, 16983, 16983, 16983, 17075, 17075, 17075, 17167, 17167,
                                                                   17167, 17257, 17257, 17257, 17348, 17348, 17348, 17440, 17440,
                                                                   17440, 17532, 17532, 17532, 17622, 17622, 17622, 17713, 17713,
                                                                   17713, 17805, 17805, 17805, 17897, 17897, 17897, 17987, 17987,
                                                                   17987, 18078, 18078, 18078, 18170, 18170, 18170, 18262, 18262,
                                                                   18262, 18353, 18353, 18353, 18444, 18444, 18444, 18536, 18536,
                                                                   18536, 18628, 18628, 18628, 18718, 18718, 18718, 18809, 18809,
                                                                   18809, 18901, 18901, 18901, 18993, 18993, 18993, 19083, 19083,
                                                                   19083, 19174, 19174, 19174, 19266, 19266, 19266, 19358, 19358,
                                                                   19358, 19448, 19448, 19448, 19539, 19539, 19539, 19631, 19631,
                                                                   19631, 19723, 19723, 19723, 19814, 19814, 19814, 19905, 19905,
                                                                   19905), class = "Date"), na_item = c("HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry", "HICP_GAS", "HICP_Energy", "EmiCO2Industry",
                                                                                                        "HICP_GAS", "HICP_Energy", "EmiCO2Industry", "HICP_GAS", "HICP_Energy",
                                                                                                        "EmiCO2Industry"), values = c(58.115, 58.115, 116.229, 48.331,
                                                                                                                                      48.331, 96.662, 76.389, 76.389, 152.778, 91.903, 91.903, 183.805,
                                                                                                                                      59.164, 59.164, 118.328, 55.244, 55.244, 110.488, 58.465, 58.465,
                                                                                                                                      116.93, 59.745, 59.745, 119.491, 52.477, 52.477, 104.954, 59.023,
                                                                                                                                      59.023, 118.047, 60.036, 60.036, 120.071, 56.93, 56.93, 113.86,
                                                                                                                                      53.996, 53.996, 107.993, 54.179, 54.179, 108.358, 48.728, 48.728,
                                                                                                                                      97.455, 45.19, 45.19, 90.381, 41.385, 41.385, 82.77, 48.682,
                                                                                                                                      48.682, 97.363, 51.364, 51.364, 102.729, 49.9, 49.9, 99.801,
                                                                                                                                      51.704, 51.704, 103.408, 54.531, 54.531, 109.062, 49.534, 49.534,
                                                                                                                                      99.069, 51.195, 51.195, 102.39, 54.93, 54.93, 109.86, 58.741,
                                                                                                                                      58.741, 117.483, 62.38, 62.38, 124.761, 69.894, 69.894, 139.788,
                                                                                                                                      63.65, 63.65, 127.3, 55.688, 55.688, 111.376, 56.074, 56.074,
                                                                                                                                      112.148, 56.809, 56.809, 113.618, 42.939, 42.939, 85.878, 40.678,
                                                                                                                                      40.678, 81.355, 39.341, 39.341, 78.683, 35.871, 35.871, 71.742,
                                                                                                                                      35.603, 35.603, 71.206, 48.673, 48.673, 97.346, 62.38, 62.38,
                                                                                                                                      124.759, 71.495, 71.495, 142.991, 80.271, 80.271, 160.542, 81.023,
                                                                                                                                      81.023, 162.045, 90.784, 90.784, 181.567, 89.575, 89.575, 179.149,
                                                                                                                                      78.674, 78.674, 157.349, 80.506, 80.506, 161.011, 76.295, 76.295,
                                                                                                                                      152.589, 58.4, 58.4, 116.8, 63.315, 63.315, 126.631, 75.804,
                                                                                                                                      75.804, 151.608, 80.825, 80.825, 161.651, 80.062, 80.062, 160.124,
                                                                                                                                      93.763, 93.763, 187.526, 79.722, 79.722, 159.445, 85.322, 85.322,
                                                                                                                                      170.644, 88.1, 88.1, 176.2, 96.428, 96.428, 192.855, 107.124,
                                                                                                                                      107.124, 214.248, 121.629, 121.629, 243.258, 128.5, 128.5, 257,
                                                                                                                                      138.756, 138.756, 277.513, 153.088, 153.088, 306.175, 180.304,
                                                                                                                                      180.304, 360.607, 169.928, 169.928, 339.856, 183.595, 183.595,
                                                                                                                                      367.19, 205.559, 205.559, 411.118, 206.323, 206.323, 412.647,
                                                                                                                                      177.341, 177.341, 354.682, 172.056, 172.056, 344.111, 198.516,
                                                                                                                                      198.516, 397.032, 220.399, 220.399, 440.799, 263.299, 263.299,
                                                                                                                                      526.599, 285.829, 285.829, 571.657, 363.329, 363.329, 726.657,
                                                                                                                                      346.739, 346.739, 693.479, 165.312, 165.312, 330.624, 130.658,
                                                                                                                                      130.658, 261.316, 176.489, 176.489, 352.979, 204.835, 204.835,
                                                                                                                                      409.67, 226.035, 226.035, 452.071, 231.382, 231.382, 462.763,
                                                                                                                                      234.628, 234.628, 469.255, 227.176, 227.176, 454.352, 256.549,
                                                                                                                                      256.549, 513.097, 299.461, 299.461, 598.921, 330.613, 330.613,
                                                                                                                                      661.226, 309.806, 309.806, 619.612, 308.944, 308.944, 617.888,
                                                                                                                                      337.25, 337.25, 674.5, 309.009, 309.009, 618.019, 308.972, 308.972,
                                                                                                                                      617.944, 305.928, 305.928, 611.856, 315.617, 315.617, 631.235,
                                                                                                                                      298.102, 298.102, 596.204, 322.344, 322.344, 644.688, 313.999,
                                                                                                                                      313.999, 627.998, 311.413, 311.413, 622.827, 319.335, 319.335,
                                                                                                                                      638.671, 301.4, 301.4, 602.8, 223.765, 223.765, 447.53, 155.546,
                                                                                                                                      155.546, 311.092, 181.548, 181.548, 363.097, 147.058, 147.058,
                                                                                                                                      294.117, 126.791, 126.791, 253.581, 99.415, 99.415, 198.83, 135.866,
                                                                                                                                      135.866, 271.732, 135.322, 135.322, 270.644, 148.564, 148.564,
                                                                                                                                      297.127, 159.574, 159.574, 319.148, 148.99, 148.99, 297.98, 151.03,
                                                                                                                                      151.03, 302.059, 176.138, 176.138, 352.276, 194.231, 194.231,
                                                                                                                                      388.462, 214.938, 214.938, 429.877, 219.908, 219.908, 439.817,
                                                                                                                                      193.313, 193.313, 386.625, 182.108, 182.108, 364.216, 194.957,
                                                                                                                                      194.957, 389.915, 179.22, 179.22, 358.439, 180.903, 180.903,
                                                                                                                                      361.806, 147.288, 147.288, 294.577, 94.356, 94.356, 188.711,
                                                                                                                                      126.937, 126.937, 253.875, 132.605, 132.605, 265.209, 178.945,
                                                                                                                                      178.945, 357.891, 202.053, 202.053, 404.105, 215.122, 215.122,
                                                                                                                                      430.244, 234.88, 234.88, 469.759, 286.059, 286.059, 572.119,
                                                                                                                                      328.271, 328.271, 656.541, 286.423, 286.423, 572.846, 255.592,
                                                                                                                                      255.592, 511.183, 238.683, 238.683, 477.366, 229.431, 229.431,
                                                                                                                                      458.862, 254.779, 254.779, 509.559, 244.246, 244.246, 488.492,
                                                                                                                                      242.555, 242.555, 485.11, 253.27, 253.27, 506.541, 234.749, 234.749,
                                                                                                                                      469.498)), row.names = c(NA, -417L), class = c("tbl_df", "tbl",
                                                                                                                                                                                     "data.frame"))


  model_result <- run_model(specification = spec,
                            dictionary = test_imf,
                            inputdata_directory = imf_data_identical_identity,
                            primary_source = "local")

  expect_true(nrow(diagnostics_model(model_result)) == 0)
  expect_output(print(model_result, plot = FALSE),"No Diagnostics available")

  expect_silent(hind_cast <- forecast_insample(
    model_result,
    sample_share = 0.98,
    uncertainty_sample = 100,
    exog_fill_method = "AR",
    plot = FALSE,
    quiet = TRUE
  ))

  expect_null(hind_cast$uncertainty)
  expect_identical(round(hind_cast$central$values,4), c(493.6997, 492.3237, 483.627, 508.0448, 498.334))
  expect_true(hind_cast$args$exog_fill_method == "AR")

})






#
# ##TODO
# ##STRESS TESTS
