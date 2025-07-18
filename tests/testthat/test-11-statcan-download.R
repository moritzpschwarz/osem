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
      "CPI_Energy"
    ),
    independent = c(
      "CPI_Gasoline + CPI_Energy + WORLD_OIL",
      "IndProdPriceIndex"
    )
  )

  dictionary <- dplyr::bind_rows(dict_statcan, dict_imf)


  actual_cols = colnames(dictionary)
  # basic functionality
  module_order <- check_config_table(spec)
  to_obtain <- determine_variables(specification = module_order,
                                   dictionary = dictionary)
  data <- download_statcan(to_obtain = to_obtain,
                           column_filters = actual_cols,
                           quiet = FALSE)

  imf_data <- download_imf(to_obtain = to_obtain,
                           column_filters = actual_cols,
                           quiet = FALSE)

  expect_length(data, 2)
  expect_type(data, "list")
  expect_named(data, c("df", "to_obtain"))
  expect_identical(data$df %>% dplyr::filter(na_item == "CPI_Gasoline") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "CPI_Energy") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
  expect_identical(data$df %>% dplyr::filter(na_item == "IndProdPriceIndex") %>% dplyr::pull(time) %>% lubridate::month() %>% unique() %>% sort(), c(1, 4, 7, 10))
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
      "CPI_Energy"
    ),
    independent = c(
      "CPI_Gasoline + CPI_Energy + WORLD_OIL",
      "IndProdPriceIndex"
    )
  )

  dictionary <- dplyr::bind_rows(dict_statcan, dict_imf)

  df <- read.csv(test_path("testdata", "canada_imf_data", "canada_imf_stable_data.csv")) %>%
    dplyr::mutate(na_item = dplyr::case_when(na_item == "HICP_Energy"~"CPI_Energy",
                                             na_item == "HICP_GAS"~"CPI_Gasoline",
                                             na_item == "IndProd"~"IndProdPriceIndex",
                                             TRUE ~ na_item))

  #run the model
  model_run <- run_model(specification = spec,
                         dictionary = dictionary,
                         input = df,
                         primary_source = "local",
                         quiet = TRUE)


  #forcast the model
  set.seed(1234)
  model_forecast <- forecast_model(model_run, plot = FALSE, quiet = TRUE)

  #plot model
  expect_s3_class(plot(model_forecast,order.as.run = TRUE),class = c("gg","ggplot"))

  bb_df <- plot(model_forecast, return.data = TRUE)
  expect_s3_class(bb_df, class = "tbl_df")
  expect_true(all(names(bb_df) == c("time", "na_item", "values", "type", "p95", "p05", "p975",
                                    "p025", "p75", "p25")))
  expect_true(all(unique(bb_df$type) == c("Forecast", "Insample Fit", "Observation")))

  expect_s3_class(plot(model_forecast, return.data = TRUE), class = "tbl_df")
  expect_identical(round(model_forecast$full_forecast_data$values,4),
                   c(584.1482, 1437.4448, 602.1602, 1496.152, 601.5115, 1508.4261,
                     584.6874, 1457.5908, 595.6294, 1480.1908, 614.6781, 1540.0322,
                     613.8344, 1551.6558, 596.0599, 1499.8772, 609.7124, 1530.2034,
                     635.6655, 1602.7002, 636.3, 646.0313, 636.3, 1614.7704, 1624.5017,
                     1614.7704, 589.2694, 600, 1487.1243, 1497.8549, 618.0738, 605.5,
                     1523.8196, 1511.2458, 629.3246, 645.3, 1613.404, 1629.3794, 598.2287,
                     617.6, 1524.8595, 1544.2308, 600.8443, 597.4, 1491.9273, 1488.4829,
                     643.2936, 630.2, 1595.3852, 1582.2917, 647.4892, 649, 1679.1122,
                     1680.623, 681.2956, 691, 1842.5663, 1852.2707, 614.76, 602.9,
                     1592.7193, 1580.8593, 551.8896, 560.1, 1410.8691, 1419.0795,
                     536.9432, 539.3, 1354.7653, 1357.1221, 518.1, 518.1, 1288.8527,
                     1288.8527, 493.5303, 482.1, 1185.9757, 1174.5454, 451.2727, 450.5,
                     1031.3773, 1030.6045, 438.261, 448.8, 1021.4985, 1032.0375, 411.5,
                     411.5, 898.9556, 898.9556, 463.9784, 455.8, 1074.7668, 1066.5884,
                     472.6093, 475.3, 1156.1124, 1158.803, 489.775, 481.4, 1189.2947,
                     1180.9197, 481.1887, 497.7, 1226.546, 1243.0573, 455.5874, 457.7,
                     1109.7955, 1111.908, 483.8363, 469.6, 1179.049, 1164.8127, 509.5009,
                     502.3, 1301.3093, 1294.1084, 512.8509, 503.7, 1306.0893, 1296.9384,
                     469.3836, 479.6, 1188.9145, 1199.1309, 460.7566, 465.4, 1143.1947,
                     1147.8382, 444.599, 452.7, 1076.6286, 1084.7297, 476.2737, 457.7,
                     1105.4635, 1086.8898, 449.8691, 456.7, 1076.8433, 1083.6741,
                     444.3385, 446.1, 1042.3021, 1044.0636, 455.3777, 441.7, 1031.4996,
                     1017.8219, 422.1692, 440.9, 1007.4352, 1026.166, 422.7713, 411.7,
                     913.1861, 902.1148, 444.7443, 436.5, 1011.8349, 1003.5906, 469.6208,
                     464.6, 1105.1791, 1100.1584, 449.7806, 458, 1115.5289, 1123.7483,
                     457.879, 434.4, 1041.2248, 1017.7458, 479.9351, 468.8, 1199.4999,
                     1188.3648, 515.063, 507.6, 1387.4631, 1380.0001, 501.2369, 516.9,
                     1407.2723, 1422.9354, 489.5869, 490, 1353.4002, 1353.8133, 475.9785,
                     473.3, 1324.8773, 1322.1988, 485.565, 490.2, 1376.1089, 1380.7439,
                     475.6695, 479.4, 1326.8713, 1330.6018, 463.1392, 472.8, 1322.9566,
                     1332.6173, 466.1573, 465.2, 1306.2852, 1305.3279, 471.4781, 475.8,
                     1334.2501, 1338.5721, 482.0199, 474.4, 1350.2293, 1342.6094,
                     459.1061, 471.9, 1337.256, 1350.05, 458.5924, 463.7, 1292.1363,
                     1297.244, 483.5431, 471.8, 1337.2492, 1325.5061, 473.5884, 476.4,
                     1365.0015, 1367.8131, 441.0615, 444, 1244.2222, 1247.1607, 428.4024,
                     425.8, 1149.1511, 1146.5487, 413.4125, 417.7, 1082.7884, 1087.0759,
                     414.6834, 409.1, 1087.011, 1081.4276, 390.8981, 400.7, 1056.9797,
                     1066.7816, 386.4789, 391.5, 1035.8143, 1040.8354, 388.1068, 393.5,
                     1021.2418, 1026.6349, 384.5736, 387.2, 972.763, 975.3894, 376.8027,
                     378.1, 873.6606, 874.9578, 401.2, 401.2, 964.5122, 964.5122,
                     496.3148, 494.6, 1408.7542, 1407.0394, 468.2323, 473.9, 1378.461,
                     1384.1287, 422.385, 421.6, 1174.0135, 1173.2285, 393.6283, 409.2,
                     1095.9277, 1111.4994, 405.1198, 412.6, 1070.3193, 1077.7995,
                     405.4931, 421.2, 1068.2091, 1083.916, 385.1215, 388.2, 962.477,
                     965.5555, 396.7009, 374.7, 954.6417, 932.6408, 425.7305, 412.5,
                     1076.8539, 1063.6234, 413.9768, 417.1, 1073.2356, 1076.3588,
                     383.0087, 389.8, 965.2038, 971.9952, 395.1081, 388.2, 968.7362,
                     961.8281, 376.4627, 402.6, 996.6664, 1022.8037, 370.8169, 371.5,
                     913.0045, 913.6875, 353.9951, 353.5, 851.2515, 850.7563, 333.676,
                     350.2, 817.076, 833.5999, 359.8058, 352.1, 841.2351, 833.5292,
                     362.3055, 352.7, 837.8293, 828.2238, 327.2572, 327.2, 751.1848,
                     751.1276, 315.0411, 315.2, 707.1413, 707.3002, 323.0241, 327.5,
                     733.1461, 737.6219, 324.599, 315.4, 710.8213, 701.6223, 336.6,
                     336.6, 772.0628, 772.0628, 307.776, 310.3, 701.8377, 704.3618,
                     307.7139, 306.3, 701.2392, 699.8253, 299.9712, 297, 678.575,
                     675.6038, 287.6838, 286.5, 621.5992, 620.4154, 300.3944, 285.1,
                     629.4944, 614.2001, 323.4148, 311.3, 702.8094, 690.6946, 318.7333,
                     325.2, 726.939, 733.4057, 308.9022, 302.7, 695.8767, 689.6744,
                     306.5482, 312.9, 718.4229, 724.7746, 297.3403, 300.6, 706.9238,
                     710.1835, 301.2148, 288.8, 688.8375, 676.4227, 276.5694, 283.6,
                     651.0405, 658.0712, 264.5255, 270.7, 614.8208, 620.9953, 266.8089,
                     261.6, 598.1886, 592.9797, 253.0943, 249.6, 549.4674, 545.9731,
                     238.365, 238.4, 497.368, 497.4029, 241.9392, 240.5, 507.8101,
                     506.3709, 244.054, 239.8, 514.9953, 510.7413, 245.6214, 242.3,
                     522.499, 519.1776, 239.058, 243, 518.8971, 522.839, 243.9689,
                     250.3, 552.2778, 558.6089, 246.8471, 252.4, 561.7211, 567.274,
                     258.6528, 249.9, 568.8406, 560.0878, NA, 253.8, NA, 575.2499))


  #hindcast model
  hind_cast <- forecast_insample(
    model_run,
    sample_share = 0.97,
    uncertainty_sample = 100,
    exog_fill_method = "last",
    quiet = TRUE
  )

  expect_s3_class(plot(hind_cast),class = c("gg","ggplot"))
  expect_s3_class(hind_cast$central,"tbl_df")
  expect_identical(round(hind_cast$central$values,4),
                   c(629.1019, 614.159, 627.9961, 1613.1813, 1598.2384, 1612.0755,
                     591.736, 605.9106, 1497.4817, 1511.6564, 613.8872, 1511.7422))

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
  test_imf <- structure(
    list(
      model_varname = c("HICP_GAS", "HICP_Energy"),
      full_name = c("The PCPS", "The TEST"),
      database = c("imf", "imf"),
      variable_code = c(NA, NA),
      dataset_id = c("PCPS", "PCPS"),
      var_col = c("na_item", "na_item"),
      nace_r2 = c(NA, NA),
      geo = c(NA, NA),
      unit = c(NA, NA),
      s_adj = c(NA, NA),
      freq = c("M", "M"),
      ref_area = c("W00", "W00"),
      commodity = c("POILAPSP", "POILAPSP"),
      unit_measure = c("USD", "USD"),
      start_period = c(NA, NA),
      end_period = c(NA, NA)
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA, -2L)
  )

  imf_data_identical_identity <- structure(
    list(time = structure(c(7305, 7305, 7305, 7395, 7395,
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
                            19905), class = "Date"),
         na_item = c("HICP_GAS", "HICP_Energy",
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
                     "EmiCO2Industry"),
         values = c(58.115, 58.115, 116.229, 48.331,
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
                            input = imf_data_identical_identity,
                            primary_source = "local",
                            quiet = TRUE)

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
  expect_identical(round(hind_cast$central$values,4), c(493.6997, 492.3237, 508.0448))
  expect_true(hind_cast$args$exog_fill_method == "AR")
  expect_output(print(hind_cast, plot = FALSE),"No forecast failures estimated")

})
