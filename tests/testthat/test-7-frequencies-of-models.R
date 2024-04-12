

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
                     dplyr::mutate(time = seq.Date(from = as.Date("2005-01-01"), length.out = time_to_sim, by = "quarter")))





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

  expect_error(
    model_mf <- run_model(specification = specification,
                          inputdata_directory = mixed_freq,
                          primary_source = "local",
                          max.ar = 1, max.dl = 0,
                          quiet = TRUE),
    regexp = "Mixed frequency models are not yet implemented.")

  expect_error(forecast_model(model_d),regexp = "Mixed frequency forecasts or forecasts with daily or monthly data are not yet implemented.")
  expect_error(forecast_model(model_m),regexp = "Mixed frequency forecasts or forecasts with daily or monthly data are not yet implemented.")

  fc_q <- forecast_model(model_q, quiet = TRUE, plot.forecast = FALSE)
  fc_a <- forecast_model(model_a, quiet = TRUE, plot.forecast = FALSE)

  expect_equal(fc_q$forecast$data[[1]]$time, structure(c(15522, 15614, 15706, 15796, 15887, 15979, 16071, 16161, 16252, 16344), class = "Date"))
  expect_equal(fc_a$forecast$data[[1]]$time, structure(c(12784, 13149, 13514, 13879, 14245, 14610, 14975, 15340, 15706, 16071, 16436, 16801, 17167, 17532, 17897, 18262, 18628, 18993, 19358, 19723, 20089, 20454, 20819, 21184, 21550, 21915, 22280, 22645, 23011, 23376, 23741, 24106, 24472, 24837, 25202, 25567, 25933, 26298, 26663, 27028), class = "Date"))

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

  aggregate.model::dict %>%
    mutate(dataset_id = case_when(model_varname == "FinConsExpHH" ~ "nama_10_gdp",
                                  model_varname == "GCapitalForm" ~ "nama_10_gdp",
                                  TRUE ~ dataset_id)) -> dict_new

  test <- run_model(specification = specification,
                    dictionary = dict_new,
                    save_to_disk = "data-raw/test/test.xlsx",
                    primary_source = "local",
                    inputdata_directory = "data-raw/test",
                    max.ar = 1, # for annual models, would not go beyond 1 (otherwise sample is too short)
                    max.dl = 1, # for annual models, would not go beyond 1 (otherwise sample is too short)
                    max.block.size = 5,
                    use_logs = "none")

  test_fc <- forecast_model(test, plot.forecast = FALSE)
  plot(test_fc, exclude.exogenous = FALSE)

})
