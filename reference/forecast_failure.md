# Determine the number of Forecast Failures in Insample Forecasting

Determine the number of Forecast Failures in Insample Forecasting

## Usage

``` r
forecast_failure(forecast, data)
```

## Arguments

- forecast:

  A forecast object as returned by
  [`forecast_model`](http://www.moritzschwarz.org/osem/reference/forecast_model.md).

- data:

  A tibble or data.frame containing the original data used for
  estimation.

## Value

A tibble representing information about the success and failures of
forecasting.

## Examples

``` r
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

set.seed(123)
testdata <- dplyr::tibble(
 time = seq.Date(from = as.Date("2005-01-01"),
                 to = as.Date("2023-10-01"),
                 by = "quarter"),
 FinConsExpGov = rnorm(mean = 100, n = length(time)),
 HICP_Gas = rnorm(mean = 200, n = length(time)),
 FinConsExpHH  = 0.5 + 0.2*FinConsExpGov + 0.3 *
   HICP_Gas + rnorm(length(time), mean = 0, sd = 0.2))

testdata <- tidyr::pivot_longer(testdata,
                               cols = -time,
                               names_to = "na_item",
                               values_to = "values")

model <- run_model(specification = specification,
                  dictionary = dict,
                  input = testdata,
                  primary_source = "local",
                  present = FALSE,
                  quiet = TRUE,
                  saturation = "IIS")


insample_output <- forecast_insample(model, sample_share = 0.97)
#> Model Run 1 up to 2023-04-01
#> Model Run 2 up to 2023-07-01
#> Model Run 3 up to 2023-10-01
#> [1] "Forecast 1 from 2023-04-01 to 2023-10-01"
#> [1] "Forecast 2 from 2023-07-01 to 2023-10-01"

insample_output$forecast_failures
#> # A tibble: 3 × 10
#>   time       na_item      values failure_all failure_95 failure_90 failure_50
#>   <date>     <chr>         <dbl> <chr>       <chr>      <chr>      <chr>     
#> 1 2023-07-01 FinConsExpHH   80.5 Success     Success    Success    Success   
#> 2 2023-10-01 FinConsExpHH   80.8 Success     Success    Success    Failure   
#> 3 2023-10-01 FinConsExpHH   80.8 Success     Success    Success    Failure   
#> # ℹ 3 more variables: start <date>, end <date>, method <chr>
```
