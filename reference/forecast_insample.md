# Forecast OSEM Insample

This function runs the OSEM model insample and can be used to evaluate
the historical forecast performance. In doing so, the function
determines the share of the sample that should be used for the insample
forecasting. The function then runs the model up to the last time point
of the sample and forecasts the values for all dependent variables. The
function also reports diagnostics for those forecasts, such as the RMSFE
and the forecast failures (share of forecasts outside of the uncertainty
bands).

## Usage

``` r
forecast_insample(
  model,
  sample_share = 0.5,
  uncertainty_sample = 100,
  exog_fill_method = "AR",
  plot = TRUE,
  quiet = FALSE,
  parallel.cores = NULL,
  insample_model_list = NULL
)
```

## Arguments

- model:

  A model object of class 'osem'.

- sample_share:

  Share of the sample that should be used for the insample forecasting.
  Must be a numeric and must be either 1 or smaller but larger than 0.

- uncertainty_sample:

  Integer. Number of draws to be made for the error bars. Default is
  100.

- exog_fill_method:

  Character vector that contains the methods to fill the exogenous
  variables. Default is "AR" but can also contain multiple methods, e.g.
  c("AR","auto").

- plot:

  Logical. Should the result be plotted? Default is TRUE.

- quiet:

  Logical. Should messages about the forecast procedure be suppressed?

- parallel.cores:

  Numeric. The number of cores to use for parallel processing. If NULL
  (default), the function will not use parallel processing. If the
  numeric, the function takes the desired number of cores.

- insample_model_list:

  Option to pre-specify the insample models (e.g. from an existing
  object of class `osem.forecast.insample`). If NULL (default), the
  function will run the models for the insample forecasting. If a list
  of models is provided, the function will use those models for the
  insample forecasting and will skip the model running step. This can be
  useful if the user already has the models available and wants to save
  time by not running them again. If a numeric value is provided, it
  will be used as the number of cores for parallel processing. The
  function will check that the specified number of cores does not exceed
  the number of available cores and will adjust accordingly if it does.
  If this happens, then the function automatically detect the number of
  available cores and use one less than that for parallel processing.

## Value

An object (list) with the class `osem.forecast.insample`. This object
contains the central estimates and the uncertainty estimates of the
forecasted values as well as the original data.
