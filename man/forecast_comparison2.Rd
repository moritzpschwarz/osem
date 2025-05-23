% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_comparison.R
\name{forecast_comparison2}
\alias{forecast_comparison2}
\title{Creates baseline forecasts for comparison with OSEM}
\usage{
forecast_comparison2(
  model,
  n.ahead,
  forecast_type = c("AR", "RW"),
  lags = NULL,
  mc = TRUE
)
}
\arguments{
\item{model}{A model object of class 'osem'.}

\item{n.ahead}{Periods to forecast ahead}

\item{forecast_type}{Type of forecast. Either "AR" for autoregressive or "RW" for random walk.}

\item{lags}{Number of lags to include in the AR model. Ignore for RW model. Default chooses the same as the max.ar setting of the model object.}

\item{mc}{Logical value whether to include in intercept in the AR model or not.}
}
\value{
Returns a data frame with the point forecasts.
}
\description{
Creates baseline forecasts for comparison with OSEM
}
\details{
The function first determines the maximum forecast horizon by adding the n.ahead argument to the most recent data
observation across all modules. For variables whose forecast origin is before that, it creates additional forecasts
up to the forecast origin. Hence, the actual number of forecasted values may differ across variables.

When the forecast type is "AR", the function first transforms the variable into logs (if only positive values
observed), otherwise using the asinh transformation. Reported forecast values are after conversion back to the level
of the variable.

In contrast to forecast_comparison(), we use the maximum available data for the univariate forecasts rather than ensuring that the same subsample is used on OSEM and the univariate models.
}
