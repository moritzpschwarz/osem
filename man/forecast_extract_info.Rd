% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forecast_extract_info.R
\name{forecast_extract_info}
\alias{forecast_extract_info}
\title{Extract relevant information from the osem model object for forecasting and nowcasting}
\usage{
forecast_extract_info(model, i, n.ahead, exog_df_ready)
}
\arguments{
\item{model}{A model object of class 'osem'.}

\item{i}{Current module that is being cycled through}

\item{n.ahead}{Periods to forecast ahead}

\item{exog_df_ready}{Outcome of forecast_exogenous_values() which is the set of forecasted exogenous values}
}
\value{
The relevant information in a list format that enables forecasting and nowcasting
}
\description{
Extract relevant information from the osem model object for forecasting and nowcasting
}
