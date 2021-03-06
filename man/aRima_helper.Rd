% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arima_helper.R
\name{aRima_helper}
\alias{aRima_helper}
\title{Create an ARIMA UI to test terms}
\usage{
aRima_helper(time_series_object)
}
\arguments{
\item{time_series_object}{time-series}
}
\value{
Interactive ARIMA testing shiny app
}
\description{
Simplifies ARIMA modeling by providing a user interface and immediate results. User can specify non-seasonal and or/seasonal AR, MA, and difference terms as well as dummy & Fourier terms. Stationary test, model convergence, fit statistics, ACF & PACF plots, residual plots, and white noise plots all provided. Function takes a time-series object with or without associated frequency. Create time series object with ts()
}
