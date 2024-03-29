% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hintr-plot-data.R
\name{hintr_calibrate_plot}
\alias{hintr_calibrate_plot}
\title{Get data for hintr calibrate plot}
\usage{
hintr_calibrate_plot(output)
}
\arguments{
\item{output}{A hintr_output object}
}
\value{
Calibrated, unadjusted and spectrum estimates of indicators
}
\description{
Takes hintr_output object, reads data and prepares data in format needed
for plotting calibrate barchart to compare calibrated, spectrum and
unadjusted estimates for a set of indicators.
}
