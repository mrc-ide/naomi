% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outputs.R
\name{generate_comparison_report}
\alias{generate_comparison_report}
\title{Generate and save comparison report at specified path}
\usage{
generate_comparison_report(report_path, outputs, quiet = FALSE)
}
\arguments{
\item{report_path}{Path to save summary report at}

\item{outputs}{Path to model outputs rds or zip file}

\item{quiet}{Suppress printing of the pandoc command line}
}
\value{
Path to summary report
}
\description{
Generate and save comparison report at specified path
}
\keyword{internal}
