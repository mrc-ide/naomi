% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model.R
\name{survey_mf}
\alias{survey_mf}
\title{Prepare model frames for survey datasets}
\usage{
survey_mf(
  survey_ids,
  indicator,
  survey_hiv_indicators,
  naomi_mf,
  deff = 1,
  min_age = 0,
  max_age = 80
)
}
\arguments{
\item{survey_ids}{Survey IDs}

\item{indicator}{Indicator to filter, character string}

\item{survey_hiv_indicators}{Survey HIV indicators}

\item{naomi_mf}{Naomi model frame}

\item{deff}{Assumed design effect for scaling effective sample size}

\item{min_age}{Min age for calculating recent infection}

\item{max_age}{Max age for calculating recent infection}
}
\description{
Prepare model frames for survey datasets
}