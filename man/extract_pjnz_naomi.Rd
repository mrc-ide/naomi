% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inputs-spectrum.R
\name{extract_pjnz_naomi}
\alias{extract_pjnz_naomi}
\title{Extract Model Inputs from Spectrum PJNZ}
\usage{
extract_pjnz_naomi(pjnz_list, extract_shiny90 = TRUE)
}
\arguments{
\item{pjnz_list}{Vector of filepaths to Spectrum PJNZ file.}

\item{extract_shiny90}{Logical; whether to attempt to extract .shiny90 zip}
}
\value{
A \code{data.frame} with Spectrum indicators.
}
\description{
If the .shiny90 file does not exist within the .PJNZ, the function will
silently not return values, even if \code{extract_shiny90 = TRUE}.
}
\examples{
pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
spec <- extract_pjnz_naomi(pjnz)

}
