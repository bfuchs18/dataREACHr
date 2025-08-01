% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ref_reach_ed.R
\docType{data}
\name{ed_data}
\alias{ed_data}
\title{ed_data: Reference tables for the energy densities of all foods in the REACH}
\format{
A data.frame with columns:
\describe{
\item{meal}{meal food was served at}
\item{food}{food item}
\item{ed}{energy denisty}
}
}
\usage{
data(ed_data)
}
\description{
Energy Density for each food served during project REACH
}
\examples{
data(ed_data)
ed <- attr(ed_data, "ed")

}
\keyword{datasets}
