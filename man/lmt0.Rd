% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_data_manip.R
\name{lmt0}
\alias{lmt0}
\title{Limit to Non-Negative Values}
\usage{
lmt0(x)
}
\arguments{
\item{x}{A numeric vector.}
}
\value{
A numeric vector with non-negative values.
}
\description{
This function takes a numeric vector and replaces any negative values with zero.
}
\examples{
lmt0(c(5, 6, 2, 0, -1))

}
