% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_timeseries.R
\name{roll_mad}
\alias{roll_mad}
\title{Make MAD (median absolute deviation) values.}
\usage{
roll_mad(values, last = F)
}
\arguments{
\item{values}{vector of numeric values to MAD.}

\item{last}{boolean - determines if last value should only be returned. Default is `FALSE`.}
}
\value{
returns a single MAD value for the last value in vector
}
\description{
Function computes MAD statistic(s) for numeric vector. All values in vector are used to create MAD statistic. MAD values for all or last entry alone can be returned.
}
\examples{
numeric_vector = c(rnorm(10, 10, 3), 100)

roll_mad(numeric_vector)

roll_mad(numeric_vector, last = T)

#compare to zscore
scale(numeric_vector)[,1]
}
