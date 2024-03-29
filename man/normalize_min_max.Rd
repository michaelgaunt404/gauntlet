% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_data_manip.R
\name{normalize_min_max}
\alias{normalize_min_max}
\title{Apply Min-Max normalization to a numeric vector.}
\usage{
normalize_min_max(x, na.rm = TRUE)
}
\arguments{
\item{x}{a numeric vector. Works in tidyverse mututate function.}

\item{na.rm}{boolean(TRUE/FALSE)}
}
\value{
a normalized numeric vector.
}
\description{
Apply Min-Max normalization to a numeric vector.
}
\examples{

mtcars \%>\%
  mutate(hp_norm = normalize_min_max(hp))
}
