% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_load.R
\name{package_load}
\alias{package_load}
\title{Load many packages.}
\usage{
package_load(pkg)
}
\arguments{
\item{pkg}{vector of string libraries.}
}
\value{
message detailing successful or unsuccessful loaded packages
}
\description{
Function loads R libraries listed in string vector. Compatable with targets formatting style.
}
\examples{
packages = c("dplyr", "ggplot")

package_load(packages)
}
