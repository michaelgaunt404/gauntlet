% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/detail_R_scripts.R
\name{detail_R_scripts}
\alias{detail_R_scripts}
\title{Extract and Summarize Script Metadata from R Scripts}
\usage{
detail_R_scripts(dir)
}
\arguments{
\item{dir}{A character string specifying the directory to search for R scripts.
The function will look for files with a `.R` extension within this directory.}
}
\value{
A data frame with the following columns:
\describe{
  \item{dir}{The directory where the script is located.}
  \item{script}{The name of the script file.}
  \item{author}{The author of the script, extracted from the script's metadata.}
  \item{description}{A high-level description of what the script does, extracted
  from the script's metadata.}
  \item{details}{Additional details or README information about the script,
  extracted from the script's metadata.}
}
}
\description{
This function searches a specified directory for R scripts and extracts
metadata from the top of each script. The metadata includes the script's
description, author, and additional details (referred to as README information).
}
\details{
**Note:** The metadata must follow a very specific format that is prescribed in
the 'gauntlet' package, which this function is a part of. The function assumes
that the format used is consistent with this prescribed structure. The format is listed below in the example.
}
\examples{
\dontrun{
# Create an example R script with the specific metadata format
script_content <-
  "#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   #
   # DESC: A high-level description of what this script does.
   #
   # By: mike gaunt, michael.gaunt@throwaway.com
   #
   # README: Additional details re/ script
   #-------- [[insert brief readme here]]
   #
   # *please use 80 character margins
   #
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mtcars \%>\% print"

# Write the script to the temporary directory
temp_dir = tempdir()
writeLines(script_content, file.path(temp_dir, "example_script.R"))

# Run the describe_R_scripts function on the temporary directory
detail_R_scripts(temp_dir)
}

}
