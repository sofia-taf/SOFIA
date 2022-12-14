% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/groupData.R
\name{groupData}
\alias{groupData}
\title{Group Data}
\usage{
groupData(dir, quiet = FALSE)
}
\arguments{
\item{dir}{directory containing the primary data files.}

\item{quiet}{whether to suppress screen output.}
}
\value{
Files are copied into subdirectories. As a byproduct, a list is returned,
describing which subdirectories contain which data files.
}
\description{
Group primary single-stock data files in subdirectories, depending on what
columns each data file contains.
}
\note{
A primary data file can have a filename such as
\file{Yellowtail_snapper_Mexico.csv} and columns such as
\code{stockid|scientificname|commonname|year|catch|stocklong}.

In addition, the data file may have columns called \code{best_effort} and/or
\code{best_index}, containing the effort and/or index series to be used in
the SOFIA analysis.

This function creates four subdirectories:
\enumerate{
\item \code{both} - for data files containing both effort and index data
\item \code{effort} - for data files containing effort data (and possibly
       also index)
\item \code{index} - for data files containing index data (and possibly also
      effort)
\item \code{neither} - for data files containing neither effort nor index
      data
}

The object returned has an attribute \code{count}, showing the number of data
files in each subdirectory. The number of original (unique) data files will
be:
\preformatted{
  both + (effort-both) + (index-both) + neither
}

The functions \code{groupData} and \code{convertData} are used together:
first group, then convert.
}
\examples{
\dontrun{
groupData("Data_files_Area_31_3")
groupData("Data_files_Area_31_3", quiet=TRUE)
}

}
\seealso{
\code{\link{convertData}} converts primary data to combined data.

\code{\link{SOFIA-package}} gives an overview of the package.
}
\author{
Arni Magnusson.
}
