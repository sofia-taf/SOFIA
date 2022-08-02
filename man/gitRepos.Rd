% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gitRepos.R
\name{gitRepos}
\alias{gitRepos}
\title{gitRepos}
\usage{
gitRepos(owner = "sofia-taf", full = FALSE)
}
\arguments{
\item{owner}{name of a GitHub organization or user.}

\item{full}{whether to include owner name in output.}
}
\value{
String vector of repository names.
}
\description{
List all GitHub repositories owned by an organization or user.
}
\note{
This function uses the \href{https://docs.github.com/en/rest}{GitHub API} to
fetch repository names, using base R.
}
\examples{
\dontrun{
gitRepos()
gitRepos("arni-magnusson", full=TRUE)
}

}
\seealso{
\code{\link{SOFIA-package}} gives an overview of the package.

The \pkg{gh} package provides more GitHub API tools.
}
\author{
Arni Magnusson.
}