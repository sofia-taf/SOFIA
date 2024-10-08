#' @name SOFIA-package
#'
#' @aliases SOFIA
#'
#' @title Tools to Work with SOFIA Analyses
#'
#' @description
#' Tools that support the SOFIA Transparent Assessment Framework.
#'
#' @details
#' \emph{Prepare data:}
#' \tabular{ll}{
#'   \code{\link{addDriors}}   \tab add driors column to stocks object\cr
#'   \code{\link{addEffort}}   \tab add effort column to catch data\cr
#'   \code{\link{addIndex}}    \tab add index column to catch data\cr
#'   \code{\link{convertData}} \tab convert primary data to combined data\cr
#'   \code{\link{groupData}}   \tab group primary data in subdirectories
#' }
#' \emph{Calculate:}
#' \tabular{ll}{
#'   \code{\link{calcCat}} \tab stock status categories
#' }
#' \emph{Plot:}
#' \tabular{ll}{
#'   \code{\link{plotCat}} \tab summary of stock status categories
#' }
#' \emph{Repositories:}
#' \tabular{ll}{
#'   \code{\link{gitRepos}} \tab list GitHub repositories\cr
#'   \code{\link{gitClone}} \tab clone GitHub repository\cr
#'   \code{\link{gitCloneAll}} \tab clone all SOFIA-TAF repositories
#' }
#'
#' @author Rishi Sharma and Arni Magnusson.
#'
#' @references
#' \url{https://github.com/sofia-taf/doc}
#'
#' @seealso
#' \pkg{SOFIA} is used in combination with the packages \pkg{sraplus}
#' (analytical model) and \pkg{TAF} (workflow).

"_PACKAGE"
