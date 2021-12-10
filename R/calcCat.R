#' Calculate Categories
#'
#' Calculate stock status categories from B/Bmsy and F/Fmsy, and add as columns
#' to an existing data frame.
#'
#' @param dat data frame of stock time series, containing columns named
#'        \code{bbmsy} and \code{ffmsy}, as well as method-specific
#'        \code{bbmsy.*} and \code{ffmsy.*}.
#' @param method string indicating which method was used to estimate B/Bmsy and
#'        F/Fmsy.
#'
#' @details
#' The column names in \code{dat} should contain the \code{method} name as a
#' suffix. For example, if \code{method = "effEdepP"} (Effective Effort and
#' Depletion Prior), then this function will look for columns called
#' \code{bbmsy.effEdepP} and \code{ffmsy.effEdepP}.
#'
#' @return
#' Data frame like \code{dat} but with additional columns containing stock
#' status category information.
#'
#' @note
#' The data frame \code{dat} and the column names are created in \file{output.R}
#' using results from the \emph{sraplus} analysis.
#'
#' This function calculates two sets of stock categories. \code{Cat3} has three
#' levels and forms the basis of the SOFIA analysis, while \code{Cat4} has four
#' levels corresponding to the different rectangles of a Kobe plot.
#'
#' @author Rishi Sharma, with a contribution by Arni Magnusson.
#'
#' @seealso
#' \code{\link{plotCat}} plots a summary of stock status categories.
#'
#' \code{\link{TSAF-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' calcCat(newResTab, method="effDepP")
#' }
#'
#' @aliases compCat
#'
#' @export

calcCat <- function(dat, method="cmsy.naive")
{
  tmpDat <- dat
  sep <- if(method == "") "" else "."

  ## for 4 categories
  tmpDat$trueCat4 <- ifelse(dat$bbmsy > 1, ifelse(dat$ffmsy < 1, 1, 2),
                     ifelse(dat$ffmsy < 1, 3, 4))
  tmpDat$estCat4 <- ifelse(dat[, paste("bbmsy", method, sep=sep)] > 1,
                    ifelse(dat[, paste("ffmsy", method, sep=sep)] < 1, 1, 2),
                    ifelse(dat[, paste("ffmsy", method, sep=sep)] < 1, 3, 4))
  tmpDat$confMat4 <- (tmpDat$trueCat4 - 1) * 4 + tmpDat$estCat4

  # for 3 categories
  tmpDat$trueCat3 <- ifelse(dat$bbmsy < 0.8, 3, ifelse(dat$bbmsy < 1.2, 2, 1))
  tmpDat$estCat3 <- ifelse(dat[, paste("bbmsy", method, sep=sep)] < 0.8, 3,
                    ifelse(dat[, paste("bbmsy", method, sep=sep)] < 1.2, 2, 1))
  tmpDat$confMat3 <- (tmpDat$trueCat3 - 1) * 3 + tmpDat$estCat3

  tmpDat
}

#' @export

## compCat() was an older name for calcCat() that was used in many TSAF scripts,
## so we provide historical support.

compCat <- function(...)
{
  ## .Deprecated("calcCat")
  calcCat(...)
}
