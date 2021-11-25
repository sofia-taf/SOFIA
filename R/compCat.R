#' Compute Categories
#'
#' Calculate stock status categories from B/Bmsy and F/Fmsy, and add as columns
#' to an existing data frame.
#'
#' @param dat a data frame containing columns named \code{bbmsy.*} and
#'        \code{ffmsy.*}.
#' @param method a string indicating which method was used to estimate B/Bmsy
#'        and F/Fmsy.
#'
#' @details
#' The column names in \code{dat} should contain the \code{method} name as a
#' suffix. For example, if \code{method = "effEdepP"}, then this function will
#' look for columns called \code{bbmsy.effEdepP} and \code{ffmsy.effEdepP}.
#'
#' @return
#' Data frame like \code{dat} but with additional columns containing stock
#' status category information.
#'
#' @seealso
#' \code{\link{TSAF-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' compCat(newResTab, method="effDepP")
#' }
#'
#' @export

compCat <- function(dat, method="cmsy.naive")
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
