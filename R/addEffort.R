#' Add Effort
#'
#' Add effort column to catch data.
#'
#' @param catch \code{tibble} containing \code{year}, \code{stock}, and
#'        \code{capture}.
#' @param effort \code{tibble} containing \code{year}, \code{stock}, and
#'        \code{effort}.
#' @param stocks.combined whether to use the same effort data for all stocks.
#'
#' @details
#' If \code{stocks.combined = TRUE} then the effort data for \code{stock$All} is
#' used for all stocks.
#'
#' If \code{stocks.combined = FALSE} then the catch and effort data are paired
#' by year and stock.
#'
#' @return
#' Data frame like \code{catch} but with the additional column \code{effort}.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{merge}} is the underlying function used to merge the two data
#' frames.
#'
#' \code{\link{addDriors}} adds a driors column to stocks.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' addEffort(catch, effort, stocks.combined=TRUE)
#' addEffort(catch, effort, stocks.combined=FALSE)
#' }
#'
#' @export

addEffort <- function(catch, effort, stocks.combined)
{
  x <- if(stocks.combined)
         merge(catch, effort[effort$stock=="All",c("year","effort")], by="year",
               all.x=TRUE, sort=FALSE)
       else
         merge(catch, effort, by=c("year","stock"), all.x=TRUE, sort=FALSE)
  x <- x[order(x$year, x$stock),]
  rownames(x) <- NULL
  x
}