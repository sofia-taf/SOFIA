#' Add Effort
#'
#' Add effort column to catch data.
#'
#' @param catch \code{tibble} containing \code{year}, \code{stock}, and
#'        \code{capture}.
#' @param effort \code{tibble} containing \code{year}, \code{stock}, and
#'        \code{effort}.
#' @param same.effort whether to use the same effort data for all stocks.
#'
#' @details
#' If \code{same.effort = TRUE} then the effort data for \code{stock$All} is
#' used for all stocks.
#'
#' If \code{same.effort = FALSE} then the catch and effort data are paired by
#' year and stock.
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
#' \code{\link{addIndex}} adds an index column to catch data.
#'
#' \code{\link{addDriors}} adds a driors column to stocks.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' addEffort(catch, effort, same.effort=TRUE)
#' addEffort(catch, effort, same.effort=FALSE)
#' }
#'
#' @export

addEffort <- function(catch, effort, same.effort)
{
  ## 1a  Make sure effort table contains stock 'All' if same.effort=TRUE
  if(same.effort && !("All" %in% effort$stock))
    stop("using same.effort=TRUE, so effort table must contain stock='All'")

  ## 1b  Make sure catch and effort have matching stocks if same.effort=FALSE
  cstocks <- sort(unique(catch$stock))
  estocks <- sort(unique(effort$stock))
  if(!same.effort && !any(cstocks %in% estocks))
    stop("using same.effort=FALSE, so stocks in 'effort' should match 'catch'")
  if(!same.effort && !all(cstocks %in% estocks))
    warning("using same.effort=FALSE, but ", sum(!(cstocks %in% estocks)),
            " stock(s) in 'catch' not found in 'effort'")

  ## 2  Merge and sort
  x <- if(same.effort)
         merge(catch, effort[effort$stock=="All",c("year","effort")], by="year",
               all.x=TRUE, sort=FALSE)
       else
         merge(catch, effort, by=c("year","stock"), all.x=TRUE, sort=FALSE)
  x <- x[order(x$year, x$stock),]
  rownames(x) <- NULL
  x
}
