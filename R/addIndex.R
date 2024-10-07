#' Add Index
#'
#' Add index column to catch data.
#'
#' @param catch \code{tibble} containing \code{year}, \code{stock}, and
#'        \code{capture}.
#' @param index \code{tibble} containing \code{year}, \code{stock}, and
#'        \code{index}.
#' @param same.index whether to use the same index data for all stocks.
#'
#' @details
#' If \code{same.index = TRUE} then the index data for \code{stock$All} is used
#' for all stocks.
#'
#' If \code{same.index = FALSE} then the catch and index data are paired by year
#' and stock.
#'
#' @return
#' Data frame like \code{catch} but with the additional column \code{index}.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{merge}} is the underlying function used to merge the two data
#' frames.
#'
#' \code{\link{addEffort}} adds an effort column to catch data.
#'
#' \code{\link{addDriors}} adds a driors column to stocks.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' addIndex(catch, index, same.index=TRUE)
#' addIndex(catch, index, same.index=FALSE)
#' }
#'
#' @export

addIndex <- function(catch, index, same.index)
{
  # 1a  Make sure index table contains stock 'All' if same.index=TRUE
  if(same.index && !("All" %in% index$stock))
    stop("using same.index=TRUE, so index table must contain stock='All'")

  # 1b  Make sure catch and index have matching stocks if same.index=FALSE
  cstocks <- sort(unique(catch$stock))
  estocks <- sort(unique(index$stock))
  if(!same.index && !any(cstocks %in% estocks))
    stop("using same.index=FALSE, so stocks in 'index' should match 'catch'")
  if(!same.index && !all(cstocks %in% estocks))
    warning("using same.index=FALSE, but ", sum(!(cstocks %in% estocks)),
            " stock(s) in 'catch' not found in 'index'")

  # 2  Merge and sort
  x <- if(same.index)
         merge(catch, index[index$stock=="All",c("year","index")], by="year",
               all.x=TRUE, sort=FALSE)
       else
         merge(catch, index, by=c("year","stock"), all.x=TRUE, sort=FALSE)
  x <- x[order(x$year, x$stock),]
  rownames(x) <- NULL
  x
}
