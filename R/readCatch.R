#' Read Catch
#'
#' Read catch data and handle the \code{Total} column appropriately.
#'
#' @param file filename, \code{"bootstrap/data/catch.csv"} by default.
#' @param stocks.combined whether stocks are combined in the subsequent
#'        analysis.
#'
#' @details
#' If \code{stocks.combined = TRUE} then only the \code{Total} column is used
#' and renamed to \code{All}. This way, it will match the \code{All} column
#' found in the effort data and priors.
#'
#' If \code{stocks.combined = FALSE} then the \code{Total} column is discarded.
#'
#' @return
#' Data frame containing year in the first column and catch in other columns.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{read.csv}} is the underlying function used to read the catch
#' data.
#'
#' \code{\link{TSAF-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' readCatch("bootstrap/data/catch.csv", stocks.combined=TRUE)
#' readCatch(stocks.combined=FALSE)
#' }
#'
#' @importFrom utils read.csv
#'
#' @export

readCatch <- function(file="bootstrap/data/catch.csv", stocks.combined)
{
  if(!identical(stocks.combined,TRUE) && !identical(stocks.combined,FALSE))
    stop("stocks.combined must be either TRUE or FALSE")

  catch <- read.csv(file, check.names=FALSE, stringsAsFactors=TRUE,
                    fileEncoding="UTF-8")

  if(stocks.combined)
    catch <- data.frame(Year=catch$Year, All=catch$Total)
  else
    catch$Total <- NULL

  catch
}
