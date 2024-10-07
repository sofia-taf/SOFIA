#' Add Driors
#'
#' Add driors (data and priors) column to \sQuote{stocks} object.
#'
#' @param stocks \code{tibble} containing \code{stock} and \code{data}.
#' @param priors data frame containing \code{stock}, \code{initial_state},
#'        \code{initial_state_cv}, \code{terminal_state}, and
#'        \code{terminal_state_cv}.
#' @param same.priors whether to use the same priors for all stocks.
#' @param shape_prior passed to \code{format_driors}.
#' @param growth_rate_prior_cv passed to \code{format_driors}.
#' @param \dots additional arguments passed to \code{format_driors}.
#'
#' @details
#' If \code{same.priors = TRUE} then the priors where \code{stock = All} are
#' used for all stocks.
#'
#' If \code{same.priors = FALSE} then the priors are read from stock-specific
#' rows in the \code{priors} data frame.
#'
#' @return
#' A \code{tibble} like \code{stocks} but with the additional column
#' \code{driors}.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{format_driors}} is the underlying function used to construct the
#' \code{driors} column.
#'
#' \code{\link{addEffort}} adds an effort column to catch data.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' addDriors(stocks, priors, same.priors=TRUE)
#' addDriors(stocks, priors, same.priors=FALSE)
#' }
#'
#' @importFrom sraplus format_driors
#' @importFrom stats na.omit
#'
#' @export

addDriors <- function(stocks, priors, same.priors, shape_prior=2,
                      growth_rate_prior_cv=0.2, ...)
{
  # 1a  Make sure priors column 'terminal_state_cv' does not contain NA
  if(any(is.na(priors$terminal_state_cv)))
    stop("priors table column 'terminal_state_cv' must not contain NA values")

  # 1b  Make sure priors table contains stock 'All' if same.priors=TRUE
  if(same.priors && !("All" %in% priors$stock))
    stop("using same.priors=TRUE, so priors table must contain stock='All'")

  # 1c  Make sure catch and priors have matching stocks if same.priors=FALSE
  sstocks <- sort(stocks$stock)
  pstocks <- sort(priors$stock)
  if(!same.priors && !any(sstocks %in% pstocks))
    stop("using same.priors=FALSE, so stocks in 'priors' should match 'stocks'")
  if(!same.priors && !all(sstocks %in% pstocks))
    warning("using same.priors=FALSE, but ", sum(!(sstocks %in% pstocks)),
            " stock(s) in 'stocks' not found in 'priors'")

  # 2  Construct driors
  driors <- list()
  for(i in seq_len(nrow(stocks)))
  {
    if(same.priors && !("All" %in% priors$stock))
      stop("using same.priors=TRUE, so priors table must contain stock='All'")
    # p is the row number for the priors data frame
    p <- if(same.priors)
           match("All", priors$stock)
         else
           match(stocks$stock[i], priors$stock)
    # Set effort and index to default value if not in data
    effort <- if("effort" %in% names(stocks$data[[i]]))
                stocks$data[[i]]$effort[!is.na(stocks$data[[i]]$effort)]
              else formals(format_driors)$effort
    effort_years <- if("effort" %in% names(stocks$data[[i]]))
                      stocks$data[[i]]$year[!is.na(stocks$data[[i]]$effort)]
                    else formals(format_driors)$effort_years
    index <- if("index" %in% names(stocks$data[[i]]))
               stocks$data[[i]]$index[!is.na(stocks$data[[i]]$index)]
             else formals(format_driors)$index
    index_years <- if("index" %in% names(stocks$data[[i]]))
                     stocks$data[[i]]$year[!is.na(stocks$data[[i]]$index)]
                   else formals(format_driors)$index_year
    driors[[i]] <- format_driors(
      taxa = stocks$stock[i],
      catch = stocks$data[[i]]$capture,
      years = stocks$data[[i]]$year,
      effort = effort,
      effort_years = effort_years,
      index = index,
      index_years = index_years,
      # Prior section
      shape_prior = shape_prior,
      initial_state = priors$initial_state[p],
      initial_state_cv = priors$initial_state_cv[p],
      terminal_state = priors$terminal_state[p],
      terminal_state_cv = priors$terminal_state_cv[p],
      growth_rate_prior_cv = growth_rate_prior_cv,
      ...)
  }
  stocks$driors <- driors
  stocks
}
