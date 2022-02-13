#' Add Driors
#'
#' Add driors (data and priors) column to \sQuote{stocks} object.
#'
#' @param stocks \code{tibble} containing \code{stock}, \code{taxa}, and
#'        \code{data}.
#' @param priors data frame containing \code{stock}, \code{initial_state},
#'        \code{initial_state_cv}, \code{terminal_state}, and
#'        \code{terminal_state_cv}.
#' @param stocks.combined whether to use the same priors for all stocks.
#' @param shape_prior passed to \code{format_driors}.
#' @param b_ref_type passed to \code{format_driors}.
#' @param growth_rate_prior passed to \code{format_driors}.
#' @param growth_rate_prior_cv passed to \code{format_driors}.
#' @param \dots additional arguments passed to \code{format_driors}.
#'
#' @details
#' If \code{stocks.combined = TRUE} then the priors where \code{stock = All} are
#' used for all stocks.
#'
#' If \code{stocks.combined = FALSE} then the priors are read from
#' stock-specific rows in the \code{priors} data frame.
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
#' addDriors(stocks, priors, stocks.combined=TRUE)
#' addDriors(stocks, priors, stocks.combined=FALSE)
#' }
#'
#' @importFrom sraplus format_driors
#' @importFrom stats na.omit
#'
#' @export

addDriors <- function(stocks, priors, stocks.combined, shape_prior=2,
                      b_ref_type="k", growth_rate_prior=NA,
                      growth_rate_prior_cv=0.2, ...)
{
  driors <- list()
  for(i in seq_len(nrow(stocks)))
  {
    ## p is the row number for the priors data frame
    p <- if(stocks.combined) match("All", priors$stock)
         else match(stocks$stock[i], priors$stock)
    driors[[i]] <- format_driors(
      taxa = stocks$taxa[i],
      shape_prior = 2,
      catch = stocks$data[[i]]$capture,
      years = stocks$data[[i]]$year,
      initial_state = priors$initial_state[p],
      initial_state_cv = priors$initial_state_cv[p],
      b_ref_type = "k",
      terminal_state = priors$terminal_state[p],
      terminal_state_cv = priors$terminal_state_cv[p],
      effort = na.omit(stocks$data[[i]])$effort,
      effort_years = na.omit(stocks$data[[i]])$year,
      growth_rate_prior = NA,
      growth_rate_prior_cv = 0.2,
      ...)
  }
  stocks$driors <- driors
  stocks
}
