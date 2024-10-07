#' Plot Categories
#'
#' Plot a summary of stock status categories by year: underfished (green), fully
#' fished (yellow), or overfished (red).
#'
#' @param dat data frame of stock time series, containing columns named
#'        \code{bbmsy} and \code{ffmsy}, as well as method-specific
#'        \code{bbmsy.*} and \code{ffmsy.*}.
#' @param method string indicating which method was used to estimate B/Bmsy and
#'        F/Fmsy.
#' @param cats either \code{3} or \code{4}, indicating whether to plot the stock
#'        status based on biomass only (3 categories) or based on biomass and
#'        fishing mortality (4 categories).
#' @param type string indicating the type of plot: \code{"count"},
#'        \code{"prop"}, or \code{"stock"}.
#' @param col vector of colors to override the default palette.
#' @param legend whether to add a legend (applies only if \code{type = "prop"}.
#'
#' @details
#' The first two columns in \code{dat} are treated as \code{'stock'} and
#' \code{'year'}, regardless of the actual column names.
#'
#' The last column names in \code{dat} should contain the \code{method} name as
#' a suffix. For example, if \code{method = "effEdepP"}, then this function will
#' look for columns called \code{bbmsy.effEdepP} and \code{ffmsy.effEdepP}.
#'
#' When plotting a \code{type = "prop"} plot, the legend format is tailored for
#' the \code{taf.png} device defaults. By passing \code{legend = FALSE}, the
#' user can manually draw a legend that fits some other plot size and aspect
#' ratio.
#'
#' @return
#' A \code{ggplot} object if \code{type} is \code{"count"} or \code{"stock"}, or
#' a table of percentages if \code{type} is \code{"prop"}.
#'
#' @author Rishi Sharma and Arni Magnusson.
#'
#' @seealso
#' \code{\link{ggplot}} is the underlying function used to produce the plot.
#'
#' \code{\link{calcCat}} calculates stock status categories.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' plotCat(stock.timeseries, method="effEdepP", cats=3, type="count")
#' plotCat(stock.timeseries, method="effEdepP", cats=3, type="prop")
#' plotCat(stock.timeseries, method="effEdepP", cats=3, type="stock")
#' }
#'
#' @aliases plotProp
#'
#' @importFrom ggplot2 aes geom_bar geom_raster ggplot theme_minimal
#'                     scale_fill_manual
#' @importFrom graphics abline box matplot par polygon title
#'
#' @export

plotCat <- function(dat, method="cmsy.naive", cats=4, type="count", col=NULL,
                    legend=TRUE)
{
  names(dat)[1:2] <- c("stock", "year")  # convert Stock->stock, yr->year

  # Create a new data frame with the categories
  status <- calcCat(dat, method=method)
  bbmsy.cols <- grep("bbmsy", names(status), value=TRUE)
  status <- status[, c("stock", "year", bbmsy.cols, "estCat3", "estCat4")]

  # Assign levels and col
  if(cats == 3)
  {
    levels <- c("Underfished", "Fully fished", "Overfished")
    status$estCat <- factor(levels[status$estCat3], levels=levels)
    # SOFIA 2024 report uses #1179be (blue) and #f07e29 (orange)
    # Here we use two hues of blue to distinguish between fully and underfished
    if(is.null(col))
      col <- c("#0e6cab", "#1179be", "#f07e29")  # darkblue, blue, orange
  }
  else
  {
    levels <- c("b>1,f<1", "b>1,f>1", "b<1,f<1", "b<1,f>1")
    status$estCat <- factor(levels[status$estCat4], levels=levels)
    if(is.null(col))
      col <- c("darkgreen", "orange", "yellow", "red")
  }

  # Plot
  if(type == "count")
  {
    year <- estCat <- NULL  # suppress R CMD check notes
    ggplot(status, aes(x=year, color=estCat)) +
      geom_bar(aes(fill=estCat), width=0.5) +
      theme_minimal() +
      scale_fill_manual(values=col)
  }
  else if(type == "prop")
  {
    if(legend)
      opar <- par(fig=c(0,0.8,0,1))
    percent <- 100 * prop.table(table(status$year, status$estCat), margin=1)
    # beg: areaplot(percent, col=col, ann=FALSE, xaxs="i", yaxs="i", border=NA)
    x <- as.integer(rownames(percent))
    y <- t(rbind(0, apply(percent, 1, cumsum)))
    matplot(x, y, type="n", ann=FALSE, xaxs="i", yaxs="i")
    xx <- c(x, rev(x))
    for(i in 1:(ncol(y)-1))
    {
      yy <- c(y[,i+1], rev(y[,i]))
      polygon(xx, yy, col=col[i], border=NA)
    }
    # end: areaplot(percent, col=col, ann=FALSE, xaxs="i", yaxs="i", border=NA)
    abline(h=c(20,40,60,80), col="lightgray", lty=2)
    title(xlab="Year", ylab="Stock status (%)")
    box()
    if(legend)
    {
      legend("right", rev(levels), fill=rev(col), border=NA, bty="n",
             inset=c(-0.3,0), xpd=NA)
      par(opar)
    }
    invisible(percent)
  }
  else if(type == "stock")
  {
    year <- stock <- estCat <- NULL  # suppress R CMD check notes
    ggplot(status, aes(x=year, y=stock, fill=estCat)) +
      geom_raster() +
      theme_minimal() +
      scale_fill_manual(values=col)
  }
}

#' @export

# plotProp() was an older name for plotCat() that was used in earlier SOFIA
# scripts, so we provide historical support.

plotProp <- function(...)
{
  # .Deprecated("plotCat")
  plotCat(...)
}
