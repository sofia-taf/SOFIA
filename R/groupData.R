#' Group Data
#'
#' Group primary single-stock data files in subdirectories, depending on what
#' columns each data file contains.
#'
#' @param dir is the directory containing the primary data files.
#' @param quiet is whether to suppress screen output.
#'
#' @return
#' Files are copied into subdirectories. As a byproduct, a list is returned,
#' describing which subdirectories contain which data files.
#'
#' @note
#' A primary data file can have a filename such as
#' \file{Yellowtail_snapper_Mexico.csv} and columns such as
#' \code{stockid|scientificname|commonname|year|catch|stocklong}.
#'
#' In addition, the data file may have columns called \code{best_effort} and/or
#' \code{best_index}, containing the effort and/or index series to be used in
#' the SOFIA analysis.
#'
#' This function creates four subdirectories:
#' \enumerate{
#' \item \code{both} - for data files containing both effort and index data
#' \item \code{effort} - for data files containing effort data (and possibly
#'        also index)
#' \item \code{index} - for data files containing index data (and possibly also
#'       effort)
#' \item \code{neither} - for data files containing neither effort nor index
#'       data
#' }
#'
#' The object returned has an attribute \code{count}, showing the number of data
#' files in each subdirectory. The number of original (unique) data files will
#' be:
#' \preformatted{
#'   both + (effort-both) + (index-both) + neither
#' }
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' groupData("Data_files_Area_31_3")
#' groupData("Data_files_Area_31_3", quiet=TRUE)
#' }
#'
#' @importFrom utils read.csv
#'
#' @export

groupData <- function(dir, quiet=FALSE)
{
  if(!dir.exists(dir))
    stop("'", dir, "' not found")

  ## 1  Import CSV files
  files <- dir(dir, pattern="\\.csv$", full=TRUE)
  if(length(files) == 0)
  {
    if(!quiet)
      message("No CSV files found in '", dir, "'")
    return()
  }
  csv <- lapply(files, read.csv)
  names(csv) <- basename(files)

  ## 2  Create directories
  unlink(file.path(dir, c("both","effort","index","neither")), recursive=TRUE)
  dir.create(file.path(dir, "both"))
  dir.create(file.path(dir, "effort"))
  dir.create(file.path(dir, "index"))
  dir.create(file.path(dir, "neither"))

  ## 3  Copy files into directories
  for(i in seq_along(files))
  {
    n <- tolower(names(csv[[i]]))
    if("best_effort" %in% n && "best_index" %in% n)
      file.copy(files[i], file.path(dir, "both"))
    if("best_effort" %in% n)
      file.copy(files[i], file.path(dir, "effort"))
    if("best_index" %in% n)
      file.copy(files[i], file.path(dir, "index"))
    if(!("best_effort" %in% n) && !("best_index" %in% n))
      file.copy(files[i], file.path(dir, "neither"))
    ## Report when column names look suspicious
    if(!("best_effort" %in% n) && any(grepl("effort", tolower(n))))
      warning(basename(files[i]), "\n  has effort data (",
              paste(n[grep("effort", tolower(n))], collapse=", "),
              ") but no 'best_effort'")
    if(!("best_index" %in% n) && any(grepl("index", tolower(n))))
      warning(basename(files[i]), "\n has index data (",
              paste(n[grep("index", tolower(n))], collapse=", "),
              ") but no 'best_index'")
  }

  ## 4  Return list
  out <- list(both=dir(file.path(dir, "both")),
              effort=dir(file.path(dir, "effort")),
              index=dir(file.path(dir, "index")),
              neither=dir(file.path(dir, "neither")))
  attr(out, "count") <- c(sapply(out, length), unique=length(files))
  if(quiet)
    invisible(out)
  else
    out
}
