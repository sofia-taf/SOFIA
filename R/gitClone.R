#' Clone
#'
#' Clone a repository from GitHub.
#'
#' @param repo GitHub repository of the form \code{"repo"} or
#'        \code{"owner/repo"}.
#' @param topdir local directory above the repository.
#' @param method cloning method, either \code{"https"} or \code{"ssh"}.
#' @param tree whether to organize repositories in a nested SOFIA tree, see
#'        details below.
#'
#' @details
#' The implicit default \code{repo} owner is \code{sofia-taf}. In other words,
#' if the user specifies a \code{"repo"} instead of \code{"owner/repo"}, this
#' function will automatically convert it to \code{"sofia-taf/repo"}.
#'
#' Generally, the default \code{method = "https"} works best in Windows, while
#' \code{method = "ssh"} works best in Linux.
#'
#' When \code{tree = TRUE}, this function will recognize standard SOFIA
#' repository names such as \code{2022Area37Demo} and
#' \code{WorkshopPriorsByStock} and create nested subdirectories on the user
#' machine:
#' \tabular{ll}{
#'   Repository on GitHub \tab Path on user machine\cr
#'   \code{2022Area37Demo} \tab \code{2022/Area37/Demo}\cr
#'   \code{WorkshopPriorsByStock} \tab \code{Workshop/PriorsByStock}\cr
#'   \code{SOFIA} \tab \code{SOFIA}
#' }
#' For repository names that do not start with a year \code{"20**"} or
#' \code{"Workshop"}, the value of \code{tree} has no effect.
#'
#' @return String containing the cloning command.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{gitRepos}} fetches a complete list of GitHub repositories.
#'
#' \code{\link{gitCloneAll}} clones all SOFIA-TAF repositories from GitHub.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' gitClone("2022Area37Demo")
#' gitClone("2022Area37Demo", "~/git/sofia-taf", method="ssh")
#' gitClone("arni-magnusson/areaplot", "~/git/arni-magnusson", method="ssh")
#' }
#'
#' @export

gitClone <- function(repo, topdir="c:/git/sofia-taf", method="https", tree=TRUE)
{
  if(!grepl("/", repo))
    repo <- file.path("sofia-taf", repo)
  prefix <- if(method == "https") "https://github.com/" else "git@github.com:"
  uri <- paste0(prefix, repo)

  subdir <- basename(repo)
  if(tree && grepl("^20[0-9][0-9]Area[0-9][0-9].+", subdir))
    subdir <- paste(substring(subdir, c(1,5,11), c(4,10,255)), collapse="/")
  if(tree && grepl("^Workshop.+", subdir))
    subdir <- paste(substring(subdir, c(1,9), c(8,255)), collapse="/")

  os <- Sys.info()[["sysname"]]
  if(grepl("^[A-Za-z]:",topdir) && os!="Windows")
    stop(topdir, " is not a ", os, " path, please specify 'topdir'")

  cmd <- paste("git clone", uri, file.path(topdir, subdir))
  system(cmd)

  invisible(cmd)
}
