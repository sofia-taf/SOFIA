#' gitCloneAll
#'
#' Clone all SOFIA-TAF repositories from GitHub.
#'
#' @param topdir local empty directory to clone to.
#' @param method cloning method, either \code{"https"} or \code{"ssh"}.
#' @param tree whether to organize repositories in a nested SOFIA tree, see
#'        details below.
#'
#' @details
#' Generally, the default \code{method = "https"} works best in Windows, while
#' \code{method = "ssh"} works best in Linux.
#'
#' When \code{tree = TRUE}, this function will recognize standard SOFIA
#' repository names such as \code{2022Area37Demo} and clone to a nested
#' subdirectory \code{2022/Area37/Demo} instead of \code{2022Area37Demo}.
#'
#' @return String vector of cloned repositories.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{gitRepos}} fetches a complete list of GitHub repositories.
#'
#' \code{\link{gitClone}} clones a repository from GitHub.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' gitCloneAll()
#' gitCloneAll("~/git/sofia-taf", method="ssh")
#' }
#'
#' @export

gitCloneAll <- function(topdir="c:/git/sofia-taf", method="https", tree=TRUE)
{
  ## if(dir.exists(topdir))
  repos <- gitRepos("sofia-taf")
  sapply(repos, gitClone, topdir=topdir, method=method, tree=tree)

  invisible(repos)
}
