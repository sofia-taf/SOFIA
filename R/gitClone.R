#' gitClone
#'
#' Clone a repository from GitHub.
#'
#' @param repo GitHub repository of the form \code{"repo"} or
#'        \code{"owner/repo"}.
#' @param topdir local directory above the repository.
#' @param method cloning method, either \code{"https"} or \code{"ssh"}.
#' @param tree whether the organize repositories in a nested SOFIA tree, see
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
#' repository names, such as \code{2022Area37Demo} and clone to a nested
#' subdirectory \code{2022/Area37/Demo} instead of \code{2022Area37Demo}. For
#' repository names that do not follow this YearAreaAnalysis pattern, the value
#' of \code{tree} has no effect.
#'
#' @return String containing the cloning command.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{gitRepos}} fetches a complete list of GitHub repositories.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' gitClone("2022Area37Demo")
#' gitClone("arni-magnusson/areaplot", "~/git/arni-magnusson", method="ssh")
#' }
#'
#' @export

gitClone <- function(repo, topdir="c:/git/sofia-taf", tree=TRUE, method="https")
{
  if(!grepl("/", repo))
    repo <- file.path("sofia-taf", repo)
  prefix <- if(method == "https") "https://github.com/" else "git@github.com:"
  uri <- paste0(prefix, repo)

  subdir <- basename(repo)
  if(tree && grepl("^20[0-9][0-9]Area[0-9][0-9].+", subdir))
    subdir <- paste(substring(subdir, c(1,5,11), c(4,10,255)), collapse="/")

  os <- Sys.info()[["sysname"]]
  if(grepl("^[A-Za-z]:",topdir) && os!="Windows")
    stop(topdir, " is not a ", os, " path, please specify 'topdir'")

  cmd <- paste("git clone", uri, file.path(topdir, subdir))
  system(cmd)

  invisible(cmd)
}
