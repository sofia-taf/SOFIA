#' List Repositories
#'
#' List all GitHub repositories owned by an organization or user.
#'
#' @param owner name of a GitHub organization or user.
#' @param full whether to include owner name in output.
#'
#' @return String vector of repository names.
#'
#' @note
#' This function uses the \href{https://docs.github.com/en/rest}{GitHub API} to
#' fetch repository names, using base R.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{gitClone}} clones a repository from GitHub.
#'
#' \code{\link{gitCloneAll}} clones all SOFIA-TAF repositories from GitHub.
#'
#' \code{\link{SOFIA-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' gitRepos()
#' gitRepos("arni-magnusson", full=TRUE)
#' }
#'
#' @export

gitRepos <- function(owner="sofia-taf", full=FALSE)
{
  # Parse JSON
  value <- function(key, txt)
  {
    v <- grep(key, txt, value=TRUE)
    v <- gsub("\"", "", v)
    v <- gsub("^.*: (.*),$", "\\1", v)
    v
  }

  # Examine how many pages of 100 repos each
  txt <- readLines(file.path("https://api.github.com/users", owner))
  n <- as.integer(value("public_repos", txt))
  pages <- 1 + n %/% 100

  # Read pages
  repos <- character()
  for(i in 1:pages)
  {
    query <- paste0("https://api.github.com/users/", owner,
                    "/repos?per_page=100&page=", i)
    txt <- readLines(query)
    repos <- c(repos, value("full_name", txt))
  }

  if(!full)
    repos <- basename(repos)
  repos
}
