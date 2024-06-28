#' \code{osem} package
#'
#' The Package for the Aggregate Model
#'
#' See the README on
#' \href{https://cran.r-project.org/package=osem/README.html}{CRAN}
#' or \href{https://github.com/moritzpschwarz/osem#readme}{GitHub}
#'
#' @docType package
#' @name osem
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
