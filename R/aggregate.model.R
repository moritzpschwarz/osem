#' \code{aggregate.model} package
#'
#' The Package for the Aggregate Model
#'
#' See the README on
#' \href{https://cran.r-project.org/package=aggregate.model/README.html}{CRAN}
#' or \href{https://github.com/moritzpschwarz/aggregate.model#readme}{GitHub}
#'
#' @docType package
#' @name aggregate.model
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom rlang :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
