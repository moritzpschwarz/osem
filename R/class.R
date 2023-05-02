#' Simple constructor for class 'aggmod'
#'
#' @param x A list storing the aggregate model output.
#'
#' @return An object of class \code{"aggmod"}, which is a named list with four elements:
#' \describe{
#'   \item{args}{A named list storing the user arguments for the aggregate
#'   model.}
#'   \item{module_order}{The original but ordered specification.}
#'   \item{module_collection}{The above specification with two added columns
#'   that store the model object for each module and the dataset used for
#'   estimation, including fitted values for the dependent variable.}
#'   \item{full_data}{A tibble or data.frame containing the complete original
#'   data for the aggregate model and the fitted values of each module.}
#' }
#'
#' @keywords internal

new_aggmod <- function(x = list()) {

  stopifnot(is.list(x))
  class(x) <- "aggmod"

  validate_aggmod(x) # raises error if doesn't fit

  return(x)

}

#' Simple validator for class 'aggmod'
#'
#' @keywords internal

validate_aggmod <- function(x) {

  val <- unclass(x)

  stopifnot(length(val) == 5L)
  stopifnot(length(names(val)) == 5L)
  stopifnot(identical(names(val), c("args", "module_order", "module_collection", "full_data", "dictionary")))
  stopifnot(identical(class(val$args), "list"))
  stopifnot(any(class(val$module_order) %in% c("tbl_df", "tbl", "data.frame")))
  stopifnot(any(class(val$module_collection) %in% c("tbl_df", "tbl", "data.frame")))
  stopifnot(any(class(val$full_data) %in% c("tbl_df", "tbl", "data.frame")))
  stopifnot(any(class(val$dictionary) %in% c("tbl_df", "tbl", "data.frame")))

  # could & should add many more checks: number of columns in module_order etc.
  # but since is likely to change in the future, don't implement all of them

  return(x)

}
