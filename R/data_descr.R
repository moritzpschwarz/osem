#' Dictionary for translating model variable names to Eurostat codes
#'
#' A dataset containing the model variable names, Eurostat code, full name, and
#' dataset id where the variable can be found.
#'
#' @format A tibble with 13 rows and 4 variables:
#' \describe{
#'   \item{eurostat_code}{Eurostat identifier of the variable}
#'   \item{model_varname}{Variable name in the model equations}
#'   \item{full_name}{Full name of the variable according to Eurostat}
#'   \item{dataset_id}{Eurostat identifier for the dataset where the variable is available}
#' }
#' @source Own compilation, codes from \url{https://ec.europa.eu/eurostat}
"dict"
