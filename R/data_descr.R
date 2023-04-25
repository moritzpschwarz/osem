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
#'   \item{var_col}{Name of variable/column in Eurostat dataset}
#'   \item{nace_r2}{Eurostat identifier for NACE Rev. 2 classification}
#'   \item{cpa2_1}{Eurostat identifier for Classification of Products by Activity (CPA)}
#' }
#' @source Own compilation, codes from \url{https://ec.europa.eu/eurostat}
"dict"


#' Sample Input for the Aggregate Model
#'
#' A dataset containing input data that has been downloaded using the functions in the aggregate.model package.
#' This dataset is supposed to be used for testing and to enable out-of-the-box estimation.
#'
#' @format A tibble with 1200 rows and 9 variables:
#' \describe{
#'   \item{unit}{Unit according to Eurostat.}
#'   \item{s_adj}{Indication whether variable was seasonally adjusted or not. SCA refers to seasonal and calendar adjusted data.}
#'   \item{na_item}{Variable Name according to the aggregate.model dictionary.}
#'   \item{geo}{Geographic location for which the data was downloaded.}
#'   \item{time}{The date of the observation.}
#'   \item{nace_r2}{If applicable, the NACE2 sector code.}
#'   \item{p_adj}{If applicable, price adjustment.}
#'   \item{cpa2_1}{If applicable, Classification of Products by Activity.}
#' }
#' @source Own compilation, data from \url{https://ec.europa.eu/eurostat}, downloaded with aggregate.model::run_model()
"sample_input"
