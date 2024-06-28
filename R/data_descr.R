#' Dictionary for translating model variable names
#'
#' A dataset containing the model variable names and all information required
#' for obtaining the data.
#'
#' @format A tibble with 40 rows and 14 variables:
#' \describe{
#'   \item{model_varname}{Variable name in the model equations, must be unique}
#'   \item{full_name}{Full name/description of the variable}
#'   \item{database}{Name of the database. Internally implemented are \code{"eurostat"} and \code{"edgar"}}
#'   \item{variable_code}{Identifier of the variable if applicable, e.g. Eurostat variable code}
#'   \item{dataset_id}{Identifier of the dataset where the variable is available, e.g. Eurostat dataset code or link to file on the web}
#'   \item{var_col}{Name of variable/column in dataset}
#'   \item{freq}{Frequency of the variable in the dataset. \code{"m"} for monthly, \code{"q"} for quarterly}
#'   \item{geo}{ISO 3166-1 alpha-2 country code}
#'   \item{unit}{Eurostat unit in which the variable is measured, e.g. to choose between different measurements for the same variable}
#'   \item{s_adj}{Eurostat seasonal adjustment or not}
#'   \item{nace_r2}{Eurostat identifier for NACE Rev. 2 classification}
#'   \item{ipcc_sector}{EDGAR IPCC National Greenhouse Gas Inventories,
#'   see \url{https://www.ipcc-nggip.iges.or.jp/public/2006gl/index.html}.
#'   \code{"TOTAL"} is not an official IPCC code but is internally interpreted to use country totals.}
#'   \item{cpa2_1}{Eurostat identifier for Classification of Products by Activity (CPA)}
#'   \item{siec}{Standard International Energy Product Classification, e.g. for Eurostat}
#'   \item{...}{Additional filters across the database. Need to have exactly the same column name as in the respective database}
#' }
#' @source Own compilation, codes from \url{https://ec.europa.eu/eurostat} and \url{https://edgar.jrc.ec.europa.eu/dataset_ghg70}
"dict"


#' Sample Input for the Aggregate Model
#'
#' A dataset containing input data that has been downloaded using the functions in the osem package.
#' This dataset is supposed to be used for testing and to enable out-of-the-box estimation.
#'
#' @format A tibble with 1200 rows and 9 variables:
#' \describe{
#'   \item{unit}{Unit according to Eurostat.}
#'   \item{s_adj}{Indication whether variable was seasonally adjusted or not. SCA refers to seasonal and calendar adjusted data.}
#'   \item{na_item}{Variable Name according to the osem dictionary.}
#'   \item{geo}{Geographic location for which the data was downloaded.}
#'   \item{time}{The date of the observation.}
#'   \item{nace_r2}{If applicable, the NACE2 sector code.}
#'   \item{p_adj}{If applicable, price adjustment.}
#'   \item{cpa2_1}{If applicable, Classification of Products by Activity.}
#' }
#' @source Own compilation, data from \url{https://ec.europa.eu/eurostat}, downloaded with osem::run_model()
"sample_input"
