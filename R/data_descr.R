#' Dictionary for translating model variable names
#'
#' A combined dataset containing all model variable dictionaries merged using bind_rows(). For details see \link{create_dictionaries}.
#' Includes variable names and metadata required for obtaining data from different sources.
#'
#' @format A tibble with 226 rows and 33 variables:
#' \describe{
#'   \item{model_varname}{Variable name in the model equations, must be unique}
#'   \item{full_name}{Full name/description of the variable}
#'   \item{variable_code}{Identifier of the variable if applicable, e.g. Eurostat variable code}
#'   \item{database}{Name of the database (e.g., "eurostat", "edgar", "statcan", "imf")}
#'   \item{dataset_id}{Identifier of the dataset where the variable is available}
#'   \item{var_col}{Name of variable/column in dataset}
#'   \item{freq}{Frequency of the variable in the dataset. \code{"m"} for monthly, \code{"q"} for quarterly, \code{"a"} for annual}
#'   \item{geo}{ISO 3166-1 alpha-2 country code or region identifier (lowercase)}
#'   \item{unit}{Unit in which the variable is measured}
#'   \item{s_adj}{Seasonal adjustment indicator}
#'   \item{nace_r2}{Identifier for NACE Rev. 2 classification where applicable}
#'   \item{cpa2_1}{Eurostat identifier for Classification of Products by Activity (CPA)}
#'   \item{sector}{Sector classification where applicable}
#'   \item{direct}{Direction indicator where applicable}
#'   \item{age}{Age category where applicable}
#'   \item{partner}{Partner country or entity where applicable}
#'   \item{finpos}{Financial position indicator where applicable}
#'   \item{p_adj}{Price adjustment indicator where applicable}
#'   \item{meat}{Meat type classification where applicable}
#'   \item{citizen}{Citizenship status where applicable}
#'   \item{wstatus}{Work status where applicable}
#'   \item{tra_oper}{Transport operation classification where applicable}
#'   \item{ipcc_sector}{EDGAR IPCC National Greenhouse Gas Inventories sector code}
#'   \item{GEO}{Geographical location for StatCan data (uppercase)}
#'   \item{Seasonal adjustment}{Seasonal adjustment indicator for StatCan data}
#'   \item{North American Industry Classification System (NAICS)}{Classification of industries for StatCan data}
#'   \item{North American Product Classification System (NAPCS)}{Product classification for StatCan data}
#'   \item{Prices}{Price level for specific categories in StatCan data}
#'   \item{Type of fuel}{Fuel type specification for StatCan data}
#'   \item{Products and product groups}{Product categorization for StatCan data}
#'   \item{ref_area}{Geographical reference area for IMF data}
#'   \item{commodity}{Type of commodity for IMF data}
#'   \item{unit_measure}{Unit of measurement for IMF data}
#' }
#'
#' @note Should new filters be necessary, users can simply add additional columns to the dictionary with names
#' that exactly match how they appear in the relevant retrieval function. For example, if using the eurostat
#' R package, statcanR, or imf.data, the column names should match exactly as they appear in the returned data
#' from these packages. If for example a statcanR query returns a column called \code{Socio-economic objectives},
#' users should add that exact column name (including all spaces and other characters) to the dictionary.
#'
#' @source Combined from multiple data sources including Eurostat, EDGAR, StatCan, and IMF.
"dict"

#' Dictionary for translating Eurostat model variable names
#'
#' A dataset containing Eurostat model variable names and all information required
#' for retrieving data from the Eurostat database. For details see \link{create_dictionaries}.
#'
#' @format A tibble with 167 rows and 22 variables:
#' \describe{
#'   \item{model_varname}{Variable name in the model equations, must be unique}
#'   \item{full_name}{Full name/description of the variable}
#'   \item{database}{Name of the database, always "eurostat" for this dictionary}
#'   \item{variable_code}{Identifier of the variable, e.g. Eurostat variable code}
#'   \item{dataset_id}{Identifier of the Eurostat dataset where the variable is available}
#'   \item{var_col}{Name of variable/column in dataset}
#'   \item{freq}{Frequency of the variable in the dataset. \code{"a"} for annual, \code{"q"} for quarterly, \code{"m"} for monthly}
#'   \item{geo}{ISO 3166-1 alpha-2 country code, e.g. "DE" for Germany}
#'   \item{unit}{Eurostat unit in which the variable is measured}
#'   \item{s_adj}{Eurostat seasonal adjustment indicator}
#'   \item{nace_r2}{Eurostat identifier for NACE Rev. 2 classification}
#'   \item{cpa2_1}{Eurostat identifier for Classification of Products by Activity (CPA)}
#'   \item{sector}{Sector classification where applicable}
#'   \item{direct}{Direction indicator where applicable}
#'   \item{age}{Age category where applicable}
#'   \item{partner}{Partner country or entity where applicable}
#'   \item{finpos}{Financial position indicator where applicable}
#'   \item{p_adj}{Price adjustment indicator where applicable}
#'   \item{meat}{Meat type classification where applicable}
#'   \item{citizen}{Citizenship status where applicable}
#'   \item{wstatus}{Work status where applicable}
#'   \item{tra_oper}{Transport operation classification where applicable}
#' }
#'
#' @note Should new filters be necessary, users can simply add additional columns to the dictionary with names
#' that exactly match how they appear in the eurostat R package. For example, if a eurostat query returns a column
#' with a specific name, that exact name should be used as a column in this dictionary.
#'
#' @source Eurostat database, available at \url{https://ec.europa.eu/eurostat}.
"dict_eurostat"

#' Dictionary for translating EDGAR model variable names
#'
#' A dataset containing EDGAR (Emissions Database for Global Atmospheric Research) model variable names
#' and all information required for obtaining emissions data. For details see \link{create_dictionaries}.
#'
#' To see all details on IPCC sectors see \url{https://www.ipcc-nggip.iges.or.jp/efdb/find_ef.php?reset=} and \url{https://www.ipcc-nggip.iges.or.jp/public/2006gl/index.html}
#'
#' @format A tibble with 45 rows and 7 variables:
#' \describe{
#'   \item{ipcc_sector}{EDGAR IPCC National Greenhouse Gas Inventories sector code,
#'   see \url{https://www.ipcc-nggip.iges.or.jp/efdb/find_ef.php?reset=} and \url{https://www.ipcc-nggip.iges.or.jp/public/2006gl/index.html}}
#'   \item{full_name}{Full name/description of the emission source}
#'   \item{model_varname}{Variable name in the model equations, must be unique}
#'   \item{dataset_id}{URL link to the dataset file on the web}
#'   \item{database}{Name of the database, always "edgar" for this dictionary}
#'   \item{geo}{ISO 3166-1 alpha-2 country code, e.g. "DE" for Germany}
#'   \item{freq}{Frequency of the variable in the dataset, e.g. \code{"m"} for monthly}
#' }
#'
#' @note Should new filters be necessary, users can simply add additional columns to the dictionary with names
#' that exactly match how they appear in the EDGAR data retrieval functions.
#'
#' @source EDGAR database, available at \url{https://edgar.jrc.ec.europa.eu/dataset_ghg80}.
"dict_edgar"

#' Dictionary for economic model identities
#'
#' A dataset containing definitions of economic identities used in the model.
#' These represent mathematical relationships between variables that must hold by definition. For details see \link{create_dictionaries}.
#'
#' @format A tibble with 7 rows and 3 variables:
#' \describe{
#'   \item{model_varname}{Variable name of the identity in the model equations, must be unique}
#'   \item{full_name}{Full name/description of the identity}
#'   \item{variable_code}{Additional identifier for the identity where applicable}
#' }
#'
#' @note These identities represent accounting relationships and mathematical constraints
#' that must be satisfied within the economic model.
#'
#' @source Own compilation based on standard economic accounting relationships.
"dict_identities"

#' Dictionary for translating StatCan model variable names
#'
#' A dataset containing StatCan model variable names and metadata for data retrieval. For details see \link{create_dictionaries}.
#'
#' @format A tibble with 5 rows and 12 variables:
#' \describe{
#'   \item{model_varname}{Unique variable name used in model equations}
#'   \item{full_name}{Full description of the variable}
#'   \item{database}{Name of the data source, here \code{"statcan"}}
#'   \item{dataset_id}{Identifier of the StatCan dataset}
#'   \item{freq}{Frequency of the data, with \code{"m"} for monthly}
#'   \item{GEO}{Geographical location, typically \code{"Canada"}}
#'   \item{Seasonal adjustment}{Indicates if data is seasonally adjusted, where applicable}
#'   \item{North American Industry Classification System (NAICS)}{Classification of industries, based on NAICS}
#'   \item{North American Product Classification System (NAPCS)}{Product classification system}
#'   \item{Prices}{Price level for specific categories, if applicable}
#'   \item{Type of fuel}{Specifies the type of fuel, for example \code{"Regular unleaded"}}
#'   \item{Products and product groups}{Specific product or group as classified by StatCan}
#' }
#'
#' @note Should new filters be necessary, users can simply add additional columns to the dictionary with names
#' that exactly match how they appear in the statcanR package. For example, if a statcanR query returns a column
#' called \code{Socio-economic objectives}, users should add that exact column name (including all spaces and other characters) to the dictionary.
#'
#' @source Data retrieved from StatCan, available at \url{https://www.statcan.gc.ca/}.
"dict_statcan"

#' Dictionary for translating IMF model variable names
#'
#' A dataset containing IMF model variable names and metadata for data retrieval. For details see \link{create_dictionaries}.
#'
#' @format A tibble with 1 row and 8 variables:
#' \describe{
#'   \item{model_varname}{Unique variable name used in model equations}
#'   \item{full_name}{Full description of the variable}
#'   \item{database}{Name of the data source, here \code{"imf"}}
#'   \item{dataset_id}{Identifier of the IMF dataset}
#'   \item{freq}{Frequency of the data, with \code{"M"} for monthly}
#'   \item{ref_area}{Geographical reference area, e.g., \code{"W00"} for world}
#'   \item{commodity}{Type of commodity, such as \code{"POILAPSP"} for oil}
#'   \item{unit_measure}{Unit of measurement, e.g., \code{"USD"} for U.S. dollars}
#' }
#'
#' @note Should new filters be necessary, users can simply add additional columns to the dictionary with names
#' that exactly match how they appear in the imf.data package. For example, if an imf.data query returns a column
#' with a specific name, that exact name should be used as a column in this dictionary.
#'
#' @source Data retrieved from IMF, available at \url{https://www.imf.org/en/Data}.
"dict_imf"

#' Sample Input for the OSEM Model
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
