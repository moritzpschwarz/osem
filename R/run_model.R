#' Runs the aggregate model
#'
#' Runs the aggregate model according to the given specification of modules.
#'
#' @param specification A tibble or data.frame with three columns. Column names must be:
#' 'type', 'dependent', and 'independent'. The column 'type' must contain for each row a
#' character of either 'd' (Identity) or 'n' (Definition - i.e. will be estimated).
#'   The column 'dependent' must contain the LHS (Y variables) and the column named
#' 'independent' containing the RHS (x variables separated by + and -).
#' @param dictionary A tibble or data.frame storing the Eurostat variable code
#'   in column 'eurostat_code' and the model variable name in 'model_varname'.
#'   If \code{download == TRUE} then the dictionary also requires a column named
#'   'dataset_id' that stores the Eurostat dataset id. When \code{NULL}, the
#'   \link[=dict]{default dictionary} is used.
#' @param inputdata_directory A path to .rds input files in which the data is
#'   stored. Can be \code{NULL} if \code{download == TRUE}.
#' @param filter_list A named list with one entry per Eurostat code that
#'   specifies the filter that is applied to choose the correct data series,
#'   e.g. geographical restrictions or whether seasonally adjusted or not. See
#'   \link{load_or_download_variables} for details.
#' @param download A logical value whether the data should be downloaded from
#'   Eurostat or will be provided directly via \code{inputdata_directory}.
#' @param save_to_disk A path to a directory where the final dataset will be
#'   saved, including the file name and ending. Not saved when \code{NULL}.
#' @param present A logical value whether the final aggregate model output
#'   should be presented or not. NOTE: not implemented yet.
#' @param quiet Logical with default = FALSE. Should messages be displayed?
#' These messages are intended to give more information about the estimation
#' and data retrieval process.
#' @param ... further arguments to be passed to \code{estimate_module}.
#'
#' @return An object of class \link[=new_aggmod]{aggmod}, which is a named list
#'   with four elements:
#' \describe{
#'   \item{args}{A named list storing the user arguments for the aggregate
#'   model.}
#'   \item{module_order_eurostatvars}{The original specification with translated
#'   variable names to Eurostat codes and arranged in order of estimation.}
#'   \item{module_collection}{The above specification with two added columns
#'   that store the model object for each module and the dataset used for
#'   estimation, including fitted values for the dependent variable.}
#'   \item{full_data}{A tibble or data.frame containing the complete original
#'   data for the aggregate model and the fitted values of each module.}
#' }
#'
#' @export
#'
#' @examples
#' spec <- tibble(
#'   type = c(
#'     "d",
#'     "d",
#'     "n"
#'   ),
#'   dependent = c(
#'     "StatDiscrep",
#'     "TOTS",
#'     "Import"
#'   ),
#'   independent = c(
#'     "TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#'     "GValueAdd + Import",
#'     "FinConsExpHH + GCapitalForm"
#'   )
#' )
#'
#' fa <- list(geo = "AT", s_adj = "SCA", unit = "CLV05_MEUR")
#' fb <- list(geo = "AT", s_adj = "SCA", unit = "CP_MEUR")
#' filter_list <- list("P7" = fa, "YA0" = fb, "P31_S14_S15" = fa, "P5G" = fa, "B1G" = fa, "P3_S13" = fa, "P6" = fa)
#' \donttest{
#' a <- run_model(specification = spec, dictionary = NULL, inputdata_directory = NULL, filter_list = filter_list, download = TRUE, save_to_disk = NULL, present = FALSE)
#' }

# config_table_small <- tibble(
#   dependent = c("StatDiscrep",
#                 "TOTS",
#                 "Import"),
#   independent = c("TOTS - FinConsExpHH - FinConsExpGov - GCapitalForm - Export",
#                   "GValueAdd + Import",
#                   "FinConsExpHH + GCapitalForm"))
#
#
# specification <- config_table_small

run_model <- function(specification,
                      dictionary = NULL,
                      inputdata_directory = paste0(getwd(), "/data/raw"),
                      filter_list = NULL,
                      download = FALSE,
                      save_to_disk = NULL,
                      present = FALSE,
                      quiet = FALSE,
                      ...) {

  if(!(is.data.frame(specification) | is.matrix(specification))){
    stop("'specification' must be a data.frame, tibble, or matrix object. Check the documentation how a specification object must look like.")
  }

  if(!all(specification$type %in% c("n","d"))){
    stop("The type column in the Specification can only either be 'n' (for Definition i.e. modelled) or 'd' (for identity).")
  }

  if(missing(dictionary) | is.null(dictionary)){dictionary <- aggregate.model::dict}

  if(!all(c("eurostat_code", "model_varname", "full_name", "dataset_id","var_col", "nace_r2") %in% colnames(dictionary))){
    stop("Dictionary does not have all the required columns. Dictionary must have the following column names:\n 'eurostat_code', 'model_varname', 'full_name', 'dataset_id', 'var_col', 'nace_r2'.")
  }

  if(any(duplicated(dictionary$model_varname))){stop("Dictionary cannot contain duplicated values for 'model_varname'.")}

  if(any(grepl("\\-|\\+|\\*|\\/|\\^",dictionary$model_varname))){stop("The 'model_varname' column in the Dictionary cannot contain any of the following characters: + - / * ^")}

  # check whether aggregate model is well-specified
  module_order <- check_config_table(specification)

  # add columns that translate dependent and independent variables into Eurostat codes
  module_order_eurostatvars <- translate_variables(specification = module_order,
                                                   dictionary = dictionary)

  # download or locally load the data necessary for the whole aggregate model
  loaded_data <- load_or_download_variables(specification = module_order_eurostatvars,
                                            filter_list = filter_list,
                                            download = download,
                                            dictionary = dictionary,
                                            inputdata_directory = inputdata_directory,
                                            save_to_disk = save_to_disk,
                                            quiet = quiet,
                                            ...)

  # add data that is not directly available but can be calculated from identities
  full_data <- calculate_identities(specification = module_order_eurostatvars, data = loaded_data, dictionary = dictionary)

  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = module_order_eurostatvars)

  # initialise storage of estimation results
  module_collection <- module_order_eurostatvars %>%
    mutate(dataset = list(NA_complex_),
           model = list(NA_complex_))

  tmp_data <- full_data
  # loop through all modules
  for (i in module_order_eurostatvars$order) {

    # print progress update
    if(!quiet){
      if(i == 1){cat("\n--- Estimation begins ---\n")}
      if(module_order_eurostatvars$type[i] == "n") {cat(paste0("Estimating ", module_order_eurostatvars$dependent[i], " = ", module_order_eurostatvars$independent[i]), "\n")}
      if(module_order_eurostatvars$type[i] == "d") {cat(paste0("Constructing ", module_order_eurostatvars$dependent[i], " = ", module_order_eurostatvars$independent[i]), "\n")}
    }

    # estimate current module, using most up-to-date dataset including predicted values
    module_estimate <- run_module(
      module = module_order_eurostatvars[module_order_eurostatvars$order == i, ],
      data = tmp_data,
      classification = classification,
      ...
    )

    # store module estimates dataset, including fitted values
    module_collection[module_collection$order == i, "dataset"] <- tibble(dataset = list(module_estimate$data))
    module_collection[module_collection$order == i, "model"] <- tibble(dataset = list(module_estimate$model))
    module_collection[module_collection$order == i, "model.args"] <- tibble(dataset = list(module_estimate$args))
    module_collection[module_collection$order == i, "indep"] <- tibble(dataset = list(module_estimate$indep))
    module_collection[module_collection$order == i, "dep"] <- tibble(dataset = list(module_estimate$dep))

    # update dataset for next module by adding fitted values
    tmp_data <- update_data(orig_data = tmp_data, new_data = module_estimate$data)

  }

  # prepare output of aggregate model
  out <- list()
  out$args <- list(specification = specification, dictionary = dictionary,
                   inputdata_directory = inputdata_directory,
                   filter_list = filter_list, download = download,
                   save_to_disk = save_to_disk, present = present)
  out$module_order_eurostatvars <- module_order_eurostatvars
  out$module_collection <- module_collection
  out$full_data <- tmp_data
  out$dictionary <- dictionary

  out <- new_aggmod(out)

  # optionally, present aggregate model output
  if (present) {
    present_model(out)
  }


  return(out)

}
