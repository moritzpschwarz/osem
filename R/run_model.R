#' Runs the OSEM model
#'
#' Runs the OSEM model according to the given specification of modules.
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
#' @param primary_source A string. Determines whether \code{"download"} or
#' \code{"local"} data loading takes precedence.
#' @param save_to_disk A path to a directory where the final dataset will be
#'   saved, including the file name and ending. Not saved when \code{NULL}.
#' @param present A logical value whether the final OSEM model output
#'   should be presented or not.
#' @param quiet Logical with default = FALSE. Should messages be displayed?
#' These messages are intended to give more information about the estimation
#' and data retrieval process.
#' @param constrain.to.minimum.sample Logical. Should all data series be
#' constrained to the minimum data series? Default is \code{TRUE}.
#' @param keep Character. A string that will be used as regex (in \code{grepl()})
#' when selection is carried out. This argument therefore requires \code{gets_selection = TRUE}.
#' Variables that match this character will not be selected over (see \code{\link[gets]{getsm}} for details).
#' @param pretest_steps Logical. Default is \code{FALSE}. This argument controls
#' whether isat should first be run for SIS in isolation before other saturation
#' methods are added (IIS, TIS). This can lead to better results if there are many
#' IIS identified at the end of the estimation sample/forecast origin. If \code{TRUE}
#' then \code{\link[gets]{isat}} is first carried out just for SIS (if activated using 'sis = TRUE'),
#' then the SIS breaks are pre-entered to another \code{\link[gets]{isat}} estimation but not selected over.
#' After both isat runs, a union model selection is done using  \code{\link[gets]{gets}}.
#' @inheritParams clean_data
#' @inheritParams estimate_module
#' @param plot Logical with default = TRUE. Should plots be displayed?
#'
#' @return An object of class \link[=new_osem]{osem}, which is a named list
#'   with four elements:
#' \describe{
#'   \item{args}{A named list storing the user arguments for the OSEM
#'   model.}
#'   \item{module_order_eurostatvars}{The original specification with translated
#'   variable names to Eurostat codes and arranged in order of estimation.}
#'   \item{module_collection}{The above specification with two added columns
#'   that store the model object for each module and the dataset used for
#'   estimation, including fitted values for the dependent variable.}
#'   \item{full_data}{A tibble or data.frame containing the complete original
#'   data for the OSEM model and the fitted values of each module.}
#' }
#'
#' @export
#'
#' @examples
#' spec <- dplyr::tibble(
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
#' \donttest{
#' run_model(specification = spec)
#' }


run_model <- function(specification,
                      dictionary = NULL,
                      inputdata_directory = paste0(getwd(), "/data/raw"),
                      primary_source = c("download", "local"),
                      save_to_disk = NULL,

                      use_logs = "both",
                      trend = TRUE,
                      ardl_or_ecm = "ardl",
                      max.ar = 4,
                      max.dl = 4,
                      saturation = c("IIS", "SIS"),
                      saturation.tpval = 0.01,
                      max.block.size = 20,
                      gets_selection = TRUE,
                      selection.tpval = 0.01,
                      constrain.to.minimum.sample = TRUE,
                      keep = NULL,

                      #manual_models = NULL,

                      pretest_steps = FALSE,

                      present = FALSE,
                      quiet = FALSE,
                      plot = TRUE
) {

  primary_source = match.arg(primary_source)

  if(!(is.data.frame(specification) | is.matrix(specification))){
    stop("'specification' must be a data.frame, tibble, or matrix object. Check the documentation how a specification object must look like.")
  }

  if(!all(specification$type %in% c("n","d"))){
    stop("The type column in the Specification can only either be 'n' (for endogenous i.e. modelled) or 'd' (for identity/definition).")
  }

  if(missing(dictionary) | is.null(dictionary)){dictionary <- osem::dict}

  if(!all(c("model_varname", "full_name", "database", "dataset_id","freq") %in% colnames(dictionary))){
    stop("Dictionary does not have all the required columns. Dictionary must have the following column names:\n 'model_varname', 'full_name', 'database', 'dataset_id', 'freq'.")
  }

  if(any(duplicated(dictionary$model_varname))){stop("Dictionary cannot contain duplicated values for 'model_varname'.")}

  if(any(grepl("\\-|\\+|\\*|\\/|\\^",dictionary$model_varname))){stop("The 'model_varname' column in the Dictionary cannot contain any of the following characters: + - / * ^")}
  if(any(grepl("\\-|\\+|\\*|\\/|\\^",specification$dependent))){stop("The 'dependent' column in the specification cannot contain any of the following characters: + - / * ^")}
  if(any(grepl("\\-|\\*|\\/|\\^",specification$independent[specification$type == "n"]))){stop("The 'independent' column of estimated equations (type = 'n') in the specification cannot contain any of the following characters: - / * ^.\nThis is only possible for identities (type = 'd').")}


  if(!is.null(save_to_disk)){
    if(!is.character(save_to_disk)){stop("'save_to_disk' must be a path to a file as a character. For example 'data.csv'.")}
    if(is.character(save_to_disk) & identical(strsplit(basename(save_to_disk), split="\\.")[[1]][-1],character(0))){stop("'save_to_disk' must be a path to a file. No file ending detected. Currently supporting RDS, rds, Rds, csv, xls, xlsx.")}
  }

  # Checking saturation and selection options
  if(!is.logical(gets_selection)){stop("'gets_selection' must be logical  (so either TRUE or FALSE).")}
  if(!is.null(saturation) & !all(saturation %in% c("IIS","SIS","TIS"))){stop("'saturation' must be either NULL to disable Indicator Saturation or a character vector that can take the values 'IIS', 'SIS', or 'TIS'. These can also be combined e.g. c('IIS', 'TIS').")}

  # check the keep argument. it can only be NULL or a character vector
  if(!is.null(keep) & !is.character(keep)){stop("The argument 'keep' must be a character vector.")}
  # raise an error when the keep argument is specified but gets_selection is FALSE
  if(!is.null(keep) & !gets_selection){stop("The argument 'keep' can only be specified when 'gets_selection' is TRUE.")}
  if(!is.null(keep) & length(keep) > 1){stop("The argument 'keep' can only be a character - not a vector.")}

  if(!is.numeric(saturation.tpval) & !is.null(saturation.tpval)){stop("'saturation.tpval' must be either NULL or numeric between 0 and 1.")}
  if(is.numeric(saturation.tpval) & (saturation.tpval > 1 | saturation.tpval < 0 )){stop("'saturation.tpval' must be either NULL or numeric between 0 and 1.")}
  if(!is.numeric(selection.tpval) & !is.null(selection.tpval)){stop("'selection.tpval' must be either NULL or numeric between 0 and 1.")}
  if(is.numeric(selection.tpval) & (selection.tpval > 1 | selection.tpval < 0 )){stop("'selection.tpval' must be either NULL or numeric between 0 and 1.")}

  # Checking log specifications
  if(length(use_logs) > 1 | !is.character(use_logs) | !all(use_logs %in% c("y","x","both","none"))){stop("The argument 'use_logs' must be a character vector and can only be one of 'both', 'y', 'x', or 'none'.")}

  # Checking ardl_or_ecm specification
  #if(length(ardl_or_ecm) > 1 | !is.character(ardl_or_ecm) | !all(use_logs %in% c("y","x","both","none"))){stop("The argument 'use_logs' must be a character vector and can only be one of 'both', 'y', 'x', or 'none'.")}
  if(is.null(ardl_or_ecm) | (!identical(ardl_or_ecm, "ardl") & !identical(ardl_or_ecm, "ecm"))){stop("The argument 'ardl_or_ecm' must be a character vector and can only be one of 'ardl' or 'ecm'.")}

  # # check that manual_models is either a named list or NULL
  # if(!is.null(manual_models) & !is.list(manual_models)){stop("The argument 'manual_models' must be a named list.")}
  # # check that the names of the list are the same as the dependent variables in the specification
  # if(!is.null(manual_models) & !all(names(manual_models) %in% specification$dependent)){stop("The names of the list in 'manual_models' must be the same as the dependent variables in the specification.")}
  # # check that the list contains only model objects of type "arx", "getsm", or "isat"
  # if(!is.null(manual_models) & !all(sapply(manual_models, function(x) class(x) %in% c("arx","getsm","isat")))){stop("The list in 'manual_models' must contain only model objects of class 'arx', 'getsm', or 'isat'.")}


  # start of OSEM model -----------------------------------------------------

  # check whether OSEM model is well-specified
  module_order <- check_config_table(specification)

  # download or locally load the data necessary for the whole OSEM model
  loaded_data <- load_or_download_variables(specification = module_order,
                                            dictionary = dictionary,
                                            primary_source = primary_source,
                                            inputdata_directory = inputdata_directory,
                                            save_to_disk = save_to_disk,
                                            quiet = quiet,
                                            constrain.to.minimum.sample = constrain.to.minimum.sample)

  # add data that is not directly available but can be calculated from identities
  full_data <- calculate_identities(specification = module_order, data = loaded_data, dictionary = dictionary)

  # check the frequencies of the full data and if necessary aggregate
  freq_output <- check_frequencies(full_data, quiet = quiet)
  full_data <- freq_output$full_data
  frequency <- freq_output$frequency


  # check for duplicates in the data
  if(full_data %>%
     dplyr::select("na_item", "time") %>%
     dplyr::filter(duplicated(.)) %>%
     nrow() > 0){

    full_data %>%
      dplyr::select("na_item", "time") %>%
      dplyr::filter(duplicated(.)) %>%
      dplyr::distinct(.data$na_item) %>%
      dplyr::pull("na_item") -> duplicated_variables

    stop(paste0("The data contains duplicates. Please remove them. ",
                "This might be related to an additional filter that is necessary for the eurostat data (e.g. need to select nace_2 for the right value for the sector).\n",
                "Variables that contain duplicates: ",paste0(duplicated_variables, collapse = ", ")))
  }



  # determine classification of variables: exogenous, endogenous by model, endogenous by identity/definition
  classification <- classify_variables(specification = module_order)

  opts_df <- module_order

  # initialise storage of estimation results
  module_collection <- module_order %>%
    dplyr::mutate(dataset = list(NA_complex_),
                  model = list(NA_complex_))

  tmp_data <- full_data
  # loop through all modules
  for (i in module_order$order) {

    # print progress update
    if(!quiet){
      if(i == 1){cat("\n--- Estimation begins ---\n")}

      if (requireNamespace("shiny", quietly = TRUE)) {
        if (shiny::isRunning()) { # send updates to shiny if called from shiny session
          shiny::incProgress(1/length(module_order$order), detail = paste0(paste0("Running step ", i, "/", length(module_order$order), ": ", module_order$dependent[i], " = ", module_order$independent[i])))
        }}


      if(module_order$type[i] == "n") {cat(paste0("Estimating ", module_order$dependent[i], " = ", module_order$independent[i]), "\n")}
      if(module_order$type[i] == "d") {cat(paste0("Constructing ", module_order$dependent[i], " = ", module_order$independent[i]), "\n")}
    }

    # estimate current module, using most up-to-date dataset including predicted values
    module_estimate <- run_module(
      module = module_order[module_order$order == i, ],
      data = tmp_data,
      classification = classification,
      use_logs = use_logs,
      trend = trend,
      ardl_or_ecm = ardl_or_ecm,
      max.ar = max.ar,
      max.dl = max.dl,
      saturation = saturation,
      saturation.tpval = saturation.tpval,
      max.block.size = max.block.size,
      gets_selection = gets_selection,
      selection.tpval = selection.tpval,
      opts_df = opts_df,
      keep = keep,
      pretest_steps = pretest_steps,
      quiet = quiet
    )

    opts_df <- module_estimate$opts_df

    # store module estimates dataset, including fitted values
    module_collection[module_collection$order == i, "dataset"] <- dplyr::tibble(dataset = list(module_estimate$data))
    module_collection[module_collection$order == i, "model"] <- dplyr::tibble(dataset = list(module_estimate$model))
    module_collection[module_collection$order == i, "model.args"] <- dplyr::tibble(dataset = list(module_estimate$args))
    module_collection[module_collection$order == i, "indep"] <- dplyr::tibble(dataset = list(module_estimate$indep))
    module_collection[module_collection$order == i, "dep"] <- dplyr::tibble(dataset = list(module_estimate$dep))
    module_collection[module_collection$order == i, "diagnostics"] <- dplyr::tibble(dataset = list(module_estimate$diagnostics))

    # update dataset for next module by adding fitted values
    tmp_data <- update_data(orig_data = tmp_data, new_data = module_estimate$data)

  }

  # prepare output of OSEM model
  out <- list()
  out$args <- list(specification = specification, dictionary = dictionary,
                   inputdata_directory = inputdata_directory,
                   primary_source = primary_source,
                   save_to_disk = save_to_disk, present = present,

                   trend = trend, max.ar = max.ar, max.dl = max.dl, use_logs = use_logs,
                   ardl_or_ecm = ardl_or_ecm,
                   saturation = saturation,
                   saturation.tpval = saturation.tpval,
                   max.block.size = max.block.size,
                   gets_selection = gets_selection,
                   selection.tpval = selection.tpval,
                   constrain.to.minimum.sample = constrain.to.minimum.sample,
                   pretest_steps = pretest_steps)

  out$module_order <- module_order
  out$module_collection <- module_collection
  out$processed_input_data <- full_data
  out$full_data <- tmp_data
  out$dictionary <- dictionary
  out$opts_df <- opts_df

  out <- new_osem(out)

  # optionally, present OSEM model output
  if (present) {
    present_model(out)
  }

  if(plot){

    plot(out)
  }

  return(out)

}
