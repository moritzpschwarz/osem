#' Runs a module
#'
#' Runs a module in two different ways depending on whether the module is
#' endogenously modelled or endogenous by identity/definition.
#'
#' @param module A row of the specification table.
#' @param data A tibble or data.frame containing the full data for the OSEM
#'   model.
#' @inheritParams identify_module_data
#' @inheritParams clean_data
#' @inheritParams estimate_module
#'
#'
#' @return Returns a list with two named elements.
#' \describe{
#'   \item{model}{For endogenously modeled modules (\code{type == "n"}), an
#'   object of class \code{\link[gets:isat]{isat}} or
#'   \code{\link[gets:arx]{arx}}. For identity/definition modules
#'   (\code{type == "d"}), \code{NULL}.}
#'   \item{data}{The data (including transformations) that was used to estimate
#'   the model and the fitted values of the dependent variable.}
#' }
#'

run_module <- function(
    module,
    data,
    classification,
    use_logs = c("both", "y", "x"),
    trend = TRUE,
    ardl_or_ecm = "ardl",
    max.ar = 4,
    max.dl = 2,
    saturation = c("IIS", "SIS"),
    saturation.tpval = 0.01,
    max.block.size = 20,
    gets_selection = TRUE,
    selection.tpval = 0.01,
    quiet) {

  raw_data <- identify_module_data(module, classification, data)

  # if is identity/definition equation, run simple summation
  if (module$type == "d") {

    moduledata <- identity_module(module = module,
                                  data = raw_data,
                                  classification = classification)

    out <- list(model = NULL,
                data = moduledata,
                args = NULL)#
    #list(use_logs = match.arg(use_logs)))

  } else if(module$type == "n") {

    # prepare data (create regressors)
    clean_df <- clean_data(raw_data = raw_data, max.ar = max.ar, max.dl = max.dl, trend = trend)

    # extract base variable names (and convert to lower case because janitor::clean_names() does so)
    dep <- module$dependent
    #dep <- tolower(dep)
    indep <- strsplits(module$independent, splits = c("\\+", "\\-"))
    indep <- gsub(" ", "", indep)
    #indep <- tolower(gsub(" ", "", indep))

    # run estimation
    estimated_module <- estimate_module(clean_data = clean_df,
                                        dep_var_basename = dep,
                                        x_vars_basename = indep,
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
                                        quiet = quiet)

    moduledata <- add_to_original_data(clean_data = clean_df,
                                       isat_object = estimated_module$best_model,
                                       dep_var_basename = dep,
                                       ardl_or_ecm = estimated_module$args$ardl_or_ecm)

    out <- list(model = estimated_module$best_model,
                data = moduledata,
                args = estimated_module$args,
                indep = gsub(" ", "", strsplits(module$independent, splits = c("\\+", "\\-"))),
                dep = module$dependent,
                diagnostics = list(super.exogeneity = estimated_module$superex_test))

  } # end "n"

  return(out)

}
