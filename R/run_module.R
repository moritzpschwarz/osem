#' Runs a module
#'
#' Runs a module in two different ways depending on whether the module is
#' endogenously modelled or endogenous by identity/definition.
#'
#' @param module A row of the specification table.
#' @param data A tibble or data.frame containing the full data for the aggregate
#'   model.
#' @param use_logs Character vector. Either "both", "x", or "y" to decide whether to use logs.
#' @param ... Further arguments to be passed to 'estimate_module'
#' @inheritParams identify_module_data
#' @inheritParams clean_data
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
#' @export

run_module <- function(module, data, classification, use_logs = c("both","y","x"), max.lag = 4, trend = TRUE, ...) {

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
    clean_df <- clean_data(raw_data = raw_data, max.lag = max.lag, trend = trend)

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
                                        ...)

    moduledata <- add_to_original_data(clean_data = clean_df,
                                       isat_object = estimated_module$best_model,
                                       dep_var_basename = dep,
                                       ardl_or_ecm = estimated_module$args[[5]])

    out <- list(model = estimated_module$best_model,
                data = moduledata,
                args = estimated_module$args,
                indep = gsub(" ", "", strsplits(module$independent, splits = c("\\+", "\\-"))),
                dep = module$dependent)

  } # end "n"

  return(out)

}
