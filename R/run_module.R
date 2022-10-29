#' Runs a module
#'
#' Runs a module in two different ways depending on whether the module is
#' endogenously modelled or endogenous by identity/definition.
#'
#' @param module A row of the specification table.
#' @param data A tibble or data.frame containing the full data for the aggregate
#'   model.
#' @inheritParams identify_module_data
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

run_module <- function(module, data, classification, ...) {

  raw_data <- identify_module_data(module, classification, data)

  # if is identity/definition equation, run simple summation
  if (module$type == "d") {

    moduledata <- identity_module(module = module,
                                  data = raw_data,
                                  classification = classification)

    out <- list(model = NULL, data = moduledata, args = NULL)

  } else if(module$type == "n") {

    # prepare data (create regressors)
    clean_df <- clean_data(raw_data)

    # extract base variable names (and convert to lower case because janitor::clean_names() does so)
    dep <- module$dependent
    dep <- tolower(dep)
    indep <- strsplits(module$independent, splits = c("\\+", "\\-"))
    indep <- tolower(gsub(" ", "", indep))

    # run estimation
    estimated_module <- estimate_module(clean_data = clean_df,
                                        dep_var_basename = dep,
                                        x_vars_basename = indep, ...)

    moduledata <- add_to_original_data(clean_data = clean_df,
                                       isat_object = estimated_module$best_model,
                                       dep_var_basename = dep,
                                       ardl_or_ecm = estimated_module$args[[5]])

    out <- list(model = estimated_module$best_model,
                data = moduledata,
                args = estimated_module$args)

  } # end "n"

  return(out)

}
