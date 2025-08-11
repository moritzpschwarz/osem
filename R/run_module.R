#' Runs a module
#'
#' Runs a module in two different ways depending on whether the module is
#' endogenously modelled or endogenous by identity/definition.
#'
#' @param module A row of the specification table.
#' @param data A tibble or data.frame containing the full data for the OSEM
#'   model.
#' @param opts_df Internal object containing detailed options and information on individual modules.
#' @inheritParams identify_module_data
#' @inheritParams clean_data
#' @inheritParams estimate_module
#' @inheritParams estimate_cvar
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
    opts_df,
    keep,
    pretest_steps,
    quiet,
    cvar.ar = 4,
    freq = NULL,
    coint_deterministic = "const",
    coint_significance = "5pct") {
  raw_data <- identify_module_data(module, classification, data)

  # if is identity/definition equation, run simple parse
  if (module$type == "d") {
    moduledata <- identity_module(
      module = module,
      data = raw_data,
      classification = classification
    )
    out <- list(
      model = NULL,
      data = moduledata,
      args = NULL,
      opts_df = opts_df
    )
    # endogenous, single equation module
  } else if (module$type == "n" & identical(module$cvar, "")) {
    # prepare data (create regressors)
    clean_data_output <- clean_data(
      raw_data = raw_data, max.ar = max.ar, max.dl = max.dl, trend = trend,
      opts_df = opts_df,
      module = module,
      use_logs = use_logs
    )
    clean_df <- clean_data_output$df
    opts_df <- clean_data_output$opts_df

    # can delete contemporaneous values for variables that only enter as lags
    # TODO: in future should probably do this in clean_data, need to add an argument to it
    if (module$lag != "") {
      lag_only_vars <- trimws(unlist(strsplit(module$lag, ",")))
      if (use_logs %in% c("both", "x")) {
        lag_only_vars <- paste0("ln.", lag_only_vars)
      }
      for (i in seq_along(lag_only_vars)) {
        clean_df <- clean_df %>%
          # remove variables that are x variables but not lagged
          dplyr::select(!(dplyr::contains(lag_only_vars[i]) & !dplyr::matches("^L\\d+\\.")))
      }
    }

    # extract base variable names (and convert to lower case because janitor::clean_names() does so)
    dep <- module$dependent
    # dep <- tolower(dep)
    indep <- strsplits(module$independent, splits = c("\\+", "\\-"))
    indep <- gsub(" ", "", indep)
    # indep <- tolower(gsub(" ", "", indep))

    # run estimation
    estimated_module <- estimate_module(
      clean_data = clean_df,
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
      keep = keep,
      pretest_steps = pretest_steps,
      quiet = quiet,
      module = module
    )

    moduledata <- add_to_original_data(
      clean_data = clean_df,
      model_object = estimated_module$best_model,
      dep_var_basename = dep,
      model_type = estimated_module$args$ardl_or_ecm,
      opts_df = opts_df,
      module = module
    )

    out <- list(
      model = estimated_module$best_model,
      data = moduledata,
      args = estimated_module$args,
      indep = gsub(" ", "", strsplits(module$independent, splits = c("\\+", "\\-"))),
      dep = module$dependent,
      diagnostics = list(super.exogeneity = estimated_module$superex_test),
      opts_df = opts_df
    )
    # endogenous, cvar module
  } else if (module$type == "n" & !identical(module$cvar, "")) {
    # prepare data (create regressors)
    clean_data_output <- clean_data(
      raw_data = raw_data, max.ar = max.ar, max.dl = max.dl, trend = trend,
      opts_df = opts_df,
      module = module,
      use_logs = use_logs
    )
    clean_df <- clean_data_output$df
    opts_df <- clean_data_output$opts_df

    # extract base variable names
    dep <- trimws(unlist(strsplit(module$dependent, ",")))
    indep <- trimws(unlist(strsplit(module$independent, "[+-]")))

    # run estimation
    estimated_cvar <- estimate_cvar(
      clean_data = clean_df,
      system_name = module$cvar,
      dep_vars_basename = module$dependent,
      x_vars_basename = indep,
      use_logs = use_logs,
      cvar.ar = cvar.ar,
      freq = freq,
      coint_deterministic = coint_deterministic,
      coint_significance = coint_significance,
      quiet = quiet
    )

    moduledata <- add_to_original_data(
      clean_data = clean_df,
      model_object = estimated_cvar,
      dep_var_basename = dep,
      model_type = "cvar",
      opts_df = opts_df,
      module = module
    )

    out <- list(
      model = estimated_cvar,
      data = moduledata,
      args = estimated_cvar$args,
      indep = indep,
      dep = dep,
      diagnostics = list(super.exogeneity = NA), # set always NA but match output structure as for ardl/ecm
      opts_df = opts_df
    )
  }
  return(out)
}
