#' Determine which model variables need to be found
#'
#' Takes the specification and returns a dictionary-like data frame with the
#' required model variables, their filters, and where they can be found.
#'
#' @inheritParams load_or_download_variables
#'
#' @return Returns a data.frame that is a subset of the dictionary (required
#' variables only) and an additional column \code{found}, which is set to
#' \code{FALSE} initially and keeps track of the data that has been found
#' through the (down)load functions.

determine_variables <- function(specification, dictionary) {
  # extract the codes used in the model
  # cvar rows contain multiple dependent variable entries
  spec_cvar <- specification %>%
    dplyr::filter(.data$cvar != "")
  spec_other <- specification %>%
    dplyr::filter(.data$cvar == "")
  dep.set <- spec_other$dependent
  for (i in seq_along(spec_cvar$dependent)) {
    vars <- trimws(unlist(strsplit(spec_cvar$dependent[i], ",")))
    dep.set <- union(dep.set, vars)
  }

  # regressors
  indep <- specification$independent
  indep.set <- NULL

  for (i in seq_along(indep)) {
    # each element of character vector may contain formula, e.g. "A + B", must split
    vars <- strsplits(
      indep[i],
      c(" \\- ", " \\+ ", " / ", " \\* ")
    )
    indep.set <- union(indep.set, vars)
  }
  indep.set <- gsub(" ", "", indep.set)
  codes.used <- union(dep.set, indep.set)

  # some model variable might be aggregate level vars (e.g. TOTS)
  # they are constructed by definition, so need to exclude them
  # exclusion by is.na(database)

  to_obtain <- dictionary %>%
    dplyr::filter(.data$model_varname %in% codes.used) %>%
    dplyr::filter(!is.na(.data$database)) %>%
    dplyr::mutate(found = FALSE) # will keep track of whether found (download or local)

  code.check <- codes.used
  idents <- specification %>%
    dplyr::filter(.data$type == "d") %>%
    dplyr::pull(.data$dependent)
  code.check <- code.check[!code.check %in% idents]

  if (!all(code.check %in% dictionary$model_varname)) {
    stop(paste0(
      "Not all model variables found in the dictionary.", "\n",
      "Missing variables: ", paste(setdiff(code.check, to_obtain$model_varname), collapse = ", ")
    ))
  }

  return(to_obtain)
}
